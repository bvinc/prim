//! HIR → wasm32 + WASI code generation.
//!
//! Single public entry point: [`generate_wasm`]. The work is split across
//! sibling modules:
//!
//! - [`types`] — HIR type ↔ wasm `ValType` mapping; function-type registry.
//! - [`layout`] — static memory layout constants; struct layout computation;
//!   field load/store helpers.
//! - [`walks`] — pre-walks over HIR (locals, scratch types, dbg prefixes,
//!   string literals).
//! - [`builtins`] — hand-written wasm bodies for `__println_*`, `__alloc`,
//!   `__print_bytes`, `_start`.
//! - [`emit`] — per-function emission of user code.

mod builtins;
mod emit;
mod layout;
mod types;
mod walks;

use crate::builtins::{
    Builtins, emit_alloc, emit_print_bytes, emit_println_bool, emit_println_f64, emit_println_i64,
    emit_println_u64, emit_start,
};
use crate::emit::{DbgSite, StrSite, build_emit_ctx, emit_user_function};
use crate::layout::{
    DOT_OFFSET, FALSE_OFFSET, NEWLINE_OFFSET, STATIC_DATA_START, StructLayout, TRUE_OFFSET,
    compute_struct_layout,
};
use crate::types::{TypeRegistry, hir_type_to_valtype};
use crate::walks::{collect_dbg_prefixes_block, collect_str_literals_block};
use prim_compiler::hir;
use std::collections::HashMap;
use std::fmt;
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ExportKind, ExportSection, FunctionSection, GlobalSection,
    GlobalType, ImportSection, MemorySection, MemoryType, Module, ValType,
};

#[derive(Debug)]
pub enum WasmError {
    MissingMain,
}

impl fmt::Display for WasmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasmError::MissingMain => write!(f, "main function not found"),
        }
    }
}

impl std::error::Error for WasmError {}

/// Compile a typechecked HIR program into a wasm32+WASI module.
pub fn generate_wasm(program: &hir::Program) -> Result<Vec<u8>, WasmError> {
    if program.main.is_none() {
        return Err(WasmError::MissingMain);
    }

    // Compute memory layout for every struct.
    let mut struct_layouts: HashMap<hir::StructId, StructLayout> = HashMap::new();
    for s in &program.structs {
        struct_layouts.insert(s.id, compute_struct_layout(s));
    }

    let mut types = TypeRegistry::new();

    // Register function types for the builtin runtime helpers.
    let fd_write_type = types.register(
        vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        vec![ValType::I32],
    );
    let println_i64_type = types.register(vec![ValType::I64], vec![]);
    let println_bool_type = types.register(vec![ValType::I32], vec![]);
    let println_f64_type = types.register(vec![ValType::F64], vec![]);
    let alloc_type = types.register(vec![ValType::I32], vec![ValType::I32]);
    let print_bytes_type = types.register(vec![ValType::I32, ValType::I32], vec![]);

    // Function index layout:
    //   0: fd_write (import)
    //   1: __println_i64
    //   2: __println_u64
    //   3: __println_bool
    //   4: __println_f64
    //   5: __alloc
    //   6: __print_bytes
    //   7+: user functions
    //   last: _start
    let fd_write_idx: u32 = 0;
    let builtins = Builtins {
        println_i64: 1,
        println_u64: 2,
        println_bool: 3,
        println_f64: 4,
        alloc: 5,
        print_bytes: 6,
    };

    // Build func_map (user functions) and runtime_map (runtime-bound functions).
    let mut func_map: HashMap<hir::FuncId, u32> = HashMap::new();
    let mut runtime_map: HashMap<hir::FuncId, String> = HashMap::new();
    let mut user_func_types: Vec<u32> = Vec::new();
    let mut next_idx: u32 = 7;
    let mut main_wasm_idx = None;

    for func in &program.functions {
        if let Some(binding) = &func.runtime_binding {
            runtime_map.insert(func.id, binding.clone());
        } else {
            func_map.insert(func.id, next_idx);
            if program.main == Some(func.name) {
                main_wasm_idx = Some(next_idx);
            }
            let params: Vec<ValType> = func
                .params
                .iter()
                .map(|p| hir_type_to_valtype(&p.ty))
                .collect();
            let results: Vec<ValType> = func
                .ret
                .as_ref()
                .map(|t| vec![hir_type_to_valtype(t)])
                .unwrap_or_default();
            let type_idx = types.register(params, results);
            user_func_types.push(type_idx);
            next_idx += 1;
        }
    }

    let start_idx = next_idx;
    let start_type = types.register(vec![], vec![]);
    let main_wasm_idx = main_wasm_idx.ok_or(WasmError::MissingMain)?;

    // First pass: walk every user function in the same order they'll be
    // emitted, collect dbg prefix strings AND string literal bytes, lay them
    // out in static memory starting at STATIC_DATA_START. Record per-function
    // slice ranges so each function's EmitCtx can index into the global
    // tables by per-function counter.
    let mut dbg_sites: Vec<DbgSite> = Vec::new();
    let mut str_sites: Vec<StrSite> = Vec::new();
    let mut static_data: Vec<u8> = Vec::new();
    let mut per_func_dbg_range: HashMap<hir::FuncId, std::ops::Range<usize>> = HashMap::new();
    let mut per_func_str_range: HashMap<hir::FuncId, std::ops::Range<usize>> = HashMap::new();
    let mut cursor: u32 = STATIC_DATA_START;
    for func in &program.functions {
        if func.runtime_binding.is_some() {
            continue;
        }
        let dbg_start = dbg_sites.len();
        let mut prefixes: Vec<&str> = Vec::new();
        collect_dbg_prefixes_block(&func.body, &mut prefixes);
        for prefix in prefixes {
            let bytes = prefix.as_bytes();
            let len = bytes.len() as u32;
            static_data.extend_from_slice(bytes);
            dbg_sites.push(DbgSite { ptr: cursor, len });
            cursor += len;
        }
        per_func_dbg_range.insert(func.id, dbg_start..dbg_sites.len());

        let str_start = str_sites.len();
        let mut literals: Vec<&str> = Vec::new();
        collect_str_literals_block(&func.body, &mut literals);
        for s in literals {
            let bytes = s.as_bytes();
            let len = bytes.len() as u32;
            static_data.extend_from_slice(bytes);
            str_sites.push(StrSite { ptr: cursor, len });
            cursor += len;
        }
        per_func_str_range.insert(func.id, str_start..str_sites.len());
    }
    let heap_start: i32 = ((cursor + 7) & !7).max(1024) as i32;

    let mut module = Module::new();

    // Type section
    module.section(&types.build_section());

    // Import section
    let mut imports = ImportSection::new();
    imports.import(
        "wasi_snapshot_preview1",
        "fd_write",
        wasm_encoder::EntityType::Function(fd_write_type),
    );
    module.section(&imports);

    // Function section
    let mut functions = FunctionSection::new();
    functions.function(println_i64_type); // __println_i64
    functions.function(println_i64_type); // __println_u64
    functions.function(println_bool_type); // __println_bool
    functions.function(println_f64_type); // __println_f64
    functions.function(alloc_type); // __alloc
    functions.function(print_bytes_type); // __print_bytes
    for &type_idx in &user_func_types {
        functions.function(type_idx);
    }
    functions.function(start_type); // _start
    module.section(&functions);

    // Memory section
    let mut memories = MemorySection::new();
    memories.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);

    // Global section: heap pointer
    let mut globals = GlobalSection::new();
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(heap_start),
    );
    module.section(&globals);

    // Export section
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, start_idx);
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);

    // Code section
    let mut codes = CodeSection::new();
    codes.function(&emit_println_i64(fd_write_idx));
    codes.function(&emit_println_u64(fd_write_idx));
    codes.function(&emit_println_bool(fd_write_idx));
    codes.function(&emit_println_f64(fd_write_idx));
    codes.function(&emit_alloc());
    codes.function(&emit_print_bytes(fd_write_idx));
    for func in &program.functions {
        if func.runtime_binding.is_none() {
            let dbg_range = per_func_dbg_range.get(&func.id).cloned().unwrap_or(0..0);
            let str_range = per_func_str_range.get(&func.id).cloned().unwrap_or(0..0);
            let dbg_slice = &dbg_sites[dbg_range];
            let str_slice = &str_sites[str_range];
            let ctx = build_emit_ctx(
                program,
                func,
                &func_map,
                &runtime_map,
                &builtins,
                &struct_layouts,
                dbg_slice,
                str_slice,
            );
            codes.function(&emit_user_function(func, &ctx)?);
        }
    }
    let main_func = program
        .functions
        .iter()
        .find(|f| program.main == Some(f.name))
        .unwrap();
    codes.function(&emit_start(main_wasm_idx, main_func.ret.is_some()));
    module.section(&codes);

    // Data section
    let mut data = DataSection::new();
    data.active(
        0,
        &ConstExpr::i32_const(NEWLINE_OFFSET),
        b"\n".iter().copied(),
    );
    data.active(
        0,
        &ConstExpr::i32_const(TRUE_OFFSET),
        b"true".iter().copied(),
    );
    data.active(
        0,
        &ConstExpr::i32_const(FALSE_OFFSET),
        b"false".iter().copied(),
    );
    data.active(0, &ConstExpr::i32_const(DOT_OFFSET), b".".iter().copied());
    if !static_data.is_empty() {
        data.active(
            0,
            &ConstExpr::i32_const(STATIC_DATA_START as i32),
            static_data.iter().copied(),
        );
    }
    module.section(&data);

    Ok(module.finish())
}
