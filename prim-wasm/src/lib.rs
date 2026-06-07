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
use crate::emit::{DbgSite, StrSite, StringLayout, build_emit_ctx, emit_user_function};
use crate::layout::{
    DOT_OFFSET, EnumLayout, FALSE_OFFSET, NEWLINE_OFFSET, STATIC_DATA_START, StructLayout,
    TRUE_OFFSET, compute_enum_layout, compute_struct_layout,
};
use crate::types::{TypeRegistry, hir_type_to_valtype};
use crate::walks::{collect_dbg_prefixes_block, collect_str_literals_block};
use prim_compiler::hir;
use std::collections::HashMap;
use std::fmt;
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ElementSection, Elements, ExportKind, ExportSection,
    FunctionSection, GlobalSection, GlobalType, ImportSection, MemorySection, MemoryType, Module,
    RefType, TableSection, TableType, TagKind, TagSection, TagType, ValType,
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
    let mut enum_layouts: HashMap<hir::EnumId, EnumLayout> = HashMap::new();
    for e in &program.enums {
        enum_layouts.insert(e.id, compute_enum_layout(e));
    }
    let mut string_layout = None;
    for s in &program.structs {
        let Some(layout) = struct_layouts.get(&s.id) else {
            continue;
        };
        let Some(symbol) = program.symbols.get(s.name.0 as usize) else {
            continue;
        };
        let module_name = &program.modules[symbol.module.0 as usize].name;
        if module_name.len() != 2 || module_name[0] != "std" || module_name[1] != "string" {
            continue;
        }
        if program.interner.resolve(&symbol.name) != "String" {
            continue;
        }
        let mut data_offset = None;
        let mut len_offset = None;
        let mut cap_offset = None;
        for field in &s.fields {
            let Some((offset, _)) = layout.fields.get(&field.name) else {
                continue;
            };
            match program.interner.resolve(&field.name) {
                "data" => data_offset = Some(*offset),
                "len" => len_offset = Some(*offset),
                "cap" => cap_offset = Some(*offset),
                _ => {}
            }
        }
        if let (Some(data_offset), Some(len_offset), Some(cap_offset)) =
            (data_offset, len_offset, cap_offset)
        {
            string_layout = Some(StringLayout {
                struct_id: s.id,
                size: layout.size,
                data_offset,
                len_offset,
                cap_offset,
            });
        }
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
        yield_tag: 0,
    };

    // Build func_map (user functions) and runtime_map (runtime-bound functions).
    let mut func_map: HashMap<hir::FuncId, u32> = HashMap::new();
    let mut runtime_map: HashMap<hir::FuncId, hir::RuntimeAbi> = HashMap::new();
    let mut user_func_types: Vec<u32> = Vec::new();
    let mut next_idx: u32 = 7;
    let mut main_wasm_idx = None;
    let mut main_func_type: Option<u32> = None;

    for func in &program.functions {
        // Uninstantiated generic templates are never called (only their
        // monomorphized clones are); they get no wasm function.
        if !func.type_params.is_empty() {
            continue;
        }
        if let Some(runtime) = func.runtime {
            runtime_map.insert(func.id, runtime);
        } else {
            func_map.insert(func.id, next_idx);
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
            if program.main == Some(func.name) {
                main_wasm_idx = Some(next_idx);
                main_func_type = Some(type_idx);
            }
            next_idx += 1;
        }
    }

    let start_idx = next_idx;
    let start_type = types.register(vec![], vec![]);
    let main_wasm_idx = main_wasm_idx.ok_or(WasmError::MissingMain)?;
    let main_func_type = main_func_type.ok_or(WasmError::MissingMain)?;

    // WasmFX: continuation type wrapping main's function signature; the
    // scheduler in `_start` uses this for `cont.new` and `resume`. The
    // `yield` tag has the empty function-type signature so suspend/resume
    // carry no values — yield is "I want to reschedule," nothing else.
    let main_cont_type = types.register_cont(main_func_type);
    let yield_tag_idx: u32 = 0;

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
        if func.runtime.is_some() || !func.type_params.is_empty() {
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

    // Trait dispatch: every impl method that may be invoked through a
    // trait fat pointer gets a stable slot in wasm table 0. The vtables
    // (one per (TraitId, StructId) impl) live in static memory and store
    // the table slot index for each method in trait declaration order.
    //
    // method_table_idx: FuncId -> wasm table slot
    // table_entries:    wasm function indices in slot order (for the
    //                   active element segment)
    // vtable_addr:      (TraitId, StructId) -> static-memory address
    let mut impl_keys: Vec<(hir::TraitId, hir::StructId)> = program.impls.keys().copied().collect();
    impl_keys.sort_by_key(|(t, s)| (t.0, s.0));

    let mut method_table_idx: HashMap<hir::FuncId, u32> = HashMap::new();
    let mut table_entries: Vec<u32> = Vec::new();
    for key in &impl_keys {
        for fid in &program.impls[key] {
            if fid.0 == u32::MAX || method_table_idx.contains_key(fid) {
                continue;
            }
            let wasm_fn = *func_map.get(fid).expect("impl method missing in func_map");
            let slot = table_entries.len() as u32;
            method_table_idx.insert(*fid, slot);
            table_entries.push(wasm_fn);
        }
    }

    // Lay out vtables in static memory, 4 bytes per slot. Each slot holds
    // a wasm table index (i32). Pad static_data once up to the aligned
    // cursor, then append vtable bytes contiguously.
    cursor = (cursor + 3) & !3;
    let pad_to = (cursor - STATIC_DATA_START) as usize;
    if static_data.len() < pad_to {
        static_data.resize(pad_to, 0);
    }
    let mut vtable_addr: HashMap<(hir::TraitId, hir::StructId), u32> = HashMap::new();
    for key in &impl_keys {
        vtable_addr.insert(*key, cursor);
        for fid in &program.impls[key] {
            let slot = if fid.0 == u32::MAX {
                // Missing impl method — sentinel slot 0 traps via the
                // null-funcref check (or wrong-signature trap) on dispatch.
                0u32
            } else {
                *method_table_idx.get(fid).expect("missing table slot")
            };
            static_data.extend_from_slice(&slot.to_le_bytes());
            cursor += 4;
        }
    }

    // Register a wasm type-index for each trait method's signature so
    // call_indirect at dispatch sites can reference it. The dispatched
    // function's wasm signature is (i32 receiver-data-ptr, ...remaining
    // params) → return type — uniform across all impls of a given trait
    // method since pointer types all lower to i32.
    let mut dyn_call_types: HashMap<(hir::TraitId, u32), u32> = HashMap::new();
    for t in &program.traits {
        for (mi, sig) in t.methods.iter().enumerate() {
            let mut params: Vec<ValType> = Vec::with_capacity(sig.params.len().max(1));
            if sig.params.is_empty() {
                params.push(ValType::I32);
            } else {
                params.push(ValType::I32);
                for p in &sig.params[1..] {
                    params.push(hir_type_to_valtype(p));
                }
            }
            let results: Vec<ValType> = sig
                .ret
                .as_ref()
                .map(|r| vec![hir_type_to_valtype(r)])
                .unwrap_or_default();
            let type_idx = types.register(params, results);
            dyn_call_types.insert((t.id, mi as u32), type_idx);
        }
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

    // Table section: a funcref table holding every impl method that may
    // be invoked through a trait fat pointer. Table 0 is the dispatch
    // table; if there are no impls, we still emit an empty table so a
    // call_indirect against table 0 is always well-formed.
    let mut tables = TableSection::new();
    let table_size = table_entries.len() as u64;
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        minimum: table_size,
        maximum: Some(table_size),
        table64: false,
        shared: false,
    });
    module.section(&tables);

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

    // Tag section: declare the `yield` tag used by the scheduler's resume
    // handler. Stack-switching tags use the same binary encoding as
    // exception tags (attribute byte 0); `TagKind::Exception` is just the
    // name wasm-encoder gives that encoding.
    let mut tags = TagSection::new();
    tags.tag(TagType {
        kind: TagKind::Exception,
        func_type_idx: start_type,
    });
    module.section(&tags);

    // Global section: heap pointer first (wasm index 0), then user globals.
    let mut globals = GlobalSection::new();
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(heap_start),
    );
    let mut global_wasm_idx: HashMap<hir::GlobalId, u32> = HashMap::new();
    for (i, g) in program.globals.iter().enumerate() {
        let val_type = hir_type_to_valtype(&g.ty);
        let init = match g.init {
            hir::GlobalInit::I32(v) => ConstExpr::i32_const(v),
            hir::GlobalInit::I64(v) => ConstExpr::i64_const(v),
            hir::GlobalInit::F32(v) => ConstExpr::f32_const(v.into()),
            hir::GlobalInit::F64(v) => ConstExpr::f64_const(v.into()),
        };
        globals.global(
            GlobalType {
                val_type,
                mutable: g.mutable,
                shared: false,
            },
            &init,
        );
        // wasm global index: heap_ptr is at 0, user globals start at 1.
        global_wasm_idx.insert(g.id, 1 + i as u32);
    }
    module.section(&globals);

    // Export section
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, start_idx);
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);

    // Element section: an active segment populates the dispatch table at
    // offset 0 with each impl method's wasm function index, plus a
    // declared segment for `ref.func $main` (the scheduler in `_start`
    // uses ref.func, which requires the target to be declared ref-able).
    let mut elements = ElementSection::new();
    if !table_entries.is_empty() {
        elements.active(
            Some(0),
            &ConstExpr::i32_const(0),
            Elements::Functions(table_entries.clone().into()),
        );
    }
    elements.declared(Elements::Functions(vec![main_wasm_idx].into()));
    module.section(&elements);

    // Code section
    let mut codes = CodeSection::new();
    codes.function(&emit_println_i64(fd_write_idx));
    codes.function(&emit_println_u64(fd_write_idx));
    codes.function(&emit_println_bool(fd_write_idx));
    codes.function(&emit_println_f64(fd_write_idx));
    codes.function(&emit_alloc());
    codes.function(&emit_print_bytes(fd_write_idx));
    for func in &program.functions {
        if func.runtime.is_none() && func.type_params.is_empty() {
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
                &enum_layouts,
                string_layout,
                &global_wasm_idx,
                &dyn_call_types,
                &vtable_addr,
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
    codes.function(&emit_start(
        main_wasm_idx,
        main_cont_type,
        yield_tag_idx,
        main_func.ret.is_some(),
    ));
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
