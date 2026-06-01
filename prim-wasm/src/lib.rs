use prim_compiler::hir;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, MemArg, MemorySection,
    MemoryType, Module, TypeSection, ValType,
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

// Static memory layout:
// [0..4)      iovec.buf
// [4..8)      iovec.buf_len
// [8..12)     nwritten
// [12..13)    '\n'
// [13..34)    digit scratch (21 bytes, enough for i64::MIN)
// [34..38)    "true"
// [38..43)    "false"
// [43..44)    '.'
// [48..128)   float fractional digit scratch
// [1024..)    bump heap
const NEWLINE_OFFSET: i32 = 12;
const DIGIT_BUF_END: i32 = 34;
const TRUE_OFFSET: i32 = 34;
const FALSE_OFFSET: i32 = 38;
const DOT_OFFSET: i32 = 43;
const FLOAT_SCRATCH: i32 = 48;
const HEAP_PTR_GLOBAL: u32 = 0;

const MEM8: MemArg = MemArg {
    offset: 0,
    align: 0,
    memory_index: 0,
};
const MEM32: MemArg = MemArg {
    offset: 0,
    align: 2,
    memory_index: 0,
};

fn hir_type_to_valtype(ty: &hir::Type) -> ValType {
    match ty {
        hir::Type::Bool
        | hir::Type::U8
        | hir::Type::I8
        | hir::Type::U16
        | hir::Type::I16
        | hir::Type::U32
        | hir::Type::I32
        | hir::Type::IntVar => ValType::I32,
        hir::Type::U64 | hir::Type::I64 => ValType::I64,
        hir::Type::Usize | hir::Type::Isize => ValType::I32,
        hir::Type::F32 => ValType::F32,
        hir::Type::F64 | hir::Type::FloatVar => ValType::F64,
        hir::Type::Pointer { .. } | hir::Type::Struct(_) => ValType::I32,
        _ => ValType::I32,
    }
}

fn is_signed_int(ty: &hir::Type) -> bool {
    matches!(
        ty,
        hir::Type::I8
            | hir::Type::I16
            | hir::Type::I32
            | hir::Type::I64
            | hir::Type::Isize
            | hir::Type::IntVar
    )
}

fn produces_value(ty: &hir::Type) -> bool {
    !matches!(ty, hir::Type::Undetermined)
}

// --- Struct layout ---

fn field_size(ty: &hir::Type) -> u32 {
    match ty {
        hir::Type::Bool | hir::Type::I8 | hir::Type::U8 => 1,
        hir::Type::I16 | hir::Type::U16 => 2,
        hir::Type::I64 | hir::Type::U64 | hir::Type::F64 | hir::Type::FloatVar => 8,
        _ => 4,
    }
}

fn align_up(offset: u32, align: u32) -> u32 {
    (offset + align - 1) & !(align - 1)
}

#[derive(Clone)]
struct StructLayout {
    size: u32,
    fields: HashMap<hir::InternSymbol, (u32, hir::Type)>,
}

fn compute_struct_layout(s: &hir::Struct) -> StructLayout {
    let mut offset = 0u32;
    let mut fields = HashMap::new();
    for f in &s.fields {
        let size = field_size(&f.ty);
        offset = align_up(offset, size);
        fields.insert(f.name, (offset, f.ty.clone()));
        offset += size;
    }
    let size = align_up(offset.max(1), 8);
    StructLayout { size, fields }
}

fn mem_arg(offset: u32, ty: &hir::Type) -> MemArg {
    let align = match field_size(ty) {
        1 => 0,
        2 => 1,
        4 => 2,
        _ => 3,
    };
    MemArg {
        offset: offset as u64,
        align,
        memory_index: 0,
    }
}

fn emit_field_store(f: &mut Function, ty: &hir::Type, offset: u32) {
    let arg = mem_arg(offset, ty);
    match ty {
        hir::Type::Bool | hir::Type::I8 | hir::Type::U8 => {
            f.instruction(&Instruction::I32Store8(arg));
        }
        hir::Type::I16 | hir::Type::U16 => {
            f.instruction(&Instruction::I32Store16(arg));
        }
        hir::Type::I64 | hir::Type::U64 => {
            f.instruction(&Instruction::I64Store(arg));
        }
        hir::Type::F32 => {
            f.instruction(&Instruction::F32Store(arg));
        }
        hir::Type::F64 => {
            f.instruction(&Instruction::F64Store(arg));
        }
        _ => {
            f.instruction(&Instruction::I32Store(arg));
        }
    };
}

fn emit_field_load(f: &mut Function, ty: &hir::Type, offset: u32) {
    let arg = mem_arg(offset, ty);
    match ty {
        hir::Type::Bool | hir::Type::U8 => {
            f.instruction(&Instruction::I32Load8U(arg));
        }
        hir::Type::I8 => {
            f.instruction(&Instruction::I32Load8S(arg));
        }
        hir::Type::U16 => {
            f.instruction(&Instruction::I32Load16U(arg));
        }
        hir::Type::I16 => {
            f.instruction(&Instruction::I32Load16S(arg));
        }
        hir::Type::I64 | hir::Type::U64 => {
            f.instruction(&Instruction::I64Load(arg));
        }
        hir::Type::F32 => {
            f.instruction(&Instruction::F32Load(arg));
        }
        hir::Type::F64 => {
            f.instruction(&Instruction::F64Load(arg));
        }
        _ => {
            f.instruction(&Instruction::I32Load(arg));
        }
    };
}

// --- Type registry ---

struct TypeRegistry {
    entries: Vec<(Vec<ValType>, Vec<ValType>)>,
    map: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
}

impl TypeRegistry {
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn register(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&idx) = self.map.get(&key) {
            idx
        } else {
            let idx = self.entries.len() as u32;
            self.map.insert(key.clone(), idx);
            self.entries.push(key);
            idx
        }
    }

    fn build_section(&self) -> TypeSection {
        let mut types = TypeSection::new();
        for (params, results) in &self.entries {
            types.ty().function(params.clone(), results.clone());
        }
        types
    }
}

// --- Emit context ---

struct Builtins {
    println_i64: u32,
    println_u64: u32,
    println_bool: u32,
    println_f64: u32,
    alloc: u32,
    print_bytes: u32,
}

#[derive(Clone, Copy)]
struct DbgSite {
    ptr: u32,
    len: u32,
}

/// Static-memory location of a string literal's bytes.
#[derive(Clone, Copy)]
struct StrSite {
    ptr: u32,
    len: u32,
}

struct EmitCtx<'a> {
    program: &'a hir::Program,
    locals: HashMap<hir::SymbolId, u32>,
    funcs: &'a HashMap<hir::FuncId, u32>,
    runtime: &'a HashMap<hir::FuncId, String>,
    builtins: &'a Builtins,
    struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    scratch_base: u32,
    /// Counter increments once per `StructLit`, `Dbg`, or `Str` site
    /// encountered in pre-order. Indexes into the function's scratch
    /// local pool.
    scratch_counter: Cell<u32>,
    /// Per-function slice of dbg site memory layouts, indexed by `dbg_counter`.
    dbg_sites: &'a [DbgSite],
    dbg_counter: Cell<u32>,
    /// Per-function slice of string literal memory layouts.
    str_sites: &'a [StrSite],
    str_counter: Cell<u32>,
}

/// Look up the offset of a struct field by its name.
///
/// Walks `program.structs` to find the struct, then matches a field by its
/// resolved name string. The `lasso::ThreadedRodeo` interner is shared
/// across the whole compilation so `InternSymbol` comparisons would also
/// work, but resolving by string is one extra layer that makes this robust
/// against future name-collisions in synthetic symbols.
fn struct_field_offset(
    program: &hir::Program,
    layouts: &HashMap<hir::StructId, StructLayout>,
    struct_id: hir::StructId,
    field_name: &str,
) -> Option<u32> {
    let s = program.structs.iter().find(|s| s.id == struct_id)?;
    let field = s
        .fields
        .iter()
        .find(|f| program.interner.resolve(&f.name) == field_name)?;
    layouts
        .get(&struct_id)?
        .fields
        .get(&field.name)
        .map(|(off, _)| *off)
}

// --- Main entry point ---

pub fn generate_wasm(program: &hir::Program) -> Result<Vec<u8>, WasmError> {
    if program.main.is_none() {
        return Err(WasmError::MissingMain);
    }

    // Compute layout for every struct
    let mut struct_layouts: HashMap<hir::StructId, StructLayout> = HashMap::new();
    for s in &program.structs {
        struct_layouts.insert(s.id, compute_struct_layout(s));
    }

    let mut types = TypeRegistry::new();

    // Register builtin function types
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

    // Build func_map (user functions) and runtime_map (runtime-bound functions)
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

    // First pass: walk every user function in the same order they'll be emitted,
    // collect dbg prefix strings AND string literal bytes, lay them out in
    // static memory starting at STATIC_DATA_START. Record per-function slice
    // ranges so each function's EmitCtx can index into the global tables by
    // per-function counter.
    const STATIC_DATA_START: u32 = 128;
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

#[allow(clippy::too_many_arguments)]
fn build_emit_ctx<'a>(
    program: &'a hir::Program,
    func: &hir::Function,
    func_map: &'a HashMap<hir::FuncId, u32>,
    runtime_map: &'a HashMap<hir::FuncId, String>,
    builtins: &'a Builtins,
    struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    dbg_sites: &'a [DbgSite],
    str_sites: &'a [StrSite],
) -> EmitCtx<'a> {
    let mut locals = HashMap::new();
    for (i, param) in func.params.iter().enumerate() {
        locals.insert(param.name, i as u32);
    }
    let param_count = func.params.len() as u32;
    let body_locals = collect_locals(&func.body);
    for (i, (sym, _)) in body_locals.iter().enumerate() {
        locals.insert(*sym, param_count + i as u32);
    }
    let scratch_base = param_count + body_locals.len() as u32;
    EmitCtx {
        program,
        locals,
        funcs: func_map,
        runtime: runtime_map,
        builtins,
        struct_layouts,
        scratch_base,
        scratch_counter: Cell::new(0),
        dbg_sites,
        dbg_counter: Cell::new(0),
        str_sites,
        str_counter: Cell::new(0),
    }
}

// --- Local variable collection ---

fn collect_locals(block: &hir::Block) -> Vec<(hir::SymbolId, ValType)> {
    let mut locals = Vec::new();
    collect_locals_block(block, &mut locals);
    locals
}

fn collect_locals_block(block: &hir::Block, locals: &mut Vec<(hir::SymbolId, ValType)>) {
    for stmt in &block.stmts {
        collect_locals_stmt(stmt, locals);
    }
    if let Some(expr) = &block.expr {
        collect_locals_expr(expr, locals);
    }
}

fn collect_locals_stmt(stmt: &hir::Stmt, locals: &mut Vec<(hir::SymbolId, ValType)>) {
    match stmt {
        hir::Stmt::Let {
            name, ty, value, ..
        } => {
            locals.push((*name, hir_type_to_valtype(ty)));
            collect_locals_expr(value, locals);
        }
        hir::Stmt::Assign { value, .. } => collect_locals_expr(value, locals),
        hir::Stmt::Expr(e) => collect_locals_expr(e, locals),
        hir::Stmt::Loop { body, .. } => collect_locals_block(body, locals),
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_block(body, locals);
        }
        hir::Stmt::Break { .. } => {}
    }
}

fn collect_locals_expr(expr: &hir::Expr, locals: &mut Vec<(hir::SymbolId, ValType)>) {
    match &expr.kind {
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_block(then_branch, locals);
            if let Some(eb) = else_branch {
                collect_locals_block(eb, locals);
            }
        }
        hir::ExprKind::Block(block) => collect_locals_block(block, locals),
        hir::ExprKind::Binary { left, right, .. } => {
            collect_locals_expr(left, locals);
            collect_locals_expr(right, locals);
        }
        hir::ExprKind::Call { args, .. } => {
            for arg in args {
                collect_locals_expr(arg, locals);
            }
        }
        hir::ExprKind::StructLit { fields, .. } => {
            for (_, val) in fields {
                collect_locals_expr(val, locals);
            }
        }
        hir::ExprKind::Field { base, .. } => collect_locals_expr(base, locals),
        hir::ExprKind::Deref(base) => collect_locals_expr(base, locals),
        hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_locals_expr(e, locals);
            }
        }
        _ => {}
    }
}

// --- Scratch local collection ---
//
// Both StructLit and Dbg expressions need a scratch local during emission.
// We pre-walk the function body in the same pre-order that emission uses, so
// per-node counters in `EmitCtx` line up with this collected list.

fn collect_scratch_types_block(
    block: &hir::Block,
    runtime: &HashMap<hir::FuncId, String>,
    out: &mut Vec<ValType>,
) {
    for stmt in &block.stmts {
        collect_scratch_types_stmt(stmt, runtime, out);
    }
    if let Some(expr) = &block.expr {
        collect_scratch_types_expr(expr, runtime, out);
    }
}

fn collect_scratch_types_stmt(
    stmt: &hir::Stmt,
    runtime: &HashMap<hir::FuncId, String>,
    out: &mut Vec<ValType>,
) {
    match stmt {
        hir::Stmt::Let { value, .. } | hir::Stmt::Assign { value, .. } => {
            collect_scratch_types_expr(value, runtime, out);
        }
        hir::Stmt::Expr(e) => collect_scratch_types_expr(e, runtime, out),
        hir::Stmt::Loop { body, .. } => collect_scratch_types_block(body, runtime, out),
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_scratch_types_expr(condition, runtime, out);
            collect_scratch_types_block(body, runtime, out);
        }
        hir::Stmt::Break { .. } => {}
    }
}

fn collect_scratch_types_expr(
    expr: &hir::Expr,
    runtime: &HashMap<hir::FuncId, String>,
    out: &mut Vec<ValType>,
) {
    match &expr.kind {
        hir::ExprKind::StructLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(val, runtime, out);
            }
        }
        hir::ExprKind::Dbg { inner, .. } => {
            out.push(hir_type_to_valtype(&inner.ty));
            collect_scratch_types_expr(inner, runtime, out);
        }
        hir::ExprKind::Str(_) => {
            // One scratch i32 holding the bump-allocated String struct ptr.
            out.push(ValType::I32);
        }
        hir::ExprKind::Binary { left, right, .. } => {
            collect_scratch_types_expr(left, runtime, out);
            collect_scratch_types_expr(right, runtime, out);
        }
        hir::ExprKind::Call { func, args } => {
            // write(fd, s: String) needs one i32 scratch to duplicate the
            // String struct ptr across two field loads.
            if runtime.get(func).map(String::as_str) == Some("prim_rt_write") {
                out.push(ValType::I32);
            }
            for a in args {
                collect_scratch_types_expr(a, runtime, out);
            }
        }
        hir::ExprKind::Field { base, .. } | hir::ExprKind::Deref(base) => {
            collect_scratch_types_expr(base, runtime, out);
        }
        hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_scratch_types_expr(e, runtime, out);
            }
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_scratch_types_expr(condition, runtime, out);
            collect_scratch_types_block(then_branch, runtime, out);
            if let Some(eb) = else_branch {
                collect_scratch_types_block(eb, runtime, out);
            }
        }
        hir::ExprKind::Block(block) => collect_scratch_types_block(block, runtime, out),
        _ => {}
    }
}

// --- Dbg prefix collection (pre-order, matches emission order) ---

fn collect_dbg_prefixes_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a str>) {
    for stmt in &block.stmts {
        collect_dbg_prefixes_stmt(stmt, out);
    }
    if let Some(expr) = &block.expr {
        collect_dbg_prefixes_expr(expr, out);
    }
}

fn collect_dbg_prefixes_stmt<'a>(stmt: &'a hir::Stmt, out: &mut Vec<&'a str>) {
    match stmt {
        hir::Stmt::Let { value, .. } | hir::Stmt::Assign { value, .. } => {
            collect_dbg_prefixes_expr(value, out);
        }
        hir::Stmt::Expr(e) => collect_dbg_prefixes_expr(e, out),
        hir::Stmt::Loop { body, .. } => collect_dbg_prefixes_block(body, out),
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_dbg_prefixes_expr(condition, out);
            collect_dbg_prefixes_block(body, out);
        }
        hir::Stmt::Break { .. } => {}
    }
}

fn collect_dbg_prefixes_expr<'a>(expr: &'a hir::Expr, out: &mut Vec<&'a str>) {
    match &expr.kind {
        hir::ExprKind::Dbg { prefix, inner } => {
            out.push(prefix.as_str());
            collect_dbg_prefixes_expr(inner, out);
        }
        hir::ExprKind::StructLit { fields, .. } => {
            for (_, val) in fields {
                collect_dbg_prefixes_expr(val, out);
            }
        }
        hir::ExprKind::Binary { left, right, .. } => {
            collect_dbg_prefixes_expr(left, out);
            collect_dbg_prefixes_expr(right, out);
        }
        hir::ExprKind::Call { args, .. } => {
            for a in args {
                collect_dbg_prefixes_expr(a, out);
            }
        }
        hir::ExprKind::Field { base, .. } | hir::ExprKind::Deref(base) => {
            collect_dbg_prefixes_expr(base, out);
        }
        hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_dbg_prefixes_expr(e, out);
            }
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_dbg_prefixes_expr(condition, out);
            collect_dbg_prefixes_block(then_branch, out);
            if let Some(eb) = else_branch {
                collect_dbg_prefixes_block(eb, out);
            }
        }
        hir::ExprKind::Block(block) => collect_dbg_prefixes_block(block, out),
        _ => {}
    }
}

// --- String literal collection (pre-order, matches emission order) ---

fn collect_str_literals_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a str>) {
    for stmt in &block.stmts {
        collect_str_literals_stmt(stmt, out);
    }
    if let Some(expr) = &block.expr {
        collect_str_literals_expr(expr, out);
    }
}

fn collect_str_literals_stmt<'a>(stmt: &'a hir::Stmt, out: &mut Vec<&'a str>) {
    match stmt {
        hir::Stmt::Let { value, .. } | hir::Stmt::Assign { value, .. } => {
            collect_str_literals_expr(value, out);
        }
        hir::Stmt::Expr(e) => collect_str_literals_expr(e, out),
        hir::Stmt::Loop { body, .. } => collect_str_literals_block(body, out),
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_str_literals_expr(condition, out);
            collect_str_literals_block(body, out);
        }
        hir::Stmt::Break { .. } => {}
    }
}

fn collect_str_literals_expr<'a>(expr: &'a hir::Expr, out: &mut Vec<&'a str>) {
    match &expr.kind {
        hir::ExprKind::Str(s) => out.push(s.as_str()),
        hir::ExprKind::Dbg { inner, .. } => collect_str_literals_expr(inner, out),
        hir::ExprKind::StructLit { fields, .. } => {
            for (_, val) in fields {
                collect_str_literals_expr(val, out);
            }
        }
        hir::ExprKind::Binary { left, right, .. } => {
            collect_str_literals_expr(left, out);
            collect_str_literals_expr(right, out);
        }
        hir::ExprKind::Call { args, .. } => {
            for a in args {
                collect_str_literals_expr(a, out);
            }
        }
        hir::ExprKind::Field { base, .. } | hir::ExprKind::Deref(base) => {
            collect_str_literals_expr(base, out);
        }
        hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_str_literals_expr(e, out);
            }
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_str_literals_expr(condition, out);
            collect_str_literals_block(then_branch, out);
            if let Some(eb) = else_branch {
                collect_str_literals_block(eb, out);
            }
        }
        hir::ExprKind::Block(block) => collect_str_literals_block(block, out),
        _ => {}
    }
}

// --- User function emission ---

fn emit_user_function(func: &hir::Function, ctx: &EmitCtx) -> Result<Function, WasmError> {
    let body_locals = collect_locals(&func.body);
    let mut scratch_types: Vec<ValType> = Vec::new();
    collect_scratch_types_block(&func.body, ctx.runtime, &mut scratch_types);
    let mut wasm_locals: Vec<(u32, ValType)> = body_locals.iter().map(|(_, vt)| (1, *vt)).collect();
    for vt in &scratch_types {
        wasm_locals.push((1, *vt));
    }
    let mut f = Function::new(wasm_locals);
    emit_block(&mut f, &func.body, ctx)?;
    if let Some(ret_ty) = &func.ret {
        let needs_default = match &func.body.expr {
            Some(expr) => !produces_value(&expr.ty),
            None => true,
        };
        if needs_default {
            emit_default_value(&mut f, ret_ty);
        }
    }
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_default_value(f: &mut Function, ty: &hir::Type) {
    match hir_type_to_valtype(ty) {
        ValType::I32 => f.instruction(&Instruction::I32Const(0)),
        ValType::I64 => f.instruction(&Instruction::I64Const(0)),
        ValType::F32 => f.instruction(&Instruction::F32Const(0.0_f32.into())),
        ValType::F64 => f.instruction(&Instruction::F64Const(0.0_f64.into())),
        _ => f.instruction(&Instruction::I32Const(0)),
    };
}

fn emit_block(f: &mut Function, block: &hir::Block, ctx: &EmitCtx) -> Result<(), WasmError> {
    for stmt in &block.stmts {
        emit_stmt(f, stmt, ctx)?;
    }
    if let Some(expr) = &block.expr {
        emit_expr(f, expr, ctx)?;
    }
    Ok(())
}

fn emit_stmt(f: &mut Function, stmt: &hir::Stmt, ctx: &EmitCtx) -> Result<(), WasmError> {
    match stmt {
        hir::Stmt::Let { name, value, .. } => {
            emit_expr(f, value, ctx)?;
            if let Some(&idx) = ctx.locals.get(name) {
                f.instruction(&Instruction::LocalSet(idx));
            }
        }
        hir::Stmt::Assign { target, value, .. } => {
            emit_expr(f, value, ctx)?;
            if let Some(&idx) = ctx.locals.get(target) {
                f.instruction(&Instruction::LocalSet(idx));
            }
        }
        hir::Stmt::Expr(expr) => {
            emit_expr(f, expr, ctx)?;
            if produces_value(&expr.ty) {
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Stmt::Loop { body, .. } => {
            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            emit_block(f, body, ctx)?;
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::End);
        }
        hir::Stmt::While {
            condition, body, ..
        } => {
            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            emit_expr(f, condition, ctx)?;
            f.instruction(&Instruction::I32Eqz);
            f.instruction(&Instruction::BrIf(1));
            emit_block(f, body, ctx)?;
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::End);
        }
        hir::Stmt::Break { .. } => {
            f.instruction(&Instruction::Br(1));
        }
    }
    Ok(())
}

fn emit_expr(f: &mut Function, expr: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    match &expr.kind {
        hir::ExprKind::Int(n) => match hir_type_to_valtype(&expr.ty) {
            ValType::I64 => {
                f.instruction(&Instruction::I64Const(*n));
            }
            _ => {
                f.instruction(&Instruction::I32Const(*n as i32));
            }
        },
        hir::ExprKind::Float(v) => match &expr.ty {
            hir::Type::F32 => {
                f.instruction(&Instruction::F32Const((*v as f32).into()));
            }
            _ => {
                f.instruction(&Instruction::F64Const((*v).into()));
            }
        },
        hir::ExprKind::Bool(b) => {
            f.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
        }
        hir::ExprKind::Ident(sym) => {
            if let Some(&idx) = ctx.locals.get(sym) {
                f.instruction(&Instruction::LocalGet(idx));
            } else {
                f.instruction(&Instruction::Unreachable);
            }
        }
        hir::ExprKind::Binary { op, left, right } => {
            emit_expr(f, left, ctx)?;
            emit_expr(f, right, ctx)?;
            emit_binary_op(f, *op, &left.ty);
        }
        hir::ExprKind::Call { func, args } => {
            if let Some(binding) = ctx.runtime.get(func) {
                emit_runtime_call(f, binding, args, ctx)?;
            } else if let Some(&idx) = ctx.funcs.get(func) {
                for arg in args {
                    emit_expr(f, arg, ctx)?;
                }
                f.instruction(&Instruction::Call(idx));
            } else {
                f.instruction(&Instruction::Unreachable);
            }
        }
        hir::ExprKind::StructLit { struct_id, fields } => {
            emit_struct_lit(f, *struct_id, fields, ctx)?;
        }
        hir::ExprKind::Field { base, field } => {
            emit_expr(f, base, ctx)?;
            let struct_id = match &base.ty {
                hir::Type::Struct(id) => *id,
                _ => {
                    f.instruction(&Instruction::Unreachable);
                    return Ok(());
                }
            };
            let layout = match ctx.struct_layouts.get(&struct_id) {
                Some(l) => l,
                None => {
                    f.instruction(&Instruction::Unreachable);
                    return Ok(());
                }
            };
            let (offset, ty) = match layout.fields.get(field) {
                Some(t) => t.clone(),
                None => {
                    f.instruction(&Instruction::Unreachable);
                    return Ok(());
                }
            };
            emit_field_load(f, &ty, offset);
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            emit_expr(f, condition, ctx)?;
            if let Some(else_block) = else_branch {
                if produces_value(&expr.ty) {
                    let vt = hir_type_to_valtype(&expr.ty);
                    f.instruction(&Instruction::If(BlockType::Result(vt)));
                } else {
                    f.instruction(&Instruction::If(BlockType::Empty));
                }
                emit_block(f, then_branch, ctx)?;
                f.instruction(&Instruction::Else);
                emit_block(f, else_block, ctx)?;
                f.instruction(&Instruction::End);
            } else {
                f.instruction(&Instruction::If(BlockType::Empty));
                emit_block(f, then_branch, ctx)?;
                f.instruction(&Instruction::End);
            }
        }
        hir::ExprKind::Block(block) => {
            emit_block(f, block, ctx)?;
        }
        hir::ExprKind::Dbg { inner, .. } => {
            emit_dbg(f, inner, ctx)?;
        }
        hir::ExprKind::Str(_) => {
            emit_str_lit(f, &expr.ty, ctx);
        }
        _ => {
            f.instruction(&Instruction::Unreachable);
        }
    }
    Ok(())
}

/// Emit a string literal: bump-allocate a `String { data, len, cap }` struct
/// pointing at the literal's bytes (already laid out in static memory by
/// the program-level pre-walk). `cap == len` since the storage isn't
/// growable in place.
fn emit_str_lit(f: &mut Function, ty: &hir::Type, ctx: &EmitCtx) {
    let scratch_idx = ctx.scratch_counter.get();
    ctx.scratch_counter.set(scratch_idx + 1);
    let str_idx = ctx.str_counter.get();
    ctx.str_counter.set(str_idx + 1);
    let scratch_local = ctx.scratch_base + scratch_idx;

    let site = match ctx.str_sites.get(str_idx as usize) {
        Some(s) => *s,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    let struct_id = match ty {
        hir::Type::Struct(id) => *id,
        _ => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    let layout = match ctx.struct_layouts.get(&struct_id) {
        Some(l) => l,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    let data_off = match struct_field_offset(ctx.program, ctx.struct_layouts, struct_id, "data") {
        Some(o) => o,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };
    let len_off = match struct_field_offset(ctx.program, ctx.struct_layouts, struct_id, "len") {
        Some(o) => o,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };
    let cap_off = match struct_field_offset(ctx.program, ctx.struct_layouts, struct_id, "cap") {
        Some(o) => o,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    // ptr = __alloc(struct_size)
    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(scratch_local));

    // store data ptr (static offset)
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.ptr as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: data_off as u64,
        align: 2,
        memory_index: 0,
    }));

    // store len
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: len_off as u64,
        align: 2,
        memory_index: 0,
    }));

    // store cap = len
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: cap_off as u64,
        align: 2,
        memory_index: 0,
    }));

    // result: ptr to the struct
    f.instruction(&Instruction::LocalGet(scratch_local));
}

fn emit_dbg(f: &mut Function, inner: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    // Allocate scratch local and dbg slot in pre-order, matching the
    // pre-walks done in `collect_scratch_types_*` and `collect_dbg_prefixes_*`.
    let scratch_idx = ctx.scratch_counter.get();
    ctx.scratch_counter.set(scratch_idx + 1);
    let dbg_idx = ctx.dbg_counter.get();
    ctx.dbg_counter.set(dbg_idx + 1);
    let scratch_local = ctx.scratch_base + scratch_idx;
    let site = match ctx.dbg_sites.get(dbg_idx as usize) {
        Some(s) => *s,
        None => {
            // Indices out of sync with pre-walk — shouldn't happen, but trap.
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };

    // Emit the inner expression and stash its value.
    emit_expr(f, inner, ctx)?;
    f.instruction(&Instruction::LocalSet(scratch_local));

    // Print the prefix ("[file:line:col] expr_text = ") — no trailing newline.
    f.instruction(&Instruction::I32Const(site.ptr as i32));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::Call(ctx.builtins.print_bytes));

    // Print the value (with trailing newline, via __println_*).
    f.instruction(&Instruction::LocalGet(scratch_local));
    emit_println_for_ty(f, &inner.ty, ctx);

    // @dbg evaluates to its inner value.
    f.instruction(&Instruction::LocalGet(scratch_local));
    Ok(())
}

fn emit_println_for_ty(f: &mut Function, ty: &hir::Type, ctx: &EmitCtx) {
    match ty {
        hir::Type::I64 => {
            f.instruction(&Instruction::Call(ctx.builtins.println_i64));
        }
        hir::Type::I8 | hir::Type::I16 | hir::Type::I32 | hir::Type::Isize | hir::Type::IntVar => {
            f.instruction(&Instruction::I64ExtendI32S);
            f.instruction(&Instruction::Call(ctx.builtins.println_i64));
        }
        hir::Type::U64 => {
            f.instruction(&Instruction::Call(ctx.builtins.println_u64));
        }
        hir::Type::U8 | hir::Type::U16 | hir::Type::U32 | hir::Type::Usize => {
            f.instruction(&Instruction::I64ExtendI32U);
            f.instruction(&Instruction::Call(ctx.builtins.println_u64));
        }
        hir::Type::Bool => {
            f.instruction(&Instruction::Call(ctx.builtins.println_bool));
        }
        hir::Type::F64 | hir::Type::FloatVar => {
            f.instruction(&Instruction::Call(ctx.builtins.println_f64));
        }
        hir::Type::F32 => {
            f.instruction(&Instruction::F64PromoteF32);
            f.instruction(&Instruction::Call(ctx.builtins.println_f64));
        }
        _ => {
            f.instruction(&Instruction::Unreachable);
        }
    }
}

fn emit_struct_lit(
    f: &mut Function,
    struct_id: hir::StructId,
    fields: &[(hir::InternSymbol, hir::Expr)],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let ptr_local = ctx.scratch_base + counter;

    let layout = match ctx.struct_layouts.get(&struct_id) {
        Some(l) => l.clone(),
        None => {
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };

    // ptr_local = __alloc(size)
    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));

    // For each field: store at offset
    for (field_sym, value) in fields {
        let (offset, field_ty) = match layout.fields.get(field_sym) {
            Some(t) => t.clone(),
            None => {
                f.instruction(&Instruction::Unreachable);
                continue;
            }
        };
        f.instruction(&Instruction::LocalGet(ptr_local));
        emit_expr(f, value, ctx)?;
        emit_field_store(f, &field_ty, offset);
    }

    // Push ptr as the struct value
    f.instruction(&Instruction::LocalGet(ptr_local));
    Ok(())
}

fn emit_runtime_call(
    f: &mut Function,
    binding: &str,
    args: &[hir::Expr],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    match binding {
        "prim_rt_println_i64" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_i64));
        }
        "prim_rt_println_i32"
        | "prim_rt_println_i16"
        | "prim_rt_println_i8"
        | "prim_rt_println_isize" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32S);
            f.instruction(&Instruction::Call(ctx.builtins.println_i64));
        }
        "prim_rt_println_u64" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_u64));
        }
        "prim_rt_println_u32"
        | "prim_rt_println_u16"
        | "prim_rt_println_u8"
        | "prim_rt_println_usize" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32U);
            f.instruction(&Instruction::Call(ctx.builtins.println_u64));
        }
        "prim_rt_println_bool" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_bool));
        }
        "prim_rt_println_f64" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_f64));
        }
        "prim_rt_println_f32" => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::F64PromoteF32);
            f.instruction(&Instruction::Call(ctx.builtins.println_f64));
        }
        // write(fd, s: String) — ignore fd for now (always stdout); load
        // s.data + s.len from the String struct, hand to __print_bytes.
        "prim_rt_write" => {
            emit_write(f, args, ctx)?;
        }
        _ => {
            f.instruction(&Instruction::Unreachable);
        }
    }
    Ok(())
}

fn emit_write(f: &mut Function, args: &[hir::Expr], ctx: &EmitCtx) -> Result<(), WasmError> {
    if args.len() < 2 {
        f.instruction(&Instruction::Unreachable);
        return Ok(());
    }

    // Reserve scratch BEFORE evaluating args, matching the pre-walk in
    // `collect_scratch_types_expr` which adds this slot at the Call node
    // before descending into its arguments.
    let scratch_idx = ctx.scratch_counter.get();
    ctx.scratch_counter.set(scratch_idx + 1);
    let tmp_local = ctx.scratch_base + scratch_idx;

    // Evaluate fd, drop it — write always goes to stdout for now.
    emit_expr(f, &args[0], ctx)?;
    f.instruction(&Instruction::Drop);

    // Evaluate the String, leaving its struct ptr on stack.
    emit_expr(f, &args[1], ctx)?;

    let struct_id = match &args[1].ty {
        hir::Type::Struct(id) => *id,
        _ => {
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };
    let data_off = struct_field_offset(ctx.program, ctx.struct_layouts, struct_id, "data");
    let len_off = struct_field_offset(ctx.program, ctx.struct_layouts, struct_id, "len");
    let (data_off, len_off) = match (data_off, len_off) {
        (Some(d), Some(l)) => (d, l),
        _ => {
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };

    //   <struct ptr>                  ;; stack: [p]
    //   local.tee tmp                 ;; stack: [p], tmp=p
    //   i32.load offset=data_off      ;; stack: [data_ptr]
    //   local.get tmp                 ;; stack: [data_ptr, p]
    //   i32.load offset=len_off       ;; stack: [data_ptr, len]
    //   call __print_bytes
    f.instruction(&Instruction::LocalTee(tmp_local));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: data_off as u64,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(tmp_local));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: len_off as u64,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(ctx.builtins.print_bytes));

    Ok(())
}

fn emit_binary_op(f: &mut Function, op: hir::BinaryOp, operand_ty: &hir::Type) {
    let vt = hir_type_to_valtype(operand_ty);
    let signed = is_signed_int(operand_ty);

    match op {
        hir::BinaryOp::Add => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Add),
            ValType::I64 => f.instruction(&Instruction::I64Add),
            ValType::F32 => f.instruction(&Instruction::F32Add),
            ValType::F64 => f.instruction(&Instruction::F64Add),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Subtract => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Sub),
            ValType::I64 => f.instruction(&Instruction::I64Sub),
            ValType::F32 => f.instruction(&Instruction::F32Sub),
            ValType::F64 => f.instruction(&Instruction::F64Sub),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Multiply => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Mul),
            ValType::I64 => f.instruction(&Instruction::I64Mul),
            ValType::F32 => f.instruction(&Instruction::F32Mul),
            ValType::F64 => f.instruction(&Instruction::F64Mul),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Divide => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32DivS),
            (ValType::I32, false) => f.instruction(&Instruction::I32DivU),
            (ValType::I64, true) => f.instruction(&Instruction::I64DivS),
            (ValType::I64, false) => f.instruction(&Instruction::I64DivU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Div),
            (ValType::F64, _) => f.instruction(&Instruction::F64Div),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Modulo => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32RemS),
            (ValType::I32, false) => f.instruction(&Instruction::I32RemU),
            (ValType::I64, true) => f.instruction(&Instruction::I64RemS),
            (ValType::I64, false) => f.instruction(&Instruction::I64RemU),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Equals => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Eq),
            ValType::I64 => f.instruction(&Instruction::I64Eq),
            ValType::F32 => f.instruction(&Instruction::F32Eq),
            ValType::F64 => f.instruction(&Instruction::F64Eq),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::NotEquals => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Ne),
            ValType::I64 => f.instruction(&Instruction::I64Ne),
            ValType::F32 => f.instruction(&Instruction::F32Ne),
            ValType::F64 => f.instruction(&Instruction::F64Ne),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Greater => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32GtS),
            (ValType::I32, false) => f.instruction(&Instruction::I32GtU),
            (ValType::I64, true) => f.instruction(&Instruction::I64GtS),
            (ValType::I64, false) => f.instruction(&Instruction::I64GtU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Gt),
            (ValType::F64, _) => f.instruction(&Instruction::F64Gt),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::GreaterEquals => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32GeS),
            (ValType::I32, false) => f.instruction(&Instruction::I32GeU),
            (ValType::I64, true) => f.instruction(&Instruction::I64GeS),
            (ValType::I64, false) => f.instruction(&Instruction::I64GeU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Ge),
            (ValType::F64, _) => f.instruction(&Instruction::F64Ge),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::Less => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32LtS),
            (ValType::I32, false) => f.instruction(&Instruction::I32LtU),
            (ValType::I64, true) => f.instruction(&Instruction::I64LtS),
            (ValType::I64, false) => f.instruction(&Instruction::I64LtU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Lt),
            (ValType::F64, _) => f.instruction(&Instruction::F64Lt),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::LessEquals => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32LeS),
            (ValType::I32, false) => f.instruction(&Instruction::I32LeU),
            (ValType::I64, true) => f.instruction(&Instruction::I64LeS),
            (ValType::I64, false) => f.instruction(&Instruction::I64LeU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Le),
            (ValType::F64, _) => f.instruction(&Instruction::F64Le),
            _ => f.instruction(&Instruction::Unreachable),
        },
    };
}

// --- Builtin function emitters ---

fn emit_fd_write_buf(f: &mut Function, fd_write_idx: u32) {
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(fd_write_idx));
    f.instruction(&Instruction::Drop);
}

fn emit_newline(f: &mut Function, fd_write_idx: u32) {
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(NEWLINE_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(f, fd_write_idx);
}

/// `__print_bytes(ptr, len)` — write `len` bytes from `ptr` to stdout via
/// WASI fd_write. No trailing newline. Used by `@dbg` for its prefix string.
fn emit_print_bytes(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![]);
    let ptr: u32 = 0;
    let len: u32 = 1;

    // iovec at [0..8) = { buf: ptr, buf_len: len }
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalGet(len));
    f.instruction(&Instruction::I32Store(MEM32));

    emit_fd_write_buf(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

fn emit_alloc() -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);
    let size: u32 = 0;
    let ptr: u32 = 1;

    // ptr = heap_ptr
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::LocalSet(ptr));

    // heap_ptr = (ptr + size + 7) & ~7
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(size));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));

    // return ptr
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::End);
    f
}

fn emit_println_i64(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32), // ptr
        (1, ValType::I32), // is_neg
        (1, ValType::I64), // abs_val
    ]);

    let val: u32 = 0;
    let ptr: u32 = 1;
    let is_neg: u32 = 2;
    let abs_val: u32 = 3;

    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalSet(is_neg));

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I64Const(0x30));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Store8(MEM8));
    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Ne);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(0x2D));
    f.instruction(&Instruction::I32Store8(MEM8));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

fn emit_println_u64(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);

    let val: u32 = 0;
    let ptr: u32 = 1;

    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I64Const(0x30));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Store8(MEM8));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(val));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Ne);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

fn emit_println_bool(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![]);
    let val: u32 = 0;

    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(TRUE_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(FALSE_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::End);

    emit_fd_write_buf(&mut f, fd_write_idx);
    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

fn emit_println_f64(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::F64),
        (1, ValType::I64),
        (1, ValType::F64),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);

    let val: u32 = 0;
    let ptr: u32 = 1;
    let is_neg: u32 = 2;
    let abs_val: u32 = 3;
    let int_part: u32 = 4;
    let frac: u32 = 5;
    let frac_end: u32 = 6;
    let count: u32 = 7;

    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::F64Const(0.0_f64.into()));
    f.instruction(&Instruction::F64Lt);
    f.instruction(&Instruction::LocalSet(is_neg));

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::F64Neg);
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64TruncSatF64U);
    f.instruction(&Instruction::LocalSet(int_part));

    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::F64ConvertI64U);
    f.instruction(&Instruction::F64Sub);
    f.instruction(&Instruction::LocalSet(frac));

    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I64Const(0x30));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Store8(MEM8));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(int_part));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Ne);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(0x2D));
    f.instruction(&Instruction::I32Store8(MEM8));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(DOT_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH));
    f.instruction(&Instruction::LocalSet(frac_end));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(count));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::F64Const(10.0_f64.into()));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalSet(frac));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::I32TruncSatF64S);
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(MEM8));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::F64ConvertI32S);
    f.instruction(&Instruction::F64Sub);
    f.instruction(&Instruction::LocalSet(frac));

    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(frac_end));

    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(count));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::F64Const(1e-10_f64.into()));
    f.instruction(&Instruction::F64Lt);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH + 1));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Load8U(MEM8));
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(frac_end));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

fn emit_start(main_idx: u32, main_returns_value: bool) -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::Call(main_idx));
    if main_returns_value {
        f.instruction(&Instruction::Drop);
    }
    f.instruction(&Instruction::End);
    f
}
