//! Emission of user code: per-function context, expression/statement
//! lowering to wasm instructions, and the dispatch for runtime-bound calls.

use crate::WasmError;
use crate::builtins::Builtins;
use crate::layout::{StructLayout, emit_field_load, emit_field_store};
use crate::types::{hir_type_to_valtype, is_signed_int, produces_value};
use crate::walks::{collect_locals, collect_scratch_types_block};
use prim_compiler::hir;
use std::cell::Cell;
use std::collections::HashMap;
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

/// Static-memory location of an `@dbg` site's prefix bytes.
#[derive(Clone, Copy)]
pub(crate) struct DbgSite {
    pub ptr: u32,
    pub len: u32,
}

/// Static-memory location of a string literal's bytes.
#[derive(Clone, Copy)]
pub(crate) struct StrSite {
    pub ptr: u32,
    pub len: u32,
}

/// Per-function emission state. Holds references to immutable program-wide
/// inputs (`program`, `funcs`, `runtime`, `builtins`, `struct_layouts`) plus
/// this function's own local scope and pre-allocated scratch slots.
pub(crate) struct EmitCtx<'a> {
    pub program: &'a hir::Program,
    pub locals: HashMap<hir::SymbolId, u32>,
    pub funcs: &'a HashMap<hir::FuncId, u32>,
    pub runtime: &'a HashMap<hir::FuncId, String>,
    pub builtins: &'a Builtins,
    pub struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    /// First wasm local index past the regular locals; scratch slots live at
    /// `scratch_base + 0`, `scratch_base + 1`, …
    pub scratch_base: u32,
    /// Counter increments once per `StructLit`, `Dbg`, `Str`, or
    /// runtime-write site encountered in pre-order. Indexes into the
    /// function's scratch local pool.
    pub scratch_counter: Cell<u32>,
    /// Per-function slice of dbg site memory layouts, indexed by `dbg_counter`.
    pub dbg_sites: &'a [DbgSite],
    pub dbg_counter: Cell<u32>,
    /// Per-function slice of string literal memory layouts.
    pub str_sites: &'a [StrSite],
    pub str_counter: Cell<u32>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_emit_ctx<'a>(
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

/// Look up the offset of a struct field by its name.
///
/// Walks `program.structs` to find the struct, then matches a field by its
/// resolved name string. The shared `lasso::ThreadedRodeo` interner across
/// the compilation means `InternSymbol` comparisons would also work; this
/// resolves by string for robustness against any future synthetic-symbol
/// edge cases.
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

// === Function body emission ===

pub(crate) fn emit_user_function(
    func: &hir::Function,
    ctx: &EmitCtx,
) -> Result<Function, WasmError> {
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
