//! Emission of user code: per-function context, expression/statement
//! lowering to wasm instructions, and the dispatch for runtime-bound calls.

use crate::WasmError;
use crate::builtins::Builtins;
use crate::layout::{EnumLayout, StructLayout, emit_field_load, emit_field_store};
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

#[derive(Clone, Copy)]
pub(crate) struct StringLayout {
    pub struct_id: hir::StructId,
    pub size: u32,
    pub data_offset: u32,
    pub len_offset: u32,
    pub cap_offset: u32,
}

/// Per-function emission state. Holds references to immutable program-wide
/// inputs (`program`, `funcs`, `runtime`, `builtins`, `struct_layouts`) plus
/// this function's own local scope and pre-allocated scratch slots.
pub(crate) struct EmitCtx<'a> {
    pub program: &'a hir::Program,
    pub locals: HashMap<hir::SymbolId, u32>,
    pub funcs: &'a HashMap<hir::FuncId, u32>,
    pub runtime: &'a HashMap<hir::FuncId, hir::RuntimeAbi>,
    pub builtins: &'a Builtins,
    pub struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    pub enum_layouts: &'a HashMap<hir::EnumId, EnumLayout>,
    pub string_layout: Option<StringLayout>,
    /// HIR GlobalId → wasm global index. User globals come after the heap
    /// pointer (wasm global 0).
    pub global_wasm_idx: &'a HashMap<hir::GlobalId, u32>,
    /// Per-trait wasm type index for `call_indirect` when dispatching that
    /// trait's methods. Method signature is uniform across the trait: an
    /// `i32` receiver (the boxed struct's data pointer) followed by the
    /// declared param types (excluding the receiver position).
    pub dyn_call_types: &'a HashMap<(hir::TraitId, u32), u32>,
    /// `(TraitId, StructId)` → static-memory address of the vtable (4
    /// bytes per slot, indexed by trait method position).
    pub vtable_addr: &'a HashMap<(hir::TraitId, hir::StructId), u32>,
    pub scratch_base: u32,
    pub scratch_counter: Cell<u32>,
    pub dbg_sites: &'a [DbgSite],
    pub dbg_counter: Cell<u32>,
    pub str_sites: &'a [StrSite],
    pub str_counter: Cell<u32>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_emit_ctx<'a>(
    program: &'a hir::Program,
    func: &hir::Function,
    func_map: &'a HashMap<hir::FuncId, u32>,
    runtime_map: &'a HashMap<hir::FuncId, hir::RuntimeAbi>,
    builtins: &'a Builtins,
    struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    enum_layouts: &'a HashMap<hir::EnumId, EnumLayout>,
    string_layout: Option<StringLayout>,
    global_wasm_idx: &'a HashMap<hir::GlobalId, u32>,
    dyn_call_types: &'a HashMap<(hir::TraitId, u32), u32>,
    vtable_addr: &'a HashMap<(hir::TraitId, hir::StructId), u32>,
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
        enum_layouts,
        string_layout,
        global_wasm_idx,
        dyn_call_types,
        vtable_addr,
        scratch_base,
        scratch_counter: Cell::new(0),
        dbg_sites,
        dbg_counter: Cell::new(0),
        str_sites,
        str_counter: Cell::new(0),
    }
}

/// If `sym` resolves to a module-level global, return its wasm global
/// index. Returns `None` if `sym` is anything else (local, function, etc.).
fn global_wasm_index(ctx: &EmitCtx, sym: hir::SymbolId) -> Option<u32> {
    let info = ctx.program.symbols.get(sym.0 as usize)?;
    let gid = match info.kind {
        hir::SymbolKind::Global(gid) => gid,
        _ => return None,
    };
    ctx.global_wasm_idx.get(&gid).copied()
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
            } else if let Some(g_idx) = global_wasm_index(ctx, *target) {
                f.instruction(&Instruction::GlobalSet(g_idx));
            } else {
                f.instruction(&Instruction::Unreachable);
            }
        }
        hir::Stmt::Return { value, .. } => {
            if let Some(expr) = value {
                emit_expr(f, expr, ctx)?;
            }
            f.instruction(&Instruction::Return);
        }
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            emit_expr(f, ptr, ctx)?;
            emit_expr(f, value, ctx)?;
            let pointee = match &ptr.ty {
                hir::Type::Pointer { pointee, .. } => (**pointee).clone(),
                _ => hir::Type::U8,
            };
            emit_field_store(f, &pointee, 0);
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
            } else if let Some(g_idx) = global_wasm_index(ctx, *sym) {
                f.instruction(&Instruction::GlobalGet(g_idx));
            } else {
                f.instruction(&Instruction::Unreachable);
            }
        }
        hir::ExprKind::Binary { op, left, right } => {
            emit_expr(f, left, ctx)?;
            emit_expr(f, right, ctx)?;
            emit_binary_op(f, *op, &left.ty);
        }
        hir::ExprKind::Call { func, args, .. } => {
            if let Some(&runtime) = ctx.runtime.get(func) {
                emit_runtime_call(f, runtime, args, ctx)?;
            } else if let Some(&idx) = ctx.funcs.get(func) {
                for arg in args {
                    emit_expr(f, arg, ctx)?;
                }
                f.instruction(&Instruction::Call(idx));
            } else {
                f.instruction(&Instruction::Unreachable);
            }
        }
        hir::ExprKind::StructLit {
            struct_id, fields, ..
        } => {
            emit_struct_lit(f, *struct_id, fields, ctx)?;
        }
        hir::ExprKind::VariantLit {
            enum_id,
            variant_idx,
            fields,
            ..
        } => {
            emit_variant_lit(f, *enum_id, *variant_idx, fields, ctx)?;
        }
        hir::ExprKind::Match { scrutinee, arms } => {
            emit_match(f, scrutinee, arms, &expr.ty, ctx)?;
        }
        hir::ExprKind::Field { base, field } => {
            emit_expr(f, base, ctx)?;
            let struct_id = match &base.ty {
                hir::Type::Struct(id, _) => *id,
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
        hir::ExprKind::Deref(operand) => {
            emit_expr(f, operand, ctx)?;
            emit_field_load(f, &expr.ty, 0);
        }
        hir::ExprKind::Coerce {
            value,
            source_struct,
            target_trait,
        } => {
            // Materialize a fat pointer `{vtable_addr: i32, data_ptr: i32}`
            // on the bump heap from a concrete struct value. The struct
            // value is already a pointer to its heap-allocated data, which
            // becomes data_ptr directly (no copy).
            //
            // Two scratch i32 locals must be claimed before evaluating the
            // inner expression so the order matches the pre-order walk
            // that reserved them in `collect_scratch_types_expr`.
            let data_local = ctx.scratch_base + ctx.scratch_counter.get();
            let fat_local = data_local + 1;
            ctx.scratch_counter.set(ctx.scratch_counter.get() + 2);

            emit_expr(f, value, ctx)?;
            f.instruction(&Instruction::LocalSet(data_local));

            f.instruction(&Instruction::I32Const(8));
            f.instruction(&Instruction::Call(ctx.builtins.alloc));
            f.instruction(&Instruction::LocalTee(fat_local));

            let vt_addr = *ctx
                .vtable_addr
                .get(&(*target_trait, *source_struct))
                .expect("missing vtable for coercion");
            f.instruction(&Instruction::I32Const(vt_addr as i32));
            f.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));

            f.instruction(&Instruction::LocalGet(fat_local));
            f.instruction(&Instruction::LocalGet(data_local));
            f.instruction(&Instruction::I32Store(MemArg {
                offset: 4,
                align: 2,
                memory_index: 0,
            }));

            f.instruction(&Instruction::LocalGet(fat_local));
        }
        hir::ExprKind::DynCall {
            receiver,
            trait_id,
            method_idx,
            args,
        } => {
            // Stash the fat pointer once so we can load data_ptr (for the
            // receiver argument) and vtable_addr (to index for the function
            // table slot) without reevaluating the receiver expression.
            // Scratch local is claimed before walking children.
            let fat_local = ctx.scratch_base + ctx.scratch_counter.get();
            ctx.scratch_counter.set(ctx.scratch_counter.get() + 1);

            emit_expr(f, receiver, ctx)?;
            f.instruction(&Instruction::LocalSet(fat_local));

            f.instruction(&Instruction::LocalGet(fat_local));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 4,
                align: 2,
                memory_index: 0,
            }));

            for arg in args {
                emit_expr(f, arg, ctx)?;
            }

            f.instruction(&Instruction::LocalGet(fat_local));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            f.instruction(&Instruction::I32Const((*method_idx as i32) * 4));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));

            let type_idx = *ctx
                .dyn_call_types
                .get(&(*trait_id, *method_idx))
                .expect("missing dyn call type index");
            f.instruction(&Instruction::CallIndirect {
                type_index: type_idx,
                table_index: 0,
            });
        }
        hir::ExprKind::BitNot(operand) => {
            emit_expr(f, operand, ctx)?;
            match hir_type_to_valtype(&expr.ty) {
                ValType::I32 => {
                    f.instruction(&Instruction::I32Const(-1));
                    f.instruction(&Instruction::I32Xor);
                }
                ValType::I64 => {
                    f.instruction(&Instruction::I64Const(-1));
                    f.instruction(&Instruction::I64Xor);
                }
                _ => {
                    f.instruction(&Instruction::Unreachable);
                }
            }
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
        hir::Type::Struct(id, _) => *id,
        _ => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    let string_layout = match ctx.string_layout {
        Some(layout) if layout.struct_id == struct_id => layout,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
        Some(_) => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    // ptr = __alloc(struct_size)
    f.instruction(&Instruction::I32Const(string_layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(scratch_local));

    // store data ptr (static offset)
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.ptr as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: string_layout.data_offset as u64,
        align: 2,
        memory_index: 0,
    }));

    // store len
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: string_layout.len_offset as u64,
        align: 2,
        memory_index: 0,
    }));

    // store cap = len
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: string_layout.cap_offset as u64,
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

fn emit_variant_lit(
    f: &mut Function,
    enum_id: hir::EnumId,
    variant_idx: u32,
    fields: &[(hir::InternSymbol, hir::Expr)],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let ptr_local = ctx.scratch_base + counter;

    let layout = match ctx.enum_layouts.get(&enum_id) {
        Some(l) => l,
        None => {
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };
    let variant = match layout.variants.get(variant_idx as usize) {
        Some(v) => v,
        None => {
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };

    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));

    f.instruction(&Instruction::LocalGet(ptr_local));
    f.instruction(&Instruction::I32Const(variant_idx as i32));
    emit_field_store(f, &hir::Type::U32, 0);

    for (field_sym, value) in fields {
        let (payload_offset, field_ty) = match variant.fields.get(field_sym) {
            Some(t) => t.clone(),
            None => {
                f.instruction(&Instruction::Unreachable);
                continue;
            }
        };
        f.instruction(&Instruction::LocalGet(ptr_local));
        emit_expr(f, value, ctx)?;
        emit_field_store(f, &field_ty, 8 + payload_offset);
    }

    f.instruction(&Instruction::LocalGet(ptr_local));
    Ok(())
}

fn emit_match(
    f: &mut Function,
    scrutinee: &hir::Expr,
    arms: &[hir::MatchArm],
    result_ty: &hir::Type,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let scrutinee_local = ctx.scratch_base + counter;
    let enum_id = match &scrutinee.ty {
        hir::Type::Enum(id, _) => *id,
        _ => {
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };

    emit_expr(f, scrutinee, ctx)?;
    f.instruction(&Instruction::LocalSet(scrutinee_local));
    emit_match_arm_chain(f, scrutinee_local, enum_id, arms, 0, result_ty, ctx)
}

fn emit_match_arm_chain(
    f: &mut Function,
    scrutinee_local: u32,
    enum_id: hir::EnumId,
    arms: &[hir::MatchArm],
    index: usize,
    result_ty: &hir::Type,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let Some(arm) = arms.get(index) else {
        f.instruction(&Instruction::Unreachable);
        if produces_value(result_ty) {
            emit_default_value(f, result_ty);
        }
        return Ok(());
    };

    match &arm.pattern {
        hir::Pattern::Wildcard { .. } => {
            emit_expr(f, &arm.body, ctx)?;
        }
        hir::Pattern::Variant {
            variant_idx,
            bindings,
            ..
        } => {
            f.instruction(&Instruction::LocalGet(scrutinee_local));
            emit_field_load(f, &hir::Type::U32, 0);
            f.instruction(&Instruction::I32Const(*variant_idx as i32));
            f.instruction(&Instruction::I32Eq);

            if produces_value(result_ty) {
                f.instruction(&Instruction::If(BlockType::Result(hir_type_to_valtype(
                    result_ty,
                ))));
            } else {
                f.instruction(&Instruction::If(BlockType::Empty));
            }

            emit_match_bindings(f, scrutinee_local, enum_id, *variant_idx, bindings, ctx);
            emit_expr(f, &arm.body, ctx)?;

            f.instruction(&Instruction::Else);
            emit_match_arm_chain(f, scrutinee_local, enum_id, arms, index + 1, result_ty, ctx)?;
            f.instruction(&Instruction::End);
        }
    }

    Ok(())
}

fn emit_match_bindings(
    f: &mut Function,
    scrutinee_local: u32,
    enum_id: hir::EnumId,
    variant_idx: u32,
    bindings: &[(hir::InternSymbol, hir::SymbolId, hir::Type)],
    ctx: &EmitCtx,
) {
    let variant = match ctx
        .enum_layouts
        .get(&enum_id)
        .and_then(|layout| layout.variants.get(variant_idx as usize))
    {
        Some(v) => v,
        None => {
            f.instruction(&Instruction::Unreachable);
            return;
        }
    };

    for (field_sym, binding_sym, _) in bindings {
        let (payload_offset, field_ty) = match variant.fields.get(field_sym) {
            Some(t) => t.clone(),
            None => {
                f.instruction(&Instruction::Unreachable);
                continue;
            }
        };
        let Some(&local) = ctx.locals.get(binding_sym) else {
            f.instruction(&Instruction::Unreachable);
            continue;
        };
        f.instruction(&Instruction::LocalGet(scrutinee_local));
        emit_field_load(f, &field_ty, 8 + payload_offset);
        f.instruction(&Instruction::LocalSet(local));
    }
}

fn emit_runtime_call(
    f: &mut Function,
    runtime: hir::RuntimeAbi,
    args: &[hir::Expr],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    match runtime {
        hir::RuntimeAbi::PrintlnI64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_i64));
        }
        hir::RuntimeAbi::PrintlnI32
        | hir::RuntimeAbi::PrintlnI16
        | hir::RuntimeAbi::PrintlnI8
        | hir::RuntimeAbi::PrintlnIsize => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32S);
            f.instruction(&Instruction::Call(ctx.builtins.println_i64));
        }
        hir::RuntimeAbi::PrintlnU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_u64));
        }
        hir::RuntimeAbi::PrintlnU32
        | hir::RuntimeAbi::PrintlnU16
        | hir::RuntimeAbi::PrintlnU8
        | hir::RuntimeAbi::PrintlnUsize => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32U);
            f.instruction(&Instruction::Call(ctx.builtins.println_u64));
        }
        hir::RuntimeAbi::PrintlnBool => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_bool));
        }
        hir::RuntimeAbi::PrintlnF64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.println_f64));
        }
        hir::RuntimeAbi::PrintlnF32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::F64PromoteF32);
            f.instruction(&Instruction::Call(ctx.builtins.println_f64));
        }
        // write(fd, s: String) — ignore fd for now (always stdout); load
        // s.data + s.len from the String struct, hand to __print_bytes.
        hir::RuntimeAbi::Write => {
            emit_write(f, args, ctx)?;
        }
        // Cooperative yield — suspend with the scheduler's yield tag.
        // Control returns to the scheduler's `on $yield` handler in `_start`,
        // which reschedules immediately (single-task case) or picks another
        // runnable continuation (future, when a queue exists).
        hir::RuntimeAbi::Yield => {
            f.instruction(&Instruction::Suspend(ctx.builtins.yield_tag));
        }
        // ---- pointer ops on *mut u8 and *mut u32 ----
        // null_T(): push 0 as an i32 (any pointer type lowers to i32).
        // ptr_add/sub/offset_T: scale n by sizeof(T), then add/sub.
        // ptr_byte_*: skip the scaling.
        // ptr_addr_T: pointer is already an i32, no-op.
        hir::RuntimeAbi::NullMutU8
        | hir::RuntimeAbi::NullMutU32
        | hir::RuntimeAbi::NullMutUsize => {
            f.instruction(&Instruction::I32Const(0));
        }
        hir::RuntimeAbi::PtrAddMutU8 | hir::RuntimeAbi::PtrByteAddMutU8 => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrSubMutU8 | hir::RuntimeAbi::PtrByteSubMutU8 => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrOffsetMutU8 | hir::RuntimeAbi::PtrByteOffsetMutU8 => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrAddMutU32 | hir::RuntimeAbi::PtrAddMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Const(4));
            f.instruction(&Instruction::I32Mul);
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrSubMutU32 | hir::RuntimeAbi::PtrSubMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Const(4));
            f.instruction(&Instruction::I32Mul);
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrOffsetMutU32 | hir::RuntimeAbi::PtrOffsetMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Const(4));
            f.instruction(&Instruction::I32Mul);
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrByteAddMutU32 | hir::RuntimeAbi::PtrByteAddMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrByteSubMutU32 | hir::RuntimeAbi::PtrByteSubMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrByteOffsetMutU32 | hir::RuntimeAbi::PtrByteOffsetMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrAddrMutU8
        | hir::RuntimeAbi::PtrAddrMutU32
        | hir::RuntimeAbi::PtrAddrMutUsize => {
            emit_expr(f, &args[0], ctx)?;
        }
        hir::RuntimeAbi::MemoryGrow => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::MemoryGrow(0));
        }
        hir::RuntimeAbi::MemoryCopy => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        }
        hir::RuntimeAbi::MemoryFill => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::MemoryFill(0));
        }
        hir::RuntimeAbi::ClzU32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Clz);
        }
        hir::RuntimeAbi::CtzU32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Ctz);
        }
        hir::RuntimeAbi::PopcntU32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Popcnt);
        }
        hir::RuntimeAbi::ClzU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64Clz);
        }
        hir::RuntimeAbi::CtzU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64Ctz);
        }
        hir::RuntimeAbi::PopcntU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64Popcnt);
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
        hir::Type::Struct(id, _) => *id,
        _ => {
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::Unreachable);
            return Ok(());
        }
    };
    let string_layout = match ctx.string_layout {
        Some(layout) if layout.struct_id == struct_id => layout,
        Some(_) | None => {
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
        offset: string_layout.data_offset as u64,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(tmp_local));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: string_layout.len_offset as u64,
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
        hir::BinaryOp::BitAnd => match vt {
            ValType::I32 => f.instruction(&Instruction::I32And),
            ValType::I64 => f.instruction(&Instruction::I64And),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::BitOr => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Or),
            ValType::I64 => f.instruction(&Instruction::I64Or),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::BitXor => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Xor),
            ValType::I64 => f.instruction(&Instruction::I64Xor),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::ShiftLeft => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Shl),
            ValType::I64 => f.instruction(&Instruction::I64Shl),
            _ => f.instruction(&Instruction::Unreachable),
        },
        hir::BinaryOp::ShiftRight => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32ShrS),
            (ValType::I32, false) => f.instruction(&Instruction::I32ShrU),
            (ValType::I64, true) => f.instruction(&Instruction::I64ShrS),
            (ValType::I64, false) => f.instruction(&Instruction::I64ShrU),
            _ => f.instruction(&Instruction::Unreachable),
        },
    };
}
