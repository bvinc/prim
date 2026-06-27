//! Emission of user code: per-function context, expression/statement
//! lowering to wasm instructions, and the dispatch for runtime-bound calls.

use crate::WasmError;
use crate::builtins::Builtins;
use crate::layout::{
    CLOCK_SCRATCH, EnumLayout, POLL_NEVENTS, StructLayout, compute_struct_layout,
    compute_tuple_layout, emit_field_load, emit_field_store,
};
use crate::types::{hir_type_to_valtype, is_signed_int, produces_value};
use crate::walks::{collect_locals, collect_scratch_types_block};
use prim_compiler::hir;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
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
    /// Concrete needs-drop type → wasm index of its synthesized `drop_T`
    /// function. A `Stmt::Drop` of type `T` lowers to a call of `drop_fns[T]`.
    pub drop_fns: &'a HashMap<hir::Type, u32>,
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
    /// Number of wasm structured blocks (`block`/`loop`/`if`) currently open
    /// at the emission cursor. Used to compute correct relative branch
    /// targets for `break`.
    pub ctrl_depth: Cell<u32>,
    /// For each enclosing loop (innermost last), the `ctrl_depth` just before
    /// its exit `block` was opened — i.e. that block's branch level. `break`
    /// targets the innermost.
    pub loop_exits: RefCell<Vec<u32>>,
    pub dbg_sites: &'a [DbgSite],
    pub dbg_counter: Cell<u32>,
    pub str_sites: &'a [StrSite],
    pub str_counter: Cell<u32>,
    /// First codegen invariant violation hit while emitting this function. The
    /// `()`-returning emit helpers record it here (and still emit a placeholder
    /// `unreachable` to keep the partial function well-formed); `emit_user_function`
    /// turns it into a hard build failure instead of shipping a trap.
    pub codegen_error: RefCell<Option<crate::WasmError>>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_emit_ctx<'a>(
    program: &'a hir::Program,
    func: &hir::Function,
    func_map: &'a HashMap<hir::FuncId, u32>,
    drop_fns: &'a HashMap<hir::Type, u32>,
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
        drop_fns,
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
        ctrl_depth: Cell::new(0),
        loop_exits: RefCell::new(Vec::new()),
        dbg_sites,
        dbg_counter: Cell::new(0),
        str_sites,
        str_counter: Cell::new(0),
        codegen_error: RefCell::new(None),
    }
}

impl EmitCtx<'_> {
    /// Record a codegen invariant violation (keeping the first one). Used by the
    /// `()`-returning emit helpers, which can't return a `Result`; the function
    /// emitter checks `take_error` afterwards and fails the build.
    fn fail(&self, msg: impl Into<String>) {
        let mut slot = self.codegen_error.borrow_mut();
        if slot.is_none() {
            *slot = Some(crate::WasmError::Internal(msg.into()));
        }
    }

    /// Take the recorded codegen error, if any.
    fn take_error(&self) -> Option<crate::WasmError> {
        self.codegen_error.borrow_mut().take()
    }

    /// Record that a wasm structured block has just been opened.
    fn enter_ctrl(&self) {
        self.ctrl_depth.set(self.ctrl_depth.get() + 1);
    }

    /// Record that a wasm structured block has just been closed (`end`).
    fn exit_ctrl(&self) {
        self.ctrl_depth.set(self.ctrl_depth.get() - 1);
    }

    /// Enter a loop whose exit `block` was opened at the current depth.
    fn enter_loop(&self) {
        self.loop_exits.borrow_mut().push(self.ctrl_depth.get());
    }

    fn exit_loop(&self) {
        self.loop_exits.borrow_mut().pop();
    }

    /// Relative branch target for `break`: the distance from the current
    /// depth out to the innermost enclosing loop's exit block. Accounts for
    /// any `if`/`match` blocks between the `break` and the loop.
    fn break_target(&self) -> u32 {
        let exit_level = *self
            .loop_exits
            .borrow()
            .last()
            .expect("break outside of a loop");
        self.ctrl_depth.get() - 1 - exit_level
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
    // A `()`-returning helper may have recorded an unlowerable shape; surface it
    // as a build failure rather than shipping the placeholder trap.
    if let Some(err) = ctx.take_error() {
        return Err(err);
    }
    Ok(f)
}

/// Record a codegen invariant violation and emit a placeholder `unreachable` so
/// the partial function stays well-formed. The recorded error is turned into a
/// build failure by `emit_user_function`. For use in `()`-returning emit helpers
/// that cannot return a `Result`.
fn bail(f: &mut Function, ctx: &EmitCtx, msg: impl Into<String>) {
    ctx.fail(msg);
    f.instruction(&Instruction::Unreachable);
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
        hir::Stmt::Let { pattern, value, .. } => {
            // Evaluate the initializer, leaving it on the stack, then bind it by
            // walking the (irrefutable) pattern — the same binder `match` uses,
            // fed from the stack and never hitting a refutable (testing) arm.
            emit_expr(f, value, ctx)?;
            emit_test_bind(f, Src::Stack, &value.ty, pattern, ctx);
        }
        hir::Stmt::Assign { target, value, .. } => {
            emit_expr(f, value, ctx)?;
            if let Some(&idx) = ctx.locals.get(target) {
                f.instruction(&Instruction::LocalSet(idx));
            } else if let Some(g_idx) = global_wasm_index(ctx, *target) {
                f.instruction(&Instruction::GlobalSet(g_idx));
            } else {
                return Err(WasmError::Internal("assignment to unresolved name".into()));
            }
        }
        hir::Stmt::Return { value, .. } => {
            if let Some(expr) = value {
                emit_expr(f, expr, ctx)?;
            }
            f.instruction(&Instruction::Return);
        }
        hir::Stmt::Drop { sym, ty, .. } => {
            emit_drop(f, *sym, ty, ctx);
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
        hir::Stmt::FieldAssign {
            object,
            field,
            value,
            ..
        } => {
            // Push the struct pointer, look up the field's offset/type, push
            // the value, then store it into the field.
            let field_layout = match &object.ty {
                hir::Type::Struct(id, _) => ctx
                    .struct_layouts
                    .get(id)
                    .and_then(|layout| layout.fields.get(field).cloned()),
                _ => None,
            };
            emit_expr(f, object, ctx)?;
            match field_layout {
                Some((offset, ty)) => {
                    emit_expr(f, value, ctx)?;
                    emit_field_store(f, &ty, offset);
                }
                None => {
                    return Err(WasmError::Internal(
                        "field not found in struct layout".into(),
                    ));
                }
            }
        }
        hir::Stmt::Expr(expr) => {
            emit_expr(f, expr, ctx)?;
            if produces_value(&expr.ty) {
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Stmt::Loop { body, .. } => {
            ctx.enter_loop();
            f.instruction(&Instruction::Block(BlockType::Empty));
            ctx.enter_ctrl();
            f.instruction(&Instruction::Loop(BlockType::Empty));
            ctx.enter_ctrl();
            emit_block(f, body, ctx)?;
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            ctx.exit_loop();
        }
        hir::Stmt::While {
            condition, body, ..
        } => {
            ctx.enter_loop();
            f.instruction(&Instruction::Block(BlockType::Empty));
            ctx.enter_ctrl();
            f.instruction(&Instruction::Loop(BlockType::Empty));
            ctx.enter_ctrl();
            emit_expr(f, condition, ctx)?;
            f.instruction(&Instruction::I32Eqz);
            f.instruction(&Instruction::BrIf(1));
            emit_block(f, body, ctx)?;
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            ctx.exit_loop();
        }
        hir::Stmt::Break { .. } => {
            f.instruction(&Instruction::Br(ctx.break_target()));
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
                return Err(WasmError::Internal("unresolved name in expression".into()));
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
                return Err(WasmError::Internal("call to unresolved function".into()));
            }
        }
        // spawn(f): make a task continuation from `f` and append it to the
        // task table; table.grow leaves the new slot index (the handle).
        hir::ExprKind::Spawn { func } => {
            let fidx = *ctx
                .funcs
                .get(func)
                .expect("spawn target missing from func map");
            f.instruction(&Instruction::RefFunc(fidx));
            f.instruction(&Instruction::ContNew(ctx.builtins.cont_type));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::TableGrow(ctx.builtins.cont_table));
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
                    return Err(WasmError::Internal(
                        "field access on non-struct type".into(),
                    ));
                }
            };
            let layout = match ctx.struct_layouts.get(&struct_id) {
                Some(l) => l,
                None => {
                    return Err(WasmError::Internal("missing struct layout".into()));
                }
            };
            let (offset, ty) = match layout.fields.get(field) {
                Some(t) => t.clone(),
                None => {
                    return Err(WasmError::Internal(
                        "field not found in struct layout".into(),
                    ));
                }
            };
            emit_field_load(f, &ty, offset);
        }
        hir::ExprKind::TupleLit(elems) => {
            emit_tuple_lit(f, &expr.ty, elems, ctx)?;
        }
        hir::ExprKind::TupleIndex { base, index } => {
            emit_expr(f, base, ctx)?;
            let elem_types = match &base.ty {
                hir::Type::Tuple(elems) => elems,
                _ => {
                    return Err(WasmError::Internal("tuple index on non-tuple type".into()));
                }
            };
            let layout = compute_tuple_layout(elem_types);
            match layout.elems.get(*index as usize) {
                Some((offset, ty)) => emit_field_load(f, ty, *offset),
                None => {
                    return Err(WasmError::Internal("tuple index out of range".into()));
                }
            }
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
                ctx.enter_ctrl();
                emit_block(f, then_branch, ctx)?;
                f.instruction(&Instruction::Else);
                emit_block(f, else_block, ctx)?;
                f.instruction(&Instruction::End);
                ctx.exit_ctrl();
            } else {
                f.instruction(&Instruction::If(BlockType::Empty));
                ctx.enter_ctrl();
                emit_block(f, then_branch, ctx)?;
                f.instruction(&Instruction::End);
                ctx.exit_ctrl();
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
            arg_modes: _,
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
            if matches!(operand.ty, hir::Type::Bool) {
                // Logical NOT: bool is 0/1, so `== 0` flips it.
                f.instruction(&Instruction::I32Eqz);
                return Ok(());
            }
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
                    return Err(WasmError::Internal(
                        "bitwise-not on unsupported operand type".into(),
                    ));
                }
            }
        }
        hir::ExprKind::Neg(operand) => {
            // Floats have a dedicated negate; integers have none, so subtract
            // from zero (push 0, then the operand, then sub).
            match hir_type_to_valtype(&expr.ty) {
                ValType::F32 => {
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::F32Neg);
                }
                ValType::F64 => {
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::F64Neg);
                }
                ValType::I64 => {
                    f.instruction(&Instruction::I64Const(0));
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::I64Sub);
                }
                _ => {
                    f.instruction(&Instruction::I32Const(0));
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::I32Sub);
                }
            }
        }
        _ => {
            return Err(WasmError::Internal(
                "unsupported expression kind in codegen".into(),
            ));
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
            bail(
                f,
                ctx,
                "string-literal site index out of sync with pre-walk",
            );
            return;
        }
    };

    let struct_id = match ty {
        hir::Type::Struct(id, _) => *id,
        _ => {
            bail(f, ctx, "string literal with non-struct type");
            return;
        }
    };

    let string_layout = match ctx.string_layout {
        Some(layout) if layout.struct_id == struct_id => layout,
        None => {
            bail(f, ctx, "missing string layout");
            return;
        }
        Some(_) => {
            bail(f, ctx, "string layout struct id mismatch");
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
            return Err(WasmError::Internal(
                "dbg site index out of sync with pre-walk".into(),
            ));
        }
    };

    // Emit the inner expression and stash its value.
    emit_expr(f, inner, ctx)?;
    f.instruction(&Instruction::LocalSet(scratch_local));

    // Print the prefix ("[file:line:col] expr_text = ") to stdout — no
    // trailing newline; ignore the returned byte count.
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(site.ptr as i32));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::Call(ctx.builtins.write_bytes));
    f.instruction(&Instruction::Drop);

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
            bail(f, ctx, "no println for this type");
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
            bail(f, ctx, "missing struct layout");
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
                bail(f, ctx, "field not found in struct layout");
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

/// Emit a tuple literal: heap-allocate the positional layout, store each
/// element at its offset, and leave the pointer on the stack. Mirrors
/// `emit_struct_lit` but the layout is computed from `tuple_ty`'s elements.
fn emit_tuple_lit(
    f: &mut Function,
    tuple_ty: &hir::Type,
    elems: &[hir::Expr],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let ptr_local = ctx.scratch_base + counter;

    let elem_types = match tuple_ty {
        hir::Type::Tuple(ts) => ts,
        _ => {
            bail(f, ctx, "tuple literal with non-tuple type");
            return Ok(());
        }
    };
    let layout = compute_tuple_layout(elem_types);

    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));

    for (value, (offset, elem_ty)) in elems.iter().zip(layout.elems.iter()) {
        f.instruction(&Instruction::LocalGet(ptr_local));
        emit_expr(f, value, ctx)?;
        emit_field_store(f, elem_ty, *offset);
    }

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
            bail(f, ctx, "missing enum layout");
            return Ok(());
        }
    };
    let variant = match layout.variants.get(variant_idx as usize) {
        Some(v) => v,
        None => {
            bail(f, ctx, "missing enum variant layout");
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
                bail(f, ctx, "variant field not found in layout");
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

/// Where the value a sub-pattern matches against lives.
#[derive(Clone, Copy)]
enum Src {
    /// Directly in a local (the scrutinee, or a stashed aggregate pointer).
    Local(u32),
    /// At `base + offset` in linear memory (a tuple element or variant field).
    Field { base: u32, offset: u32 },
    /// On top of the operand stack (a `let` initializer just evaluated). Unlike
    /// a local or field it can be read only once, so any pattern that needs the
    /// value more than once stashes it to a scratch local first (`aggregate_base`).
    Stack,
}

/// Compile a `match` as a chain of fail-blocks wrapped in a result-carrying
/// block. Each arm tests-and-binds its pattern, short-circuiting to its
/// fail-block on the first mismatch (so loads behind a non-matching
/// constructor are never executed), then runs its body and branches to `$done`.
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
    emit_expr(f, scrutinee, ctx)?;
    f.instruction(&Instruction::LocalSet(scrutinee_local));

    let produces = produces_value(result_ty);
    // `$done`: every matched arm branches here with its body's value.
    let done_level = ctx.ctrl_depth.get();
    if produces {
        f.instruction(&Instruction::Block(BlockType::Result(hir_type_to_valtype(
            result_ty,
        ))));
    } else {
        f.instruction(&Instruction::Block(BlockType::Empty));
    }
    ctx.enter_ctrl();

    // A consumed scrutinee transfers ownership into the arm; once its payload is
    // bound, the match owns the box(es) the arm did not move out and frees them.
    let consuming = hir::cfg::match_consumes(arms);
    for arm in arms {
        // `$fail`: a mismatch in this arm's test branches to its end, falling
        // through to the next arm.
        f.instruction(&Instruction::Block(BlockType::Empty));
        ctx.enter_ctrl();
        emit_test_bind(
            f,
            Src::Local(scrutinee_local),
            &scrutinee.ty,
            &arm.pattern,
            ctx,
        );
        if consuming {
            emit_consume_cleanup(f, scrutinee_local, &[], &scrutinee.ty, &arm.pattern, ctx);
        }
        emit_expr(f, &arm.body, ctx)?;
        let done_br = ctx.ctrl_depth.get() - 1 - done_level;
        f.instruction(&Instruction::Br(done_br));
        f.instruction(&Instruction::End);
        ctx.exit_ctrl();
    }

    // Exhaustiveness is checked at typecheck, so no arm matching is
    // unreachable at runtime; `Unreachable` also satisfies the block's result.
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    ctx.exit_ctrl();
    Ok(())
}

/// Push the value identified by `src` (of type `ty`) onto the stack.
fn push_src(f: &mut Function, src: Src, ty: &hir::Type) {
    match src {
        Src::Local(idx) => {
            f.instruction(&Instruction::LocalGet(idx));
        }
        Src::Field { base, offset } => {
            f.instruction(&Instruction::LocalGet(base));
            emit_field_load(f, ty, offset);
        }
        // Already on the operand stack — nothing to push. Valid only for a
        // single consuming use (a leaf binding); callers needing it repeatedly
        // route through `aggregate_base`.
        Src::Stack => {}
    }
}

/// Recursively test a value (`src`, of type `ty`) against `pattern`, branching
/// to the enclosing `$fail` block (relative depth 0 — no blocks are opened
/// here) on any mismatch, and binding any sub-bindings along the matching path.
fn emit_test_bind(
    f: &mut Function,
    src: Src,
    ty: &hir::Type,
    pattern: &hir::Pattern,
    ctx: &EmitCtx,
) {
    match pattern {
        hir::Pattern::Wildcard { .. } => {
            // A value left on the stack (a `let _ = expr`) must be dropped to
            // keep the stack balanced; one in a local/field is just ignored.
            if matches!(src, Src::Stack) && produces_value(ty) {
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Pattern::Binding { symbol, .. } => {
            if let Some(&local) = ctx.locals.get(symbol) {
                push_src(f, src, ty);
                f.instruction(&Instruction::LocalSet(local));
            } else if matches!(src, Src::Stack) && produces_value(ty) {
                // Unbound name fed from the stack: discard to stay balanced.
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Pattern::Int { value, ty: pty, .. } => {
            push_src(f, src, ty);
            if matches!(hir_type_to_valtype(pty), ValType::I64) {
                f.instruction(&Instruction::I64Const(*value));
                f.instruction(&Instruction::I64Ne);
            } else {
                f.instruction(&Instruction::I32Const(*value as i32));
                f.instruction(&Instruction::I32Ne);
            }
            f.instruction(&Instruction::BrIf(0));
        }
        hir::Pattern::Bool { value, .. } => {
            push_src(f, src, ty);
            f.instruction(&Instruction::I32Const(*value as i32));
            f.instruction(&Instruction::I32Ne);
            f.instruction(&Instruction::BrIf(0));
        }
        hir::Pattern::Tuple { elems, .. } => {
            let base = aggregate_base(f, src, ty, ctx);
            let elem_types: Vec<hir::Type> = match ty {
                hir::Type::Tuple(ts) => ts.clone(),
                _ => Vec::new(),
            };
            let layout = compute_tuple_layout(&elem_types);
            for (elem, (offset, elem_ty)) in elems.iter().zip(layout.elems.iter()) {
                emit_test_bind(
                    f,
                    Src::Field {
                        base,
                        offset: *offset,
                    },
                    elem_ty,
                    elem,
                    ctx,
                );
            }
        }
        hir::Pattern::Variant {
            variant_idx,
            fields,
            ..
        } => {
            // Use the enum id from the value's (monomorphized) type, not the
            // pattern's — mono rewrites types but not the pattern's `enum_id`.
            let enum_id = match ty {
                hir::Type::Enum(id, _) => *id,
                _ => {
                    bail(f, ctx, "variant pattern on non-enum type");
                    return;
                }
            };
            let base = aggregate_base(f, src, ty, ctx);
            // Discriminant test.
            f.instruction(&Instruction::LocalGet(base));
            emit_field_load(f, &hir::Type::U32, 0);
            f.instruction(&Instruction::I32Const(*variant_idx as i32));
            f.instruction(&Instruction::I32Ne);
            f.instruction(&Instruction::BrIf(0));

            let variant = ctx
                .enum_layouts
                .get(&enum_id)
                .and_then(|layout| layout.variants.get(*variant_idx as usize))
                .cloned();
            let Some(variant) = variant else {
                bail(f, ctx, "missing enum variant layout");
                return;
            };
            for fp in fields {
                let Some((payload_offset, field_ty)) = variant.fields.get(&fp.field).cloned()
                else {
                    bail(f, ctx, "variant field not found in layout");
                    continue;
                };
                emit_test_bind(
                    f,
                    Src::Field {
                        base,
                        offset: 8 + payload_offset,
                    },
                    &field_ty,
                    &fp.pattern,
                    ctx,
                );
            }
        }
        hir::Pattern::Struct { fields, .. } => {
            // Irrefutable: no discriminant test, just bind each named field from
            // its offset. The struct id comes from the value's (monomorphized)
            // type, not the pattern's.
            let sid = match ty {
                hir::Type::Struct(id, _) => *id,
                _ => {
                    bail(f, ctx, "struct pattern on non-struct type");
                    return;
                }
            };
            let base = aggregate_base(f, src, ty, ctx);
            let Some(layout) = ctx.struct_layouts.get(&sid).cloned() else {
                bail(f, ctx, "missing struct layout");
                return;
            };
            for fp in fields {
                let Some((offset, field_ty)) = layout.fields.get(&fp.field).cloned() else {
                    bail(f, ctx, "field not found in struct layout");
                    continue;
                };
                emit_test_bind(f, Src::Field { base, offset }, &field_ty, &fp.pattern, ctx);
            }
        }
    }
}

/// Produce a local holding an aggregate value's pointer for repeated access:
/// reuse the local directly when `src` already is one, else stash the loaded
/// pointer in a fresh scratch local. (Mirrored by `walks::collect_match_arm_temps`.)
fn aggregate_base(f: &mut Function, src: Src, ty: &hir::Type, ctx: &EmitCtx) -> u32 {
    match src {
        Src::Local(idx) => idx,
        // A field load or the stack-top value: stash it in a scratch local so the
        // pointer can be reread for each sub-field. (`push_src` is a no-op for
        // `Stack`, so this just `LocalSet`s the value already on the stack.)
        Src::Field { .. } | Src::Stack => {
            let counter = ctx.scratch_counter.get();
            ctx.scratch_counter.set(counter + 1);
            let tmp = ctx.scratch_base + counter;
            push_src(f, src, ty);
            f.instruction(&Instruction::LocalSet(tmp));
            tmp
        }
    }
}

/// Free the heap box(es) of a consumed match value `base` (a local holding the
/// box pointer), reached by `path` — a chain of field byte-offsets from `base`.
/// Run after the arm bound the fields it took: any nested destructure recurses
/// (freeing its own box first), and this value's box is freed last. Bound leaves
/// are dropped by drop elaboration, so they are skipped here. Pointers are
/// re-derived from `base` each time, so no scratch locals are needed.
///
/// Un-taken (wildcard/omitted) needs-drop fields, and the live payload behind a
/// wildcard arm over an enum, are not dropped yet — the same recursive-drop gap
/// documented for enum payloads, to be closed by the synthesized-drop follow-up.
fn emit_consume_cleanup(
    f: &mut Function,
    base: u32,
    path: &[u32],
    ty: &hir::Type,
    pattern: &hir::Pattern,
    ctx: &EmitCtx,
) {
    // A whole-value binding moved ownership into the binding; its own drop frees
    // the box. Nothing to free here.
    if matches!(pattern, hir::Pattern::Binding { .. }) {
        return;
    }
    match (ty, pattern) {
        (
            hir::Type::Enum(eid, _),
            hir::Pattern::Variant {
                variant_idx,
                fields,
                ..
            },
        ) => {
            if let Some(variant) = ctx
                .enum_layouts
                .get(eid)
                .and_then(|l| l.variants.get(*variant_idx as usize))
            {
                for fp in fields {
                    if is_destructure(&fp.pattern) {
                        if let Some((payload_offset, field_ty)) = variant.fields.get(&fp.field) {
                            let mut child = path.to_vec();
                            child.push(8 + *payload_offset);
                            emit_consume_cleanup(f, base, &child, field_ty, &fp.pattern, ctx);
                        }
                    }
                }
            }
        }
        (hir::Type::Struct(sid, _), hir::Pattern::Struct { fields, .. }) => {
            if let Some(layout) = ctx.struct_layouts.get(sid) {
                for fp in fields {
                    if is_destructure(&fp.pattern) {
                        if let Some((offset, field_ty)) = layout.fields.get(&fp.field) {
                            let mut child = path.to_vec();
                            child.push(*offset);
                            emit_consume_cleanup(f, base, &child, field_ty, &fp.pattern, ctx);
                        }
                    }
                }
            }
        }
        (hir::Type::Tuple(ts), hir::Pattern::Tuple { elems, .. }) => {
            let layout = compute_tuple_layout(ts);
            for (elem, (offset, elem_ty)) in elems.iter().zip(layout.elems.iter()) {
                if is_destructure(elem) {
                    let mut child = path.to_vec();
                    child.push(*offset);
                    emit_consume_cleanup(f, base, &child, elem_ty, elem, ctx);
                }
            }
        }
        _ => {}
    }
    // Free this value's own box, after its fields were read.
    push_box_path(f, base, path);
    f.instruction(&Instruction::Call(ctx.builtins.free));
}

/// Push the box pointer reached from local `base` by following `path` (a chain
/// of field byte-offsets, each an i32 pointer load). An empty `path` pushes
/// `base` itself.
fn push_box_path(f: &mut Function, base: u32, path: &[u32]) {
    f.instruction(&Instruction::LocalGet(base));
    for &offset in path {
        emit_field_load(f, &hir::Type::U32, offset);
    }
}

/// Whether a pattern destructures an aggregate in place (vs. binding/ignoring
/// the whole value) — the patterns whose own box must be freed on consume.
fn is_destructure(pattern: &hir::Pattern) -> bool {
    matches!(
        pattern,
        hir::Pattern::Variant { .. } | hir::Pattern::Struct { .. } | hir::Pattern::Tuple { .. }
    )
}

/// Emit RAII drop of owned local `sym` of type `ty`: a call to the type's
/// synthesized `drop_T` function, which runs the value's own `Drop::drop`,
/// recursively drops its owned fields, and frees its box.
fn emit_drop(f: &mut Function, sym: hir::SymbolId, ty: &hir::Type, ctx: &EmitCtx) {
    let Some(&local) = ctx.locals.get(&sym) else {
        return;
    };
    if let Some(&drop_fn) = ctx.drop_fns.get(ty) {
        f.instruction(&Instruction::LocalGet(local));
        f.instruction(&Instruction::Call(drop_fn));
    }
}

/// Collect every concrete type that needs a synthesized drop function: the
/// types dropped at `Stmt::Drop` sites, transitively closed over the owned
/// fields a drop recurses into. Returned in a deterministic order so the
/// assigned wasm indices are stable.
pub(crate) fn collect_drop_types(program: &hir::Program, info: &hir::DropInfo) -> Vec<hir::Type> {
    let mut work: Vec<hir::Type> = Vec::new();
    for func in &program.functions {
        collect_drop_sites(&func.body, &mut work);
    }
    work.reverse(); // process in source order despite the LIFO worklist
    let mut seen: HashSet<hir::Type> = HashSet::new();
    let mut order: Vec<hir::Type> = Vec::new();
    while let Some(ty) = work.pop() {
        if !info.needs_drop(&ty) || !seen.insert(ty.clone()) {
            continue;
        }
        order.push(ty.clone());
        // Enqueue the owned field types this drop will recurse into.
        for (_, fty) in recursable_fields(&ty, program) {
            work.push(fty);
        }
    }
    order
}

/// Synthesize the body of `drop_T(ptr: i32)`: run `T`'s own `Drop::drop` (if
/// any) while its fields are still valid, then recursively drop each owned
/// field, then free `T`'s box. Each box is freed exactly once, by its own
/// `drop_T`.
pub(crate) fn emit_drop_fn(
    ty: &hir::Type,
    drop_fns: &HashMap<hir::Type, u32>,
    func_map: &HashMap<hir::FuncId, u32>,
    program: &hir::Program,
    info: &hir::DropInfo,
    free_idx: u32,
) -> Function {
    let mut f = Function::new(vec![]); // param 0 is the box pointer
    if let Some(drop_method) = info.drop_method(ty) {
        if let Some(&widx) = func_map.get(&drop_method) {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::Call(widx));
        }
    }
    for (offset, fty) in recursable_fields(ty, program) {
        // Present only for needs-drop field types; scalars are skipped.
        if let Some(&didx) = drop_fns.get(&fty) {
            f.instruction(&Instruction::LocalGet(0));
            emit_field_load(&mut f, &fty, offset); // load the field's box pointer
            f.instruction(&Instruction::Call(didx));
        }
    }
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(free_idx));
    f.instruction(&Instruction::End);
    f
}

/// The owned fields a `drop_T` recurses into, as `(byte offset, type)` in
/// declaration order. Structs and tuples have a static field list; enums and
/// arrays are not yet recursed (their payloads/elements are left to a
/// follow-up), so they contribute none.
fn recursable_fields(ty: &hir::Type, program: &hir::Program) -> Vec<(u32, hir::Type)> {
    match ty {
        hir::Type::Struct(sid, _) => match program.structs.get(sid.0 as usize) {
            Some(s) => {
                let layout = compute_struct_layout(s);
                s.fields
                    .iter()
                    .filter_map(|field| {
                        layout
                            .fields
                            .get(&field.name)
                            .map(|(off, fty)| (*off, fty.clone()))
                    })
                    .collect()
            }
            None => Vec::new(),
        },
        hir::Type::Tuple(elems) => compute_tuple_layout(elems).elems,
        _ => Vec::new(),
    }
}

/// Walk a body collecting the type of every `Stmt::Drop` it contains.
fn collect_drop_sites(block: &hir::Block, out: &mut Vec<hir::Type>) {
    for stmt in &block.stmts {
        match stmt {
            hir::Stmt::Drop { ty, .. } => out.push(ty.clone()),
            hir::Stmt::Loop { body, .. } | hir::Stmt::While { body, .. } => {
                collect_drop_sites(body, out)
            }
            hir::Stmt::Let { value, .. }
            | hir::Stmt::Assign { value, .. }
            | hir::Stmt::Return {
                value: Some(value), ..
            }
            | hir::Stmt::Expr(value) => collect_drop_sites_expr(value, out),
            hir::Stmt::DerefAssign { ptr, value, .. } => {
                collect_drop_sites_expr(ptr, out);
                collect_drop_sites_expr(value, out);
            }
            hir::Stmt::FieldAssign { object, value, .. } => {
                collect_drop_sites_expr(object, out);
                collect_drop_sites_expr(value, out);
            }
            hir::Stmt::Return { value: None, .. } | hir::Stmt::Break { .. } => {}
        }
    }
}

/// Recurse into the blocks an expression can contain (`if`/`match`/block) to
/// reach nested `Stmt::Drop`s.
fn collect_drop_sites_expr(expr: &hir::Expr, out: &mut Vec<hir::Type>) {
    match &expr.kind {
        hir::ExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            collect_drop_sites(then_branch, out);
            if let Some(b) = else_branch {
                collect_drop_sites(b, out);
            }
        }
        hir::ExprKind::Block(b) => collect_drop_sites(b, out),
        hir::ExprKind::Match { arms, .. } => {
            for arm in arms {
                collect_drop_sites_expr(&arm.body, out);
            }
        }
        _ => {}
    }
}

fn emit_runtime_call(
    f: &mut Function,
    runtime: hir::RuntimeAbi,
    args: &[hir::Expr],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    match runtime {
        // write(fd, s: String) — ignore fd for now (always stdout); load
        // s.data + s.len from the String struct, hand to __write_bytes.
        hir::RuntimeAbi::Write => {
            emit_write(f, args, ctx)?;
        }
        // now_nanos() -> u64: clock_time_get(CLOCK_MONOTONIC=1, precision=0,
        // out=CLOCK_SCRATCH); discard errno; load the u64 timestamp.
        hir::RuntimeAbi::ClockNow => {
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I64Const(0));
            f.instruction(&Instruction::I32Const(CLOCK_SCRATCH));
            f.instruction(&Instruction::Call(ctx.builtins.clock));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(CLOCK_SCRATCH));
            f.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        }
        // read_raw(fd, ptr, cap) -> nread: build a one-element iovec at the
        // shared scratch (buf@0, buf_len@4) and call fd_read; the host writes
        // the count to offset 8, which becomes the result.
        hir::RuntimeAbi::Read => {
            let store32 = MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            };
            // iovec.buf = ptr
            f.instruction(&Instruction::I32Const(0));
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Store(store32));
            // iovec.buf_len = cap
            f.instruction(&Instruction::I32Const(4));
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::I32Store(store32));
            // fd_read(fd, iovs=0, iovs_len=1, nread=8); discard errno.
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Const(8));
            f.instruction(&Instruction::Call(ctx.builtins.fd_read));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(8));
            f.instruction(&Instruction::I32Load(store32));
        }
        // poll(subs, events, nsubs) -> nevents: the scheduler has already laid
        // out the subscription structs; call poll_oneoff and return the count
        // the host writes to POLL_NEVENTS.
        hir::RuntimeAbi::Poll => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::I32Const(POLL_NEVENTS));
            f.instruction(&Instruction::Call(ctx.builtins.poll_oneoff));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(POLL_NEVENTS));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
        }
        // Cooperative yield — suspend with the scheduler's yield tag.
        // Control returns to the scheduler's `on $yield` handler in `_start`,
        // which reschedules immediately (single-task case) or picks another
        // runnable continuation (future, when a queue exists).
        hir::RuntimeAbi::Trap => {
            f.instruction(&Instruction::Unreachable);
        }
        hir::RuntimeAbi::Yield => {
            f.instruction(&Instruction::Suspend(ctx.builtins.yield_tag));
        }
        // resume(handle) -> bool: delegate to the __rt_resume helper.
        hir::RuntimeAbi::Resume => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.rt_resume));
        }
        // task_count() -> usize: number of slots in the task table.
        hir::RuntimeAbi::TaskCount => {
            f.instruction(&Instruction::TableSize(ctx.builtins.cont_table));
        }
        // task_live(handle) -> bool: the slot still holds a continuation.
        hir::RuntimeAbi::TaskLive => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::TableGet(ctx.builtins.cont_table));
            f.instruction(&Instruction::RefIsNull);
            f.instruction(&Instruction::I32Eqz);
        }
        // `spawn` is rewritten to ExprKind::Spawn during lowering, so a call
        // through the runtime ABI never reaches codegen.
        hir::RuntimeAbi::Spawn => {
            unreachable!("spawn is lowered to ExprKind::Spawn in hir_builder");
        }
        // spawn_main(): seed the program's main as a task. Same shape as
        // ExprKind::Spawn but the target is `main`, known to the compiler.
        hir::RuntimeAbi::SpawnMain => {
            f.instruction(&Instruction::RefFunc(ctx.builtins.main_func));
            f.instruction(&Instruction::ContNew(ctx.builtins.cont_type));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::TableGrow(ctx.builtins.cont_table));
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
        // `size_of[T]()` is folded to a constant during monomorphization, so
        // no call to it ever reaches codegen.
        hir::RuntimeAbi::SizeOf => unreachable!("size_of is folded in monomorphization"),
        // Generic `*mut T` primitives. A pointer is an i32 address, so these
        // are type-independent; element scaling happens in Prim via size_of.
        hir::RuntimeAbi::Null => {
            f.instruction(&Instruction::I32Const(0));
        }
        hir::RuntimeAbi::PtrByteAdd | hir::RuntimeAbi::PtrByteOffset | hir::RuntimeAbi::At => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrByteSub => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrAddr | hir::RuntimeAbi::FromAddr => {
            emit_expr(f, &args[0], ctx)?;
        }
        // Float <-> integer conversions (std.convert). Float-to-integer
        // truncates toward zero and saturates (no trap on overflow/NaN);
        // integer-to-float rounds to nearest; f32->f64 is an exact widen.
        hir::RuntimeAbi::F64ToU64Trunc => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64TruncSatF64U);
        }
        hir::RuntimeAbi::U64ToF64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::F64ConvertI64U);
        }
        hir::RuntimeAbi::F32ToF64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::F64PromoteF32);
        }
        // Integer conversions (std.convert). A `u8`/`i32`/… all share the
        // wasm `i32` representation, so most narrowings are a mask or a
        // sign-extend and most widenings are a no-op or an i64 extend.
        hir::RuntimeAbi::ConvNoop => {
            emit_expr(f, &args[0], ctx)?;
        }
        hir::RuntimeAbi::ConvTruncU8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Const(0xFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvTruncU16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Const(0xFFFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvSext8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Extend8S);
        }
        hir::RuntimeAbi::ConvSext16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Extend16S);
        }
        hir::RuntimeAbi::ConvExtI32S => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32S);
        }
        hir::RuntimeAbi::ConvExtI32U => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32U);
        }
        hir::RuntimeAbi::ConvWrapI64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
        }
        hir::RuntimeAbi::ConvWrapTruncU8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Const(0xFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvWrapTruncU16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Const(0xFFFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvWrapSext8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Extend8S);
        }
        hir::RuntimeAbi::ConvWrapSext16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Extend16S);
        }
    }
    Ok(())
}

fn emit_write(f: &mut Function, args: &[hir::Expr], ctx: &EmitCtx) -> Result<(), WasmError> {
    if args.len() < 3 {
        return Err(WasmError::Internal(
            "write_raw expects three arguments".into(),
        ));
    }

    // write_raw(fd, ptr, len) -> nwritten: evaluate the three args in order
    // and call __write_bytes, which leaves the written-byte count on the stack
    // (the primitive's return value). Byte-oriented — no String/Vec knowledge.
    emit_expr(f, &args[0], ctx)?; // fd:  i32
    emit_expr(f, &args[1], ctx)?; // ptr: *mut u8 (i32)
    emit_expr(f, &args[2], ctx)?; // len: usize (i32)
    f.instruction(&Instruction::Call(ctx.builtins.write_bytes));
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
            _ => unreachable!("add on unsupported operand type"),
        },
        hir::BinaryOp::Subtract => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Sub),
            ValType::I64 => f.instruction(&Instruction::I64Sub),
            ValType::F32 => f.instruction(&Instruction::F32Sub),
            ValType::F64 => f.instruction(&Instruction::F64Sub),
            _ => unreachable!("subtract on unsupported operand type"),
        },
        hir::BinaryOp::Multiply => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Mul),
            ValType::I64 => f.instruction(&Instruction::I64Mul),
            ValType::F32 => f.instruction(&Instruction::F32Mul),
            ValType::F64 => f.instruction(&Instruction::F64Mul),
            _ => unreachable!("multiply on unsupported operand type"),
        },
        hir::BinaryOp::Divide => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32DivS),
            (ValType::I32, false) => f.instruction(&Instruction::I32DivU),
            (ValType::I64, true) => f.instruction(&Instruction::I64DivS),
            (ValType::I64, false) => f.instruction(&Instruction::I64DivU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Div),
            (ValType::F64, _) => f.instruction(&Instruction::F64Div),
            _ => unreachable!("divide on unsupported operand type"),
        },
        hir::BinaryOp::Modulo => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32RemS),
            (ValType::I32, false) => f.instruction(&Instruction::I32RemU),
            (ValType::I64, true) => f.instruction(&Instruction::I64RemS),
            (ValType::I64, false) => f.instruction(&Instruction::I64RemU),
            _ => unreachable!("modulo on unsupported operand type"),
        },
        hir::BinaryOp::Equals => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Eq),
            ValType::I64 => f.instruction(&Instruction::I64Eq),
            ValType::F32 => f.instruction(&Instruction::F32Eq),
            ValType::F64 => f.instruction(&Instruction::F64Eq),
            _ => unreachable!("equals on unsupported operand type"),
        },
        hir::BinaryOp::NotEquals => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Ne),
            ValType::I64 => f.instruction(&Instruction::I64Ne),
            ValType::F32 => f.instruction(&Instruction::F32Ne),
            ValType::F64 => f.instruction(&Instruction::F64Ne),
            _ => unreachable!("notequals on unsupported operand type"),
        },
        hir::BinaryOp::Greater => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32GtS),
            (ValType::I32, false) => f.instruction(&Instruction::I32GtU),
            (ValType::I64, true) => f.instruction(&Instruction::I64GtS),
            (ValType::I64, false) => f.instruction(&Instruction::I64GtU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Gt),
            (ValType::F64, _) => f.instruction(&Instruction::F64Gt),
            _ => unreachable!("greater on unsupported operand type"),
        },
        hir::BinaryOp::GreaterEquals => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32GeS),
            (ValType::I32, false) => f.instruction(&Instruction::I32GeU),
            (ValType::I64, true) => f.instruction(&Instruction::I64GeS),
            (ValType::I64, false) => f.instruction(&Instruction::I64GeU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Ge),
            (ValType::F64, _) => f.instruction(&Instruction::F64Ge),
            _ => unreachable!("greaterequals on unsupported operand type"),
        },
        hir::BinaryOp::Less => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32LtS),
            (ValType::I32, false) => f.instruction(&Instruction::I32LtU),
            (ValType::I64, true) => f.instruction(&Instruction::I64LtS),
            (ValType::I64, false) => f.instruction(&Instruction::I64LtU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Lt),
            (ValType::F64, _) => f.instruction(&Instruction::F64Lt),
            _ => unreachable!("less on unsupported operand type"),
        },
        hir::BinaryOp::LessEquals => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32LeS),
            (ValType::I32, false) => f.instruction(&Instruction::I32LeU),
            (ValType::I64, true) => f.instruction(&Instruction::I64LeS),
            (ValType::I64, false) => f.instruction(&Instruction::I64LeU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Le),
            (ValType::F64, _) => f.instruction(&Instruction::F64Le),
            _ => unreachable!("lessequals on unsupported operand type"),
        },
        hir::BinaryOp::BitAnd => match vt {
            ValType::I32 => f.instruction(&Instruction::I32And),
            ValType::I64 => f.instruction(&Instruction::I64And),
            _ => unreachable!("bitand on unsupported operand type"),
        },
        hir::BinaryOp::BitOr => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Or),
            ValType::I64 => f.instruction(&Instruction::I64Or),
            _ => unreachable!("bitor on unsupported operand type"),
        },
        hir::BinaryOp::BitXor => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Xor),
            ValType::I64 => f.instruction(&Instruction::I64Xor),
            _ => unreachable!("bitxor on unsupported operand type"),
        },
        hir::BinaryOp::ShiftLeft => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Shl),
            ValType::I64 => f.instruction(&Instruction::I64Shl),
            _ => unreachable!("shiftleft on unsupported operand type"),
        },
        hir::BinaryOp::ShiftRight => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32ShrS),
            (ValType::I32, false) => f.instruction(&Instruction::I32ShrU),
            (ValType::I64, true) => f.instruction(&Instruction::I64ShrS),
            (ValType::I64, false) => f.instruction(&Instruction::I64ShrU),
            _ => unreachable!("shiftright on unsupported operand type"),
        },
    };
}
