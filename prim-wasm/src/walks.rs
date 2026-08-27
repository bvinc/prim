//! Pre-walks over HIR function bodies.
//!
//! Each walk collects something the emission pass needs to know up-front
//! (before it starts producing wasm instructions). All walks are pre-order
//! and visit the same nodes in the same sequence as `emit::emit_expr`, so
//! per-node counters in `EmitCtx` line up with the collected lists.

use crate::emit::{
    ScalarField, is_destructure, is_inline_field_read, needs_value_copy, store_orphans_box,
};
use crate::layout::{compute_enum_layout, compute_struct_layout, compute_tuple_layout};
use crate::types::hir_type_to_valtype;
use prim_compiler::hir;
use prim_compiler::hir::inline::InlinePolicy;
use std::collections::HashMap;
use wasm_encoder::ValType;

// === Local variables ===

/// Collect every `Let`-bound or `Param`-introduced local in the function,
/// in declaration order, paired with its wasm `ValType`.
pub(crate) fn collect_locals(block: &hir::Block) -> Vec<(hir::SymbolId, ValType)> {
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
        hir::Stmt::Let { pattern, value, .. } => {
            collect_locals_pattern(pattern, locals);
            collect_locals_expr(value, locals);
        }
        hir::Stmt::Assign { value, .. } => collect_locals_expr(value, locals),
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_locals_expr(ptr, locals);
            collect_locals_expr(value, locals);
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            collect_locals_expr(object, locals);
            collect_locals_expr(value, locals);
        }
        hir::Stmt::Expr(e) => collect_locals_expr(e, locals),
        hir::Stmt::Loop { body, .. } => collect_locals_block(body, locals),
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_block(body, locals);
        }
        hir::Stmt::Break { .. } => {}
        hir::Stmt::Return { value, .. } => {
            if let Some(v) = value {
                collect_locals_expr(v, locals);
            }
        }
        hir::Stmt::Drop { .. } => {}
    }
}

/// Push a local for every binding a pattern introduces, recursively.
fn collect_locals_pattern(pattern: &hir::Pattern, locals: &mut Vec<(hir::SymbolId, ValType)>) {
    match pattern {
        hir::Pattern::Wildcard { .. } | hir::Pattern::Int { .. } | hir::Pattern::Bool { .. } => {}
        hir::Pattern::Binding { symbol, ty, .. } => {
            locals.push((*symbol, hir_type_to_valtype(ty)));
        }
        hir::Pattern::Tuple { elems, .. } => {
            for elem in elems {
                collect_locals_pattern(elem, locals);
            }
        }
        hir::Pattern::Variant { fields, .. } | hir::Pattern::Struct { fields, .. } => {
            for fp in fields {
                collect_locals_pattern(&fp.pattern, locals);
            }
        }
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
        hir::ExprKind::Block(block) | hir::ExprKind::UnsafeBlock(block) => {
            collect_locals_block(block, locals)
        }
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
        hir::ExprKind::TupleLit(elems) => {
            for e in elems {
                collect_locals_expr(e, locals);
            }
        }
        hir::ExprKind::TupleIndex { base, .. } => collect_locals_expr(base, locals),
        hir::ExprKind::VariantLit { fields, .. } => {
            for (_, val) in fields {
                collect_locals_expr(val, locals);
            }
        }
        hir::ExprKind::Match {
            mode: _,
            scrutinee,
            arms,
        } => {
            collect_locals_expr(scrutinee, locals);
            for arm in arms {
                collect_locals_pattern(&arm.pattern, locals);
                collect_locals_expr(&arm.body, locals);
            }
        }
        hir::ExprKind::Field { base, .. } => collect_locals_expr(base, locals),
        hir::ExprKind::Deref(base) => collect_locals_expr(base, locals),
        hir::ExprKind::Neg(operand) | hir::ExprKind::BitNot(operand) => {
            collect_locals_expr(operand, locals)
        }
        hir::ExprKind::Coerce { value, .. } => collect_locals_expr(value, locals),
        hir::ExprKind::DynCall { receiver, args, .. } => {
            collect_locals_expr(receiver, locals);
            for a in args {
                collect_locals_expr(a, locals);
            }
        }
        hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            collect_locals_expr(receiver, locals);
            for a in args {
                collect_locals_expr(a, locals);
            }
        }
        hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_locals_expr(e, locals);
            }
        }
        hir::ExprKind::IndirectCall { callee, args } => {
            collect_locals_expr(callee, locals);
            for a in args {
                collect_locals_expr(a, locals);
            }
        }
        _ => {}
    }
}

// === Scratch local types ===
//
// `StructLit`, `Dbg`, `Str`, and runtime `write(...)` calls each need an
// extra wasm local to hold an intermediate pointer or value. This walk
// returns one `ValType` per such site, in the same pre-order that emission
// uses to consume them.

/// Reserve only a call's argument scratch (no scalar-ABI return materialize),
/// mirroring `emit::emit_raw_call`.
fn call_arg_scratch(
    func: &hir::FuncId,
    args: &[hir::Expr],
    arg_modes: &[hir::PassMode],
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    let abi = scalar_abi.get(func);
    for (i, a) in args.iter().enumerate() {
        if abi.is_some_and(|v| v.get(i).copied().unwrap_or(false)) {
            scalar_value_scratch(a, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out);
        } else {
            collect_scratch_types_expr(a, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out);
            // An `own` argument that is a read of an inline-aggregate field is
            // boxed out by `emit_move_value` (two scratch locals, src/dst).
            if arg_modes.get(i).copied() == Some(hir::PassMode::Own)
                && is_inline_field_read(a, policy)
            {
                out.extend([ValType::I32, ValType::I32]);
            }
        }
    }
}

/// Reserve the scratch that `emit::emit_scalar_value` consumes for `e` (a
/// by-value call argument or return): none for a name (it pushes field locals or
/// loads via an existing local), the literal's own box for a struct/tuple
/// literal, a scalar-return call's argument scratch (passed through, no box),
/// else the value's own scratch plus one stash pointer.
fn scalar_value_scratch(
    e: &hir::Expr,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    match &e.kind {
        hir::ExprKind::Ident(_) => {}
        hir::ExprKind::StructLit { .. } | hir::ExprKind::TupleLit(_) => {
            collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out);
        }
        // A scalar-ABI-returning call is passed straight through (no box).
        hir::ExprKind::Call { func, args, .. } if scalar_ret.contains_key(func) => {
            call_arg_scratch(
                func,
                args,
                &[],
                runtime,
                scalar_abi,
                scalar_ret,
                ret_scalar,
                policy,
                out,
            );
        }
        _ => {
            collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out);
            out.push(ValType::I32);
        }
    }
}

/// Collect scratch types for a whole function body. The trailing expression is
/// the implicit return value, so for a scalar-ABI return it reserves the same
/// scratch as `emit_scalar_value` (mirroring an explicit `return`). Inner-block
/// trailing expressions are ordinary block values and stay in
/// `collect_scratch_types_block`.
pub(crate) fn collect_scratch_types_body(
    body: &hir::Block,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    for stmt in &body.stmts {
        collect_scratch_types_stmt(
            stmt, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
        );
    }
    if let Some(expr) = &body.expr {
        if ret_scalar {
            scalar_value_scratch(
                expr, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        } else {
            collect_scratch_types_expr(
                expr, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
    }
}

fn collect_scratch_types_block(
    block: &hir::Block,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    for stmt in &block.stmts {
        collect_scratch_types_stmt(
            stmt, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
        );
    }
    if let Some(expr) = &block.expr {
        collect_scratch_types_expr(
            expr, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
        );
    }
}

fn collect_scratch_types_stmt(
    stmt: &hir::Stmt,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    match stmt {
        hir::Stmt::Assign { value, .. } => {
            collect_scratch_types_expr(
                value, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            // `emit_copy_value` claims two scratch locals (src/dst) to deep-copy
            // a `Copy` aggregate or an inline-aggregate field read (value
            // semantics for `a = b`).
            if needs_value_copy(value, policy) {
                out.extend([ValType::I32, ValType::I32]);
            }
        }
        // The value is produced first, then the pattern binds it (reserving a
        // scratch pointer per tuple level).
        hir::Stmt::Let { pattern, value, .. } => {
            collect_scratch_types_expr(
                value, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            // A simple binding of a `Copy` aggregate or an inline-aggregate
            // field deep-copies its value (`emit_copy_value`); two scratch
            // locals (src/dst).
            if matches!(pattern, hir::Pattern::Binding { .. }) && needs_value_copy(value, policy) {
                out.extend([ValType::I32, ValType::I32]);
            }
            collect_scratch_types_pattern(pattern, policy, out);
        }
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_scratch_types_expr(
                ptr, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            collect_scratch_types_expr(
                value, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            // A whole moved box stored into an inline slot is stashed (one i32)
            // so `emit` can free it after the byte copy.
            if store_orphans_box(value, policy) {
                out.push(ValType::I32);
            }
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            collect_scratch_types_expr(
                object, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            collect_scratch_types_expr(
                value, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            // Same whole-box stash as `DerefAssign`, for an inline field store.
            if store_orphans_box(value, policy) {
                out.push(ValType::I32);
            }
        }
        hir::Stmt::Expr(e) => {
            collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out)
        }
        hir::Stmt::Loop { body, .. } => collect_scratch_types_block(
            body, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
        ),
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_scratch_types_expr(
                condition, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            collect_scratch_types_block(
                body, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::Stmt::Break { .. } => {}
        hir::Stmt::Return { value, .. } => {
            if let Some(v) = value {
                if ret_scalar {
                    scalar_value_scratch(
                        v, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                    );
                } else {
                    collect_scratch_types_expr(
                        v, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                    );
                    // `emit_move_value` boxes out an inline-aggregate field
                    // read (two scratch locals, src/dst).
                    if is_inline_field_read(v, policy) {
                        out.extend([ValType::I32, ValType::I32]);
                    }
                }
            }
        }
        hir::Stmt::Drop { .. } => {}
    }
}

/// Reserve scratch slots an irrefutable (`let`) pattern's binding needs: one
/// i32 pointer per tuple/struct level (to stash the base while elements are
/// extracted). Mirrors `emit::emit_test_bind` fed from `Src::Stack`, where the
/// root aggregate is stashed via `aggregate_base` just like a nested one.
fn collect_scratch_types_pattern(
    pattern: &hir::Pattern,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    match pattern {
        hir::Pattern::Wildcard { .. } | hir::Pattern::Int { .. } | hir::Pattern::Bool { .. } => {}
        // A nested field binding of an inline aggregate in a `let` takes
        // ownership, so it deep-copies its inline bytes (`emit_field_copy_out`):
        // two scratch locals (addr/dst).
        hir::Pattern::Binding { ty, .. } => {
            if policy.is_inline(ty) {
                out.extend([ValType::I32, ValType::I32]);
            }
        }
        hir::Pattern::Tuple { elems, .. } => {
            out.push(ValType::I32);
            for elem in elems {
                collect_scratch_types_pattern(elem, policy, out);
            }
        }
        // A struct destructure stashes its base pointer (one i32), like a tuple.
        hir::Pattern::Struct { fields, .. } => {
            out.push(ValType::I32);
            for fp in fields {
                collect_scratch_types_pattern(&fp.pattern, policy, out);
            }
        }
        hir::Pattern::Variant { fields, .. } => {
            for fp in fields {
                collect_scratch_types_pattern(&fp.pattern, policy, out);
            }
        }
    }
}

/// Reserve the scratch temps a `match` arm's test-and-bind needs: one i32 per
/// *non-root* aggregate (tuple/variant) node, to stash its pointer for repeated
/// field access, plus two i32 per nested `Copy`-aggregate field binding (its
/// box-out deep copy). Mirrors `emit::emit_test_bind` / `emit::aggregate_base`
/// (the root value lives in the scrutinee local, so it needs no temp).
fn collect_match_arm_temps(pattern: &hir::Pattern, policy: &InlinePolicy, out: &mut Vec<ValType>) {
    fn child(pattern: &hir::Pattern, policy: &InlinePolicy, out: &mut Vec<ValType>) {
        match pattern {
            hir::Pattern::Binding { ty, mode, .. } => {
                // A match field binding deep-copies only when it takes ownership:
                // a `Copy` value, or an explicit `own` binding. Bare/`read`/`mut`
                // borrow the field and need no box-out.
                if policy.is_inline(ty) && (policy.is_copy(ty) || *mode == hir::PassMode::Own) {
                    out.extend([ValType::I32, ValType::I32]);
                }
            }
            hir::Pattern::Tuple { elems, .. } => {
                out.push(ValType::I32);
                for elem in elems {
                    child(elem, policy, out);
                }
            }
            hir::Pattern::Variant { fields, .. } | hir::Pattern::Struct { fields, .. } => {
                out.push(ValType::I32);
                for fp in fields {
                    child(&fp.pattern, policy, out);
                }
            }
            _ => {}
        }
    }
    match pattern {
        hir::Pattern::Tuple { elems, .. } => {
            for elem in elems {
                child(elem, policy, out);
            }
        }
        hir::Pattern::Variant { fields, .. } | hir::Pattern::Struct { fields, .. } => {
            for fp in fields {
                child(&fp.pattern, policy, out);
            }
        }
        _ => {}
    }
}

/// Reserve the scratch `emit::emit_consume_cleanup` consumes after an arm body:
/// one i32 per *boxed* (non-inline) field that the pattern destructures in
/// place, to stash the loaded box pointer as the recursion's new base. Mirrors
/// `emit::cleanup_field`'s boxed branch (an inline destructure adds only a
/// byte-offset path step and needs no scratch; a wildcard/omitted field's
/// drop in place needs none either).
fn collect_consume_cleanup_scratch(
    ty: &hir::Type,
    pattern: &hir::Pattern,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
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
            let Some(e) = policy.program().enums.get(eid.0 as usize) else {
                return;
            };
            let layout = compute_enum_layout(e, policy);
            let Some(variant) = layout.variants.get(*variant_idx as usize) else {
                return;
            };
            for (name, (_, field_ty)) in &variant.fields {
                let sub = fields
                    .iter()
                    .find(|fp| fp.field == *name)
                    .map(|fp| &fp.pattern);
                collect_cleanup_field_scratch(field_ty, sub, policy, out);
            }
        }
        (hir::Type::Struct(sid, _), hir::Pattern::Struct { fields, .. }) => {
            let Some(s) = policy.program().structs.get(sid.0 as usize) else {
                return;
            };
            let layout = compute_struct_layout(s, policy);
            for (name, (_, field_ty)) in &layout.fields {
                let sub = fields
                    .iter()
                    .find(|fp| fp.field == *name)
                    .map(|fp| &fp.pattern);
                collect_cleanup_field_scratch(field_ty, sub, policy, out);
            }
        }
        (hir::Type::Tuple(ts), hir::Pattern::Tuple { elems, .. }) => {
            let layout = compute_tuple_layout(ts, policy);
            for (i, (_, elem_ty)) in layout.elems.iter().enumerate() {
                let sub = elems.get(i);
                collect_cleanup_field_scratch(elem_ty, sub, policy, out);
            }
        }
        _ => {}
    }
}

fn collect_cleanup_field_scratch(
    field_ty: &hir::Type,
    sub: Option<&hir::Pattern>,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    match sub {
        // Taken whole: the binding owns it and drops it — no cleanup scratch.
        Some(hir::Pattern::Binding { .. }) => {}
        // Destructured in place.
        Some(p) if is_destructure(p) => {
            if policy.is_inline(field_ty) {
                // Inline: recurse with an extended byte-offset path (no scratch).
                collect_consume_cleanup_scratch(field_ty, p, policy, out);
            } else {
                // Boxed recursive field: one i32 stash for the loaded box pointer,
                // then recurse from that fresh base.
                out.push(ValType::I32);
                collect_consume_cleanup_scratch(field_ty, p, policy, out);
            }
        }
        // Wildcard or omitted: dropped in place with no scratch.
        _ => {}
    }
}

fn collect_scratch_types_expr(
    expr: &hir::Expr,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    policy: &InlinePolicy,
    out: &mut Vec<ValType>,
) {
    match &expr.kind {
        hir::ExprKind::StructLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(
                    val, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        hir::ExprKind::TupleLit(elems) | hir::ExprKind::ArrayLit(elems) => {
            // Built as a homogeneous tuple: one box pointer, then the elements.
            out.push(ValType::I32);
            for e in elems {
                collect_scratch_types_expr(
                    e, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        hir::ExprKind::TupleIndex { base, .. } => {
            collect_scratch_types_expr(
                base, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::ExprKind::VariantLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(
                    val, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        hir::ExprKind::Match {
            mode: _,
            scrutinee,
            arms,
        } => {
            // The scrutinee local holds the matched value (scalar or pointer).
            out.push(hir_type_to_valtype(&scrutinee.ty));
            collect_scratch_types_expr(
                scrutinee, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            for arm in arms {
                collect_match_arm_temps(&arm.pattern, policy, out);
                collect_scratch_types_expr(
                    &arm.body, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
                // `emit_consume_cleanup` runs after the arm body for a consuming
                // match; reserve the scratch its boxed-field recursion needs in
                // the same position it claims it.
                if policy.match_consumes(arms) {
                    collect_consume_cleanup_scratch(&scrutinee.ty, &arm.pattern, policy, out);
                }
            }
        }
        hir::ExprKind::Str(_) => {
            // One scratch i32 holding the bump-allocated String struct ptr.
            out.push(ValType::I32);
        }
        hir::ExprKind::Binary { left, right, .. } => {
            collect_scratch_types_expr(
                left, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            collect_scratch_types_expr(
                right, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::ExprKind::Call {
            func,
            args,
            arg_modes,
            ..
        } => {
            call_arg_scratch(
                func, args, arg_modes, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            // In this general context `emit_expr` materializes a scalar-ABI
            // return into a box: one temp per leaf field, then the box pointer.
            if let Some(fields) = scalar_ret.get(func) {
                for sf in fields {
                    out.push(sf.valtype);
                }
                out.push(ValType::I32);
            }
        }
        hir::ExprKind::Field { base, .. } => {
            collect_scratch_types_expr(
                base, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::ExprKind::Deref(base) => {
            // An inline-aggregate deref-read boxes a byte copy: two i32 scratch
            // slots (slot stash + box stash). Scalar/boxed reads need none.
            if policy.is_inline(&expr.ty) {
                out.push(ValType::I32);
                out.push(ValType::I32);
            }
            collect_scratch_types_expr(
                base, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::ExprKind::Neg(operand) | hir::ExprKind::BitNot(operand) => {
            collect_scratch_types_expr(
                operand, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::ExprKind::Coerce { value, .. } => {
            // Three i32 scratch slots: source-box stash (for deep-copying a
            // `Copy` aggregate), data_ptr stash, and fat pointer base.
            out.push(ValType::I32);
            out.push(ValType::I32);
            out.push(ValType::I32);
            collect_scratch_types_expr(
                value, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
        }
        hir::ExprKind::DynCall { receiver, args, .. } => {
            // One i32 scratch slot for the fat pointer.
            out.push(ValType::I32);
            collect_scratch_types_expr(
                receiver, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            for a in args {
                collect_scratch_types_expr(
                    a, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            // Should be rewritten to Call by monomorphization before
            // codegen; recurse so any nested generic expression's scratch
            // needs are still counted if this leaks through.
            collect_scratch_types_expr(
                receiver, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            for a in args {
                collect_scratch_types_expr(
                    a, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_scratch_types_expr(
                condition, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            collect_scratch_types_block(
                then_branch,
                runtime,
                scalar_abi,
                scalar_ret,
                ret_scalar,
                policy,
                out,
            );
            if let Some(eb) = else_branch {
                collect_scratch_types_block(
                    eb, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::UnsafeBlock(block) => {
            collect_scratch_types_block(
                block, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            )
        }
        hir::ExprKind::IndirectCall { callee, args } => {
            // One i32 scratch slot to stash the callee's function-table index
            // while the arguments are pushed underneath it.
            out.push(ValType::I32);
            collect_scratch_types_expr(
                callee, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
            );
            for a in args {
                collect_scratch_types_expr(
                    a, runtime, scalar_abi, scalar_ret, ret_scalar, policy, out,
                );
            }
        }
        _ => {}
    }
}

// === String literal bytes ===

/// Collect each string literal's bytes in pre-order — same shape as the
/// dbg-prefix walk, just looking at `Str` nodes instead of `Dbg`.
pub(crate) fn collect_str_literals_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a str>) {
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
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_str_literals_expr(ptr, out);
            collect_str_literals_expr(value, out);
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            collect_str_literals_expr(object, out);
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
        hir::Stmt::Return { value, .. } => {
            if let Some(v) = value {
                collect_str_literals_expr(v, out);
            }
        }
        hir::Stmt::Drop { .. } => {}
    }
}

fn collect_str_literals_expr<'a>(expr: &'a hir::Expr, out: &mut Vec<&'a str>) {
    match &expr.kind {
        hir::ExprKind::Str(s) => out.push(s.as_str()),
        hir::ExprKind::StructLit { fields, .. } => {
            for (_, val) in fields {
                collect_str_literals_expr(val, out);
            }
        }
        hir::ExprKind::VariantLit { fields, .. } => {
            for (_, val) in fields {
                collect_str_literals_expr(val, out);
            }
        }
        hir::ExprKind::Match {
            mode: _,
            scrutinee,
            arms,
        } => {
            collect_str_literals_expr(scrutinee, out);
            for arm in arms {
                collect_str_literals_expr(&arm.body, out);
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
        hir::ExprKind::Coerce { value, .. } => collect_str_literals_expr(value, out),
        hir::ExprKind::DynCall { receiver, args, .. } => {
            collect_str_literals_expr(receiver, out);
            for a in args {
                collect_str_literals_expr(a, out);
            }
        }
        hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            collect_str_literals_expr(receiver, out);
            for a in args {
                collect_str_literals_expr(a, out);
            }
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
        hir::ExprKind::Block(block) | hir::ExprKind::UnsafeBlock(block) => {
            collect_str_literals_block(block, out)
        }
        hir::ExprKind::IndirectCall { callee, args } => {
            collect_str_literals_expr(callee, out);
            for a in args {
                collect_str_literals_expr(a, out);
            }
        }
        _ => {}
    }
}
