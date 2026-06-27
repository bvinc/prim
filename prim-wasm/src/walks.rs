//! Pre-walks over HIR function bodies.
//!
//! Each walk collects something the emission pass needs to know up-front
//! (before it starts producing wasm instructions). All walks are pre-order
//! and visit the same nodes in the same sequence as `emit::emit_expr`, so
//! per-node counters in `EmitCtx` line up with the collected lists.

use crate::emit::ScalarField;
use crate::types::hir_type_to_valtype;
use prim_compiler::hir;
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
        hir::ExprKind::Match { scrutinee, arms } => {
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
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    out: &mut Vec<ValType>,
) {
    let abi = scalar_abi.get(func);
    for (i, a) in args.iter().enumerate() {
        if abi.is_some_and(|v| v.get(i).copied().unwrap_or(false)) {
            scalar_value_scratch(a, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        } else {
            collect_scratch_types_expr(a, runtime, scalar_abi, scalar_ret, ret_scalar, out);
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
    out: &mut Vec<ValType>,
) {
    match &e.kind {
        hir::ExprKind::Ident(_) => {}
        hir::ExprKind::StructLit { .. } | hir::ExprKind::TupleLit(_) => {
            collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        // A scalar-ABI-returning call is passed straight through (no box).
        hir::ExprKind::Call { func, args, .. } if scalar_ret.contains_key(func) => {
            call_arg_scratch(func, args, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        _ => {
            collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, out);
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
    out: &mut Vec<ValType>,
) {
    for stmt in &body.stmts {
        collect_scratch_types_stmt(stmt, runtime, scalar_abi, scalar_ret, ret_scalar, out);
    }
    if let Some(expr) = &body.expr {
        if ret_scalar {
            scalar_value_scratch(expr, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        } else {
            collect_scratch_types_expr(expr, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
    }
}

fn collect_scratch_types_block(
    block: &hir::Block,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    out: &mut Vec<ValType>,
) {
    for stmt in &block.stmts {
        collect_scratch_types_stmt(stmt, runtime, scalar_abi, scalar_ret, ret_scalar, out);
    }
    if let Some(expr) = &block.expr {
        collect_scratch_types_expr(expr, runtime, scalar_abi, scalar_ret, ret_scalar, out);
    }
}

fn collect_scratch_types_stmt(
    stmt: &hir::Stmt,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    out: &mut Vec<ValType>,
) {
    match stmt {
        hir::Stmt::Assign { value, .. } => {
            collect_scratch_types_expr(value, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        // The value is produced first, then the pattern binds it (reserving a
        // scratch pointer per tuple level).
        hir::Stmt::Let { pattern, value, .. } => {
            collect_scratch_types_expr(value, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            collect_scratch_types_pattern(pattern, out);
        }
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_scratch_types_expr(ptr, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            collect_scratch_types_expr(value, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            collect_scratch_types_expr(object, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            collect_scratch_types_expr(value, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::Stmt::Expr(e) => {
            collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, out)
        }
        hir::Stmt::Loop { body, .. } => {
            collect_scratch_types_block(body, runtime, scalar_abi, scalar_ret, ret_scalar, out)
        }
        hir::Stmt::While {
            condition, body, ..
        } => {
            collect_scratch_types_expr(condition, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            collect_scratch_types_block(body, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::Stmt::Break { .. } => {}
        hir::Stmt::Return { value, .. } => {
            if let Some(v) = value {
                if ret_scalar {
                    scalar_value_scratch(v, runtime, scalar_abi, scalar_ret, ret_scalar, out);
                } else {
                    collect_scratch_types_expr(v, runtime, scalar_abi, scalar_ret, ret_scalar, out);
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
fn collect_scratch_types_pattern(pattern: &hir::Pattern, out: &mut Vec<ValType>) {
    match pattern {
        hir::Pattern::Wildcard { .. }
        | hir::Pattern::Binding { .. }
        | hir::Pattern::Int { .. }
        | hir::Pattern::Bool { .. } => {}
        hir::Pattern::Tuple { elems, .. } => {
            out.push(ValType::I32);
            for elem in elems {
                collect_scratch_types_pattern(elem, out);
            }
        }
        // A struct destructure stashes its base pointer (one i32), like a tuple.
        hir::Pattern::Struct { fields, .. } => {
            out.push(ValType::I32);
            for fp in fields {
                collect_scratch_types_pattern(&fp.pattern, out);
            }
        }
        hir::Pattern::Variant { fields, .. } => {
            for fp in fields {
                collect_scratch_types_pattern(&fp.pattern, out);
            }
        }
    }
}

/// Reserve the scratch temps a `match` arm's test-and-bind needs: one i32 per
/// *non-root* aggregate (tuple/variant) node, to stash its pointer for repeated
/// field access. Mirrors `emit::emit_test_bind` / `emit::aggregate_base` (the
/// root value lives in the scrutinee local, so it needs no temp).
fn collect_match_arm_temps(pattern: &hir::Pattern, out: &mut Vec<ValType>) {
    fn child(pattern: &hir::Pattern, out: &mut Vec<ValType>) {
        match pattern {
            hir::Pattern::Tuple { elems, .. } => {
                out.push(ValType::I32);
                for elem in elems {
                    child(elem, out);
                }
            }
            hir::Pattern::Variant { fields, .. } | hir::Pattern::Struct { fields, .. } => {
                out.push(ValType::I32);
                for fp in fields {
                    child(&fp.pattern, out);
                }
            }
            _ => {}
        }
    }
    match pattern {
        hir::Pattern::Tuple { elems, .. } => {
            for elem in elems {
                child(elem, out);
            }
        }
        hir::Pattern::Variant { fields, .. } | hir::Pattern::Struct { fields, .. } => {
            for fp in fields {
                child(&fp.pattern, out);
            }
        }
        _ => {}
    }
}

fn collect_scratch_types_expr(
    expr: &hir::Expr,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    ret_scalar: bool,
    out: &mut Vec<ValType>,
) {
    match &expr.kind {
        hir::ExprKind::StructLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(val, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::TupleLit(elems) => {
            out.push(ValType::I32);
            for e in elems {
                collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::TupleIndex { base, .. } => {
            collect_scratch_types_expr(base, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::ExprKind::VariantLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(val, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::Match { scrutinee, arms } => {
            // The scrutinee local holds the matched value (scalar or pointer).
            out.push(hir_type_to_valtype(&scrutinee.ty));
            collect_scratch_types_expr(scrutinee, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            for arm in arms {
                collect_match_arm_temps(&arm.pattern, out);
                collect_scratch_types_expr(
                    &arm.body, runtime, scalar_abi, scalar_ret, ret_scalar, out,
                );
            }
        }
        hir::ExprKind::Dbg { inner, .. } => {
            out.push(hir_type_to_valtype(&inner.ty));
            collect_scratch_types_expr(inner, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::ExprKind::Str(_) => {
            // One scratch i32 holding the bump-allocated String struct ptr.
            out.push(ValType::I32);
        }
        hir::ExprKind::Binary { left, right, .. } => {
            collect_scratch_types_expr(left, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            collect_scratch_types_expr(right, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::ExprKind::Call { func, args, .. } => {
            call_arg_scratch(func, args, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            // In this general context `emit_expr` materializes a scalar-ABI
            // return into a box: one temp per leaf field, then the box pointer.
            if let Some(fields) = scalar_ret.get(func) {
                for sf in fields {
                    out.push(sf.valtype);
                }
                out.push(ValType::I32);
            }
        }
        hir::ExprKind::Field { base, .. } | hir::ExprKind::Deref(base) => {
            collect_scratch_types_expr(base, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::ExprKind::Neg(operand) | hir::ExprKind::BitNot(operand) => {
            collect_scratch_types_expr(operand, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::ExprKind::Coerce { value, .. } => {
            // Two i32 scratch slots: data_ptr stash and fat pointer base.
            out.push(ValType::I32);
            out.push(ValType::I32);
            collect_scratch_types_expr(value, runtime, scalar_abi, scalar_ret, ret_scalar, out);
        }
        hir::ExprKind::DynCall { receiver, args, .. } => {
            // One i32 scratch slot for the fat pointer.
            out.push(ValType::I32);
            collect_scratch_types_expr(receiver, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            for a in args {
                collect_scratch_types_expr(a, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            // Should be rewritten to Call by monomorphization before
            // codegen; recurse so any nested generic expression's scratch
            // needs are still counted if this leaks through.
            collect_scratch_types_expr(receiver, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            for a in args {
                collect_scratch_types_expr(a, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_scratch_types_expr(e, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_scratch_types_expr(condition, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            collect_scratch_types_block(
                then_branch,
                runtime,
                scalar_abi,
                scalar_ret,
                ret_scalar,
                out,
            );
            if let Some(eb) = else_branch {
                collect_scratch_types_block(eb, runtime, scalar_abi, scalar_ret, ret_scalar, out);
            }
        }
        hir::ExprKind::Block(block) => {
            collect_scratch_types_block(block, runtime, scalar_abi, scalar_ret, ret_scalar, out)
        }
        _ => {}
    }
}

// === @dbg prefix strings ===

/// Collect each `@dbg` site's prefix string (already built by HIR lowering)
/// in pre-order, so the program-level layout pass can place them in static
/// memory and emission can index them by counter.
pub(crate) fn collect_dbg_prefixes_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a str>) {
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
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_dbg_prefixes_expr(ptr, out);
            collect_dbg_prefixes_expr(value, out);
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            collect_dbg_prefixes_expr(object, out);
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
        hir::Stmt::Return { value, .. } => {
            if let Some(v) = value {
                collect_dbg_prefixes_expr(v, out);
            }
        }
        hir::Stmt::Drop { .. } => {}
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
        hir::ExprKind::TupleLit(elems) => {
            for e in elems {
                collect_dbg_prefixes_expr(e, out);
            }
        }
        hir::ExprKind::TupleIndex { base, .. } => collect_dbg_prefixes_expr(base, out),
        hir::ExprKind::VariantLit { fields, .. } => {
            for (_, val) in fields {
                collect_dbg_prefixes_expr(val, out);
            }
        }
        hir::ExprKind::Match { scrutinee, arms } => {
            collect_dbg_prefixes_expr(scrutinee, out);
            for arm in arms {
                collect_dbg_prefixes_expr(&arm.body, out);
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
        hir::ExprKind::Neg(operand) | hir::ExprKind::BitNot(operand) => {
            collect_dbg_prefixes_expr(operand, out);
        }
        hir::ExprKind::Coerce { value, .. } => collect_dbg_prefixes_expr(value, out),
        hir::ExprKind::DynCall { receiver, args, .. } => {
            collect_dbg_prefixes_expr(receiver, out);
            for a in args {
                collect_dbg_prefixes_expr(a, out);
            }
        }
        hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            collect_dbg_prefixes_expr(receiver, out);
            for a in args {
                collect_dbg_prefixes_expr(a, out);
            }
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
        hir::ExprKind::Dbg { inner, .. } => collect_str_literals_expr(inner, out),
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
        hir::ExprKind::Match { scrutinee, arms } => {
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
        hir::ExprKind::Block(block) => collect_str_literals_block(block, out),
        _ => {}
    }
}
