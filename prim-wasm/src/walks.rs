//! Pre-walks over HIR function bodies.
//!
//! Each walk collects something the emission pass needs to know up-front
//! (before it starts producing wasm instructions). All walks are pre-order
//! and visit the same nodes in the same sequence as `emit::emit_expr`, so
//! per-node counters in `EmitCtx` line up with the collected lists.

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
        hir::Stmt::Let {
            name, ty, value, ..
        } => {
            locals.push((*name, hir_type_to_valtype(ty)));
            collect_locals_expr(value, locals);
        }
        hir::Stmt::Assign { value, .. } => collect_locals_expr(value, locals),
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_locals_expr(ptr, locals);
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
        hir::ExprKind::VariantLit { fields, .. } => {
            for (_, val) in fields {
                collect_locals_expr(val, locals);
            }
        }
        hir::ExprKind::Match { scrutinee, arms } => {
            collect_locals_expr(scrutinee, locals);
            for arm in arms {
                if let hir::Pattern::Variant { bindings, .. } = &arm.pattern {
                    for (_, binding, ty) in bindings {
                        locals.push((*binding, hir_type_to_valtype(ty)));
                    }
                }
                collect_locals_expr(&arm.body, locals);
            }
        }
        hir::ExprKind::Field { base, .. } => collect_locals_expr(base, locals),
        hir::ExprKind::Deref(base) => collect_locals_expr(base, locals),
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

pub(crate) fn collect_scratch_types_block(
    block: &hir::Block,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
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
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    out: &mut Vec<ValType>,
) {
    match stmt {
        hir::Stmt::Let { value, .. } | hir::Stmt::Assign { value, .. } => {
            collect_scratch_types_expr(value, runtime, out);
        }
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            collect_scratch_types_expr(ptr, runtime, out);
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
        hir::Stmt::Return { value, .. } => {
            if let Some(v) = value {
                collect_scratch_types_expr(v, runtime, out);
            }
        }
    }
}

fn collect_scratch_types_expr(
    expr: &hir::Expr,
    runtime: &HashMap<hir::FuncId, hir::RuntimeAbi>,
    out: &mut Vec<ValType>,
) {
    match &expr.kind {
        hir::ExprKind::StructLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(val, runtime, out);
            }
        }
        hir::ExprKind::VariantLit { fields, .. } => {
            out.push(ValType::I32);
            for (_, val) in fields {
                collect_scratch_types_expr(val, runtime, out);
            }
        }
        hir::ExprKind::Match { scrutinee, arms } => {
            out.push(ValType::I32);
            collect_scratch_types_expr(scrutinee, runtime, out);
            for arm in arms {
                collect_scratch_types_expr(&arm.body, runtime, out);
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
        hir::ExprKind::Call { func, args, .. } => {
            // write(fd, s: String) needs one i32 scratch to duplicate the
            // String struct ptr across two field loads.
            if runtime.get(func) == Some(&hir::RuntimeAbi::Write) {
                out.push(ValType::I32);
            }
            for a in args {
                collect_scratch_types_expr(a, runtime, out);
            }
        }
        hir::ExprKind::Field { base, .. } | hir::ExprKind::Deref(base) => {
            collect_scratch_types_expr(base, runtime, out);
        }
        hir::ExprKind::Coerce { value, .. } => {
            // Two i32 scratch slots: data_ptr stash and fat pointer base.
            out.push(ValType::I32);
            out.push(ValType::I32);
            collect_scratch_types_expr(value, runtime, out);
        }
        hir::ExprKind::DynCall { receiver, args, .. } => {
            // One i32 scratch slot for the fat pointer.
            out.push(ValType::I32);
            collect_scratch_types_expr(receiver, runtime, out);
            for a in args {
                collect_scratch_types_expr(a, runtime, out);
            }
        }
        hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            // Should be rewritten to Call by monomorphization before
            // codegen; recurse so any nested generic expression's scratch
            // needs are still counted if this leaks through.
            collect_scratch_types_expr(receiver, runtime, out);
            for a in args {
                collect_scratch_types_expr(a, runtime, out);
            }
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
