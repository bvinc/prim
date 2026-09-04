//! Block inlining: splice every call to an `inline fn` into its caller.
//!
//! Runs after monomorphization (so every type is concrete and no `Type::Param`
//! remains) and before drop elaboration. `block(...)` parameters are
//! second-class and have no runtime value; this pass is what makes their
//! *textual capture* semantics real:
//!
//! - a borrow parameter (`read`/`mut`, non-`Copy`) is substituted, by name,
//!   with the caller's argument place (no binding, no drop);
//! - a `Copy` parameter is bound to a fresh local (`let`), evaluated once;
//! - a `block` parameter is erased: each `b(args)` call in the callee body is
//!   replaced with the block literal's body, with the block literal's parameter
//!   names substituted by `args`;
//! - `return`/`break` inside a block body are therefore literally the caller's
//!   `return`/`break` — they were typechecked against the caller's context.
//!
//! After this pass, no `Type::Block`, `ExprKind::BlockLit`, or
//! `ExprKind::BlockCall` remains, and every `inline fn` is emptied so codegen
//! skips its second-class signature.

use super::{
    Block, BlockLitParam, Enum, Expr, ExprKind, FuncId, Function, MethodOwner, PassMode, Pattern,
    Program, Stmt, Struct, Symbol, SymbolId, SymbolKind, Type,
};
use std::collections::{HashMap, HashSet};

/// Read-only structural info needed while splicing (so we can decide whether a
/// block element is an inline aggregate or a boxed pointer, and whether a value
/// is `Copy`).
struct LayoutInfo<'a> {
    structs: &'a [Struct],
    enums: &'a [Enum],
    /// Non-generic struct/enum owners with an explicit `impl Copy` — the same
    /// set typecheck consults, so an `own` `impl Copy` param is copied once
    /// rather than aliasing the caller's place.
    copy_types: &'a HashSet<MethodOwner>,
}

impl LayoutInfo<'_> {
    /// Whether `ty` is `Copy` (post-mono, concrete): scalars and raw pointers
    /// unconditionally, plus `impl Copy` structs/enums. Mirrors
    /// `cfg::CopyCtx::is_copy`, which is the copy policy typecheck/ownership
    /// apply to inline-fn `own` parameters.
    fn is_copy(&self, ty: &Type) -> bool {
        super::cfg::is_copy(self.copy_types, ty)
    }

    /// Mirror `InlinePolicy::is_inline`: an aggregate with a finite byte layout
    /// (so its fields live in the slot itself) rather than a box pointer.
    fn is_inline(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Struct(..) | Type::Tuple(..) | Type::Enum(..) | Type::Array(..)
        ) && self.finite(ty, &mut Vec::new())
    }

    fn finite(&self, ty: &Type, visiting: &mut Vec<u32>) -> bool {
        match ty {
            Type::Struct(sid, _) => {
                if visiting.contains(&sid.0) {
                    return false;
                }
                visiting.push(sid.0);
                let ok = self
                    .structs
                    .get(sid.0 as usize)
                    .map(|s| s.fields.iter().all(|f| self.finite(&f.ty, visiting)))
                    .unwrap_or(false);
                visiting.pop();
                ok
            }
            Type::Enum(eid, _) => {
                if visiting.contains(&eid.0) {
                    return false;
                }
                visiting.push(eid.0);
                let ok = self
                    .enums
                    .get(eid.0 as usize)
                    .map(|e| {
                        e.variants
                            .iter()
                            .all(|v| v.fields.iter().all(|f| self.finite(&f.ty, visiting)))
                    })
                    .unwrap_or(false);
                visiting.pop();
                ok
            }
            Type::Tuple(elems) => elems.iter().all(|t| self.finite(t, visiting)),
            Type::Array(elem, _) => self.finite(elem, visiting),
            // Scalars, pointers, and trait objects are finite leaves.
            _ => true,
        }
    }
}

/// A substituted place: two forms of the same location. `read` is what a
/// *read* of the bound name should evaluate to; `write` is the pointer to
/// deref for a whole-place assignment (`e = v`).
///
/// The split matters for block elements: a scalar element reads as a value but
/// writes through its pointer, while an inline-aggregate element both reads
/// (its fields alias) and writes through its pointer.
#[derive(Clone)]
struct Place {
    read: Expr,
    write: Expr,
}

pub fn inline_program(program: &mut Program) {
    // Snapshot the (post-mono) inline bodies so callers can be rewritten in
    // place while their callees are read from the snapshot.
    let snapshot: Vec<Function> = program.functions.clone();
    let (symbols, functions, structs, enums, copy_types) = (
        &mut program.symbols,
        &mut program.functions,
        &*program.structs,
        &*program.enums,
        &program.copy_types,
    );
    let layout = LayoutInfo {
        structs,
        enums,
        copy_types,
    };
    for f in functions.iter_mut() {
        if !f.is_inline {
            rewrite_block(&mut f.body, &snapshot, symbols, &layout);
        }
    }
    // Empty every inline fn: its calls are all inlined, so nothing references
    // its body or its `block`-typed params anymore. Clearing (rather than
    // removing) keeps FuncId indices stable.
    for f in functions.iter_mut() {
        if f.is_inline {
            f.body.stmts.clear();
            f.body.expr = None;
            f.params.clear();
            f.ret = None;
        }
    }
}

// === Recursive inlining rewrite ===

fn rewrite_block(
    block: &mut Block,
    snapshot: &[Function],
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) {
    for stmt in &mut block.stmts {
        rewrite_stmt(stmt, snapshot, symbols, layout);
    }
    if let Some(e) = &mut block.expr {
        rewrite_expr(e, snapshot, symbols, layout);
    }
}

fn rewrite_stmt(
    stmt: &mut Stmt,
    snapshot: &[Function],
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) {
    match stmt {
        Stmt::Let { value, .. } => rewrite_expr(value, snapshot, symbols, layout),
        Stmt::Expr(e) => rewrite_expr(e, snapshot, symbols, layout),
        Stmt::Assign { value, .. } => rewrite_expr(value, snapshot, symbols, layout),
        Stmt::DerefAssign { ptr, value, .. } => {
            rewrite_expr(ptr, snapshot, symbols, layout);
            rewrite_expr(value, snapshot, symbols, layout);
        }
        Stmt::FieldAssign { object, value, .. } => {
            rewrite_expr(object, snapshot, symbols, layout);
            rewrite_expr(value, snapshot, symbols, layout);
        }
        Stmt::Loop { body, .. } => rewrite_block(body, snapshot, symbols, layout),
        Stmt::While {
            condition, body, ..
        } => {
            rewrite_expr(condition, snapshot, symbols, layout);
            rewrite_block(body, snapshot, symbols, layout);
        }
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                rewrite_expr(v, snapshot, symbols, layout);
            }
        }
        Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

fn rewrite_expr(
    expr: &mut Expr,
    snapshot: &[Function],
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) {
    match &mut expr.kind {
        ExprKind::Call { func, args, .. } => {
            for a in args.iter_mut() {
                rewrite_expr(a, snapshot, symbols, layout);
            }
            if snapshot[func.0 as usize].is_inline {
                let mut body = build_inlined_body(*func, args, snapshot, symbols, layout);
                rewrite_block(&mut body, snapshot, symbols, layout);
                expr.kind = ExprKind::Block(body);
            }
        }
        ExprKind::Binary { left, right, .. } => {
            rewrite_expr(left, snapshot, symbols, layout);
            rewrite_expr(right, snapshot, symbols, layout);
        }
        ExprKind::Spawn { .. }
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::ConstParam(_)
        | ExprKind::Error => {}
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, f) in fields.iter_mut() {
                rewrite_expr(f, snapshot, symbols, layout);
            }
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => {
            rewrite_expr(scrutinee, snapshot, symbols, layout);
            for arm in arms.iter_mut() {
                rewrite_expr(&mut arm.body, snapshot, symbols, layout);
            }
        }
        ExprKind::Field { base, .. } => rewrite_expr(base, snapshot, symbols, layout),
        ExprKind::Deref(base) => rewrite_expr(base, snapshot, symbols, layout),
        ExprKind::MethodCall { receiver, args, .. } => {
            rewrite_expr(receiver, snapshot, symbols, layout);
            for a in args.iter_mut() {
                rewrite_expr(a, snapshot, symbols, layout);
            }
        }
        ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            rewrite_expr(receiver, snapshot, symbols, layout);
            for a in args.iter_mut() {
                rewrite_expr(a, snapshot, symbols, layout);
            }
        }
        ExprKind::Coerce { value, .. } => rewrite_expr(value, snapshot, symbols, layout),
        ExprKind::BitNot(inner) | ExprKind::Neg(inner) => {
            rewrite_expr(inner, snapshot, symbols, layout)
        }
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems.iter_mut() {
                rewrite_expr(e, snapshot, symbols, layout);
            }
        }
        ExprKind::TupleIndex { base, .. } => rewrite_expr(base, snapshot, symbols, layout),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expr(condition, snapshot, symbols, layout);
            rewrite_block(then_branch, snapshot, symbols, layout);
            if let Some(b) = else_branch {
                rewrite_block(b, snapshot, symbols, layout);
            }
        }
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => {
            rewrite_block(b, snapshot, symbols, layout)
        }
        ExprKind::BlockLit { body, .. } => rewrite_block(body, snapshot, symbols, layout),
        ExprKind::BlockCall { args, .. } => {
            for a in args.iter_mut() {
                rewrite_expr(a, snapshot, symbols, layout);
            }
        }
    }
}

// === Building one inlined body ===

/// `(literal param names, declared element types, literal body)`.
type BlockSplice = (Vec<BlockLitParam>, Vec<Type>, Block);

fn build_inlined_body(
    func: FuncId,
    args: &[Expr],
    snapshot: &[Function],
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) -> Block {
    let callee = &snapshot[func.0 as usize];

    let mut rename: HashMap<SymbolId, SymbolId> = HashMap::new();
    let mut place_map: HashMap<SymbolId, Place> = HashMap::new();
    let mut block_map: HashMap<SymbolId, BlockSplice> = HashMap::new();
    let mut prelude: Vec<Stmt> = Vec::new();

    for (p, arg) in callee.params.iter().zip(args.iter()) {
        match &p.ty {
            Type::Block(block_params) => {
                let (params, body) = match &arg.kind {
                    ExprKind::BlockLit { params, body } => (params.clone(), body.clone()),
                    // Typecheck already rejected a non-literal block argument;
                    // an empty body keeps this pass total.
                    _ => (Vec::new(), Block::empty()),
                };
                let elem_tys = block_params.iter().map(|bp| bp.ty.clone()).collect();
                block_map.insert(p.name, (params, elem_tys, body));
            }
            _ if layout.is_copy(&p.ty) => {
                let fresh = fresh_symbol(symbols, p.name);
                rename.insert(p.name, fresh);
                prelude.push(Stmt::Let {
                    pattern: Pattern::Binding {
                        symbol: fresh,
                        ty: p.ty.clone(),
                        mode: PassMode::Own,
                        span: p.span,
                    },
                    ty: p.ty.clone(),
                    value: arg.clone(),
                    span: p.span,
                });
            }
            _ => {
                // Borrow parameter: substitute its name with the caller's place.
                place_map.insert(p.name, Place::same(arg.clone()));
            }
        }
    }

    let mut body = callee.body.clone();
    // Freshen every local binding the callee introduces (let/loop/match/block
    // literal params) so two splices of the same callee never share a symbol.
    let mut bindings = Vec::new();
    collect_binding_symbols(&body, &mut bindings);
    for sym in bindings {
        let fresh = fresh_symbol(symbols, sym);
        rename.insert(sym, fresh);
    }
    rename_block(&mut body, &rename);

    // Substitute borrow params by name, then erase block calls.
    substitute_places(&mut body, &place_map);
    splice_block_calls(&mut body, &block_map, symbols, layout);

    prelude.extend(body.stmts);
    body.stmts = prelude;
    body
}

impl Place {
    fn same(expr: Expr) -> Self {
        Place {
            read: expr.clone(),
            write: expr,
        }
    }

    /// Build the read/write place for a block element. The callee passes the
    /// element as a deref (`*add(...)`); scalars read the deref value, while
    /// inline aggregates must keep the *address* (their fields alias the slot).
    /// Both write through the deref's inner pointer.
    fn for_block_elem(arg: &Expr, elem_ty: &Type, layout: &LayoutInfo<'_>) -> Self {
        let write = strip_deref(arg);
        let read = if layout.is_inline(elem_ty) {
            let mut addr = strip_deref(arg);
            addr.ty = elem_ty.clone();
            addr
        } else {
            arg.clone()
        };
        Place { read, write }
    }
}

/// If `expr` is `Deref(inner)`, return `inner` (the pointer); otherwise the
/// expression itself.
fn strip_deref(expr: &Expr) -> Expr {
    if let ExprKind::Deref(inner) = &expr.kind {
        (**inner).clone()
    } else {
        expr.clone()
    }
}

fn fresh_symbol(symbols: &mut Vec<Symbol>, old: SymbolId) -> SymbolId {
    let id = SymbolId(symbols.len() as u32);
    let template = symbols
        .get(old.0 as usize)
        .expect("missing symbol to freshen");
    symbols.push(Symbol {
        id,
        module: template.module,
        name: template.name,
        kind: SymbolKind::Local,
    });
    id
}

// === Symbol freshening (rename) ===

fn collect_binding_symbols(block: &Block, out: &mut Vec<SymbolId>) {
    for s in &block.stmts {
        collect_stmt_bindings(s, out);
    }
    if let Some(e) = &block.expr {
        collect_expr_bindings(e, out);
    }
}

fn collect_stmt_bindings(stmt: &Stmt, out: &mut Vec<SymbolId>) {
    match stmt {
        Stmt::Let { pattern, value, .. } => {
            collect_pattern_bindings(pattern, out);
            collect_expr_bindings(value, out);
        }
        Stmt::Expr(e) => collect_expr_bindings(e, out),
        Stmt::Assign { value, .. } => collect_expr_bindings(value, out),
        Stmt::DerefAssign { ptr, value, .. } => {
            collect_expr_bindings(ptr, out);
            collect_expr_bindings(value, out);
        }
        Stmt::FieldAssign { object, value, .. } => {
            collect_expr_bindings(object, out);
            collect_expr_bindings(value, out);
        }
        Stmt::Loop { body, .. } => collect_binding_symbols(body, out),
        Stmt::While {
            condition, body, ..
        } => {
            collect_expr_bindings(condition, out);
            collect_binding_symbols(body, out);
        }
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                collect_expr_bindings(v, out);
            }
        }
        Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

fn collect_expr_bindings(expr: &Expr, out: &mut Vec<SymbolId>) {
    match &expr.kind {
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::ConstParam(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
        ExprKind::Binary { left, right, .. } => {
            collect_expr_bindings(left, out);
            collect_expr_bindings(right, out);
        }
        ExprKind::Call { args, .. } => {
            for a in args {
                collect_expr_bindings(a, out);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, f) in fields {
                collect_expr_bindings(f, out);
            }
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => {
            collect_expr_bindings(scrutinee, out);
            for arm in arms {
                collect_pattern_bindings(&arm.pattern, out);
                collect_expr_bindings(&arm.body, out);
            }
        }
        ExprKind::Field { base, .. } => collect_expr_bindings(base, out),
        ExprKind::Deref(base) => collect_expr_bindings(base, out),
        ExprKind::MethodCall { receiver, args, .. } => {
            collect_expr_bindings(receiver, out);
            for a in args {
                collect_expr_bindings(a, out);
            }
        }
        ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            collect_expr_bindings(receiver, out);
            for a in args {
                collect_expr_bindings(a, out);
            }
        }
        ExprKind::Coerce { value, .. } => collect_expr_bindings(value, out),
        ExprKind::BitNot(inner) | ExprKind::Neg(inner) => collect_expr_bindings(inner, out),
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                collect_expr_bindings(e, out);
            }
        }
        ExprKind::TupleIndex { base, .. } => collect_expr_bindings(base, out),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_expr_bindings(condition, out);
            collect_binding_symbols(then_branch, out);
            if let Some(b) = else_branch {
                collect_binding_symbols(b, out);
            }
        }
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => collect_binding_symbols(b, out),
        ExprKind::BlockLit { params, body } => {
            for p in params {
                out.push(p.name);
            }
            collect_binding_symbols(body, out);
        }
        ExprKind::BlockCall { args, .. } => {
            for a in args {
                collect_expr_bindings(a, out);
            }
        }
    }
}

fn collect_pattern_bindings(pattern: &Pattern, out: &mut Vec<SymbolId>) {
    match pattern {
        Pattern::Binding { symbol, .. } => out.push(*symbol),
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                collect_pattern_bindings(e, out);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                collect_pattern_bindings(&f.pattern, out);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

fn rename_block(block: &mut Block, rename: &HashMap<SymbolId, SymbolId>) {
    for stmt in &mut block.stmts {
        rename_stmt(stmt, rename);
    }
    if let Some(e) = &mut block.expr {
        rename_expr(e, rename);
    }
}

fn rename_stmt(stmt: &mut Stmt, rename: &HashMap<SymbolId, SymbolId>) {
    match stmt {
        Stmt::Let { pattern, value, .. } => {
            rename_pattern(pattern, rename);
            rename_expr(value, rename);
        }
        Stmt::Expr(e) => rename_expr(e, rename),
        Stmt::Assign { target, value, .. } => {
            if let Some(&n) = rename.get(target) {
                *target = n;
            }
            rename_expr(value, rename);
        }
        Stmt::DerefAssign { ptr, value, .. } => {
            rename_expr(ptr, rename);
            rename_expr(value, rename);
        }
        Stmt::FieldAssign { object, value, .. } => {
            rename_expr(object, rename);
            rename_expr(value, rename);
        }
        Stmt::Loop { body, .. } => rename_block(body, rename),
        Stmt::While {
            condition, body, ..
        } => {
            rename_expr(condition, rename);
            rename_block(body, rename);
        }
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                rename_expr(v, rename);
            }
        }
        Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

fn rename_expr(expr: &mut Expr, rename: &HashMap<SymbolId, SymbolId>) {
    match &mut expr.kind {
        ExprKind::Ident(sym) => {
            if let Some(&n) = rename.get(sym) {
                *sym = n;
            }
        }
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::ConstParam(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
        ExprKind::Binary { left, right, .. } => {
            rename_expr(left, rename);
            rename_expr(right, rename);
        }
        ExprKind::Call { args, .. } => {
            for a in args {
                rename_expr(a, rename);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, f) in fields {
                rename_expr(f, rename);
            }
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => {
            rename_expr(scrutinee, rename);
            for arm in arms {
                rename_pattern(&mut arm.pattern, rename);
                rename_expr(&mut arm.body, rename);
            }
        }
        ExprKind::Field { base, .. } => rename_expr(base, rename),
        ExprKind::Deref(base) => rename_expr(base, rename),
        ExprKind::MethodCall { receiver, args, .. } => {
            rename_expr(receiver, rename);
            for a in args {
                rename_expr(a, rename);
            }
        }
        ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            rename_expr(receiver, rename);
            for a in args {
                rename_expr(a, rename);
            }
        }
        ExprKind::Coerce { value, .. } => rename_expr(value, rename),
        ExprKind::BitNot(inner) | ExprKind::Neg(inner) => rename_expr(inner, rename),
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                rename_expr(e, rename);
            }
        }
        ExprKind::TupleIndex { base, .. } => rename_expr(base, rename),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rename_expr(condition, rename);
            rename_block(then_branch, rename);
            if let Some(b) = else_branch {
                rename_block(b, rename);
            }
        }
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => rename_block(b, rename),
        ExprKind::BlockLit { params, body } => {
            for p in params {
                if let Some(&n) = rename.get(&p.name) {
                    p.name = n;
                }
            }
            rename_block(body, rename);
        }
        ExprKind::BlockCall { param, args, .. } => {
            if let Some(&n) = rename.get(param) {
                *param = n;
            }
            for a in args {
                rename_expr(a, rename);
            }
        }
    }
}

fn rename_pattern(pattern: &mut Pattern, rename: &HashMap<SymbolId, SymbolId>) {
    match pattern {
        Pattern::Binding { symbol, .. } => {
            if let Some(&n) = rename.get(symbol) {
                *symbol = n;
            }
        }
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                rename_pattern(e, rename);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                rename_pattern(&mut f.pattern, rename);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

// === Place substitution (borrow params and block params) ===

fn substitute_places(block: &mut Block, places: &HashMap<SymbolId, Place>) {
    for stmt in &mut block.stmts {
        substitute_places_stmt(stmt, places);
    }
    if let Some(e) = &mut block.expr {
        substitute_places_expr(e, places);
    }
}

fn substitute_places_stmt(stmt: &mut Stmt, places: &HashMap<SymbolId, Place>) {
    match stmt {
        Stmt::Let { value, .. } => substitute_places_expr(value, places),
        Stmt::Expr(e) => substitute_places_expr(e, places),
        Stmt::Assign { target, value, .. } => {
            if let Some(rep) = places.get(target) {
                // Writing a substituted place: `e = v` becomes `*e = v`. The
                // value may itself read the place (`e = e + 1`), so substitute
                // it first, then store through the write pointer.
                substitute_places_expr(value, places);
                let ptr = rep.write.clone();
                let val = value.clone();
                *stmt = Stmt::DerefAssign {
                    ptr,
                    value: val,
                    span: value.span,
                };
            } else {
                substitute_places_expr(value, places);
            }
        }
        Stmt::DerefAssign { ptr, value, .. } => {
            substitute_places_expr(ptr, places);
            substitute_places_expr(value, places);
        }
        Stmt::FieldAssign { object, value, .. } => {
            substitute_places_expr(object, places);
            substitute_places_expr(value, places);
        }
        Stmt::Loop { body, .. } => substitute_places(body, places),
        Stmt::While {
            condition, body, ..
        } => {
            substitute_places_expr(condition, places);
            substitute_places(body, places);
        }
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                substitute_places_expr(v, places);
            }
        }
        Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

fn substitute_places_expr(expr: &mut Expr, places: &HashMap<SymbolId, Place>) {
    match &mut expr.kind {
        ExprKind::Ident(sym) => {
            if let Some(rep) = places.get(sym) {
                expr.kind = rep.read.kind.clone();
                // Keep the original expression's type (the element type); the
                // replacement place carries the same type.
            }
        }
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::ConstParam(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
        ExprKind::Binary { left, right, .. } => {
            substitute_places_expr(left, places);
            substitute_places_expr(right, places);
        }
        ExprKind::Call { args, .. } => {
            for a in args {
                substitute_places_expr(a, places);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, f) in fields {
                substitute_places_expr(f, places);
            }
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => {
            substitute_places_expr(scrutinee, places);
            for arm in arms {
                substitute_places_expr(&mut arm.body, places);
            }
        }
        ExprKind::Field { base, .. } => substitute_places_expr(base, places),
        ExprKind::Deref(base) => substitute_places_expr(base, places),
        ExprKind::MethodCall { receiver, args, .. } => {
            substitute_places_expr(receiver, places);
            for a in args {
                substitute_places_expr(a, places);
            }
        }
        ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            substitute_places_expr(receiver, places);
            for a in args {
                substitute_places_expr(a, places);
            }
        }
        ExprKind::Coerce { value, .. } => substitute_places_expr(value, places),
        ExprKind::BitNot(inner) | ExprKind::Neg(inner) => substitute_places_expr(inner, places),
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                substitute_places_expr(e, places);
            }
        }
        ExprKind::TupleIndex { base, .. } => substitute_places_expr(base, places),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            substitute_places_expr(condition, places);
            substitute_places(then_branch, places);
            if let Some(b) = else_branch {
                substitute_places(b, places);
            }
        }
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => substitute_places(b, places),
        ExprKind::BlockLit { body, .. } => substitute_places(body, places),
        ExprKind::BlockCall { args, .. } => {
            for a in args {
                substitute_places_expr(a, places);
            }
        }
    }
}

// === Block-call splicing ===

fn splice_block_calls(
    block: &mut Block,
    blocks: &HashMap<SymbolId, BlockSplice>,
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) {
    for stmt in &mut block.stmts {
        splice_block_calls_stmt(stmt, blocks, symbols, layout);
    }
    if let Some(e) = &mut block.expr {
        splice_block_calls_expr(e, blocks, symbols, layout);
    }
}

fn splice_block_calls_stmt(
    stmt: &mut Stmt,
    blocks: &HashMap<SymbolId, BlockSplice>,
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) {
    match stmt {
        Stmt::Let { value, .. } => splice_block_calls_expr(value, blocks, symbols, layout),
        Stmt::Expr(e) => splice_block_calls_expr(e, blocks, symbols, layout),
        Stmt::Assign { value, .. } => splice_block_calls_expr(value, blocks, symbols, layout),
        Stmt::DerefAssign { ptr, value, .. } => {
            splice_block_calls_expr(ptr, blocks, symbols, layout);
            splice_block_calls_expr(value, blocks, symbols, layout);
        }
        Stmt::FieldAssign { object, value, .. } => {
            splice_block_calls_expr(object, blocks, symbols, layout);
            splice_block_calls_expr(value, blocks, symbols, layout);
        }
        Stmt::Loop { body, .. } => splice_block_calls(body, blocks, symbols, layout),
        Stmt::While {
            condition, body, ..
        } => {
            splice_block_calls_expr(condition, blocks, symbols, layout);
            splice_block_calls(body, blocks, symbols, layout);
        }
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                splice_block_calls_expr(v, blocks, symbols, layout);
            }
        }
        Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

fn splice_block_calls_expr(
    expr: &mut Expr,
    blocks: &HashMap<SymbolId, BlockSplice>,
    symbols: &mut Vec<Symbol>,
    layout: &LayoutInfo<'_>,
) {
    match &mut expr.kind {
        ExprKind::BlockCall { param, args, .. } => {
            if let Some((lit_params, elem_tys, lit_body)) = blocks.get(param) {
                let mut body = lit_body.clone();
                // Freshen the block body's own bindings per splice site.
                let mut bindings = Vec::new();
                collect_binding_symbols(&body, &mut bindings);
                let mut rename = HashMap::new();
                for sym in bindings {
                    let fresh = fresh_symbol(symbols, sym);
                    rename.insert(sym, fresh);
                }
                rename_block(&mut body, &rename);

                // Substitute the literal's parameter names with the call args
                // (places). The args have already had the callee's borrow
                // params substituted and copy params renamed.
                let mut places = HashMap::new();
                for ((lit, ty), a) in lit_params.iter().zip(elem_tys.iter()).zip(args.iter()) {
                    places.insert(lit.name, Place::for_block_elem(a, ty, layout));
                }
                substitute_places(&mut body, &places);
                // A block call is Unit-valued; discard the body's trailing expr.
                discard_tail(&mut body);
                let span = expr.span;
                expr.kind = ExprKind::Block(body);
                expr.span = span;
            }
        }
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::ConstParam(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
        ExprKind::Binary { left, right, .. } => {
            splice_block_calls_expr(left, blocks, symbols, layout);
            splice_block_calls_expr(right, blocks, symbols, layout);
        }
        ExprKind::Call { args, .. } => {
            for a in args {
                splice_block_calls_expr(a, blocks, symbols, layout);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, f) in fields {
                splice_block_calls_expr(f, blocks, symbols, layout);
            }
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => {
            splice_block_calls_expr(scrutinee, blocks, symbols, layout);
            for arm in arms {
                splice_block_calls_expr(&mut arm.body, blocks, symbols, layout);
            }
        }
        ExprKind::Field { base, .. } => splice_block_calls_expr(base, blocks, symbols, layout),
        ExprKind::Deref(base) => splice_block_calls_expr(base, blocks, symbols, layout),
        ExprKind::MethodCall { receiver, args, .. } => {
            splice_block_calls_expr(receiver, blocks, symbols, layout);
            for a in args {
                splice_block_calls_expr(a, blocks, symbols, layout);
            }
        }
        ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            splice_block_calls_expr(receiver, blocks, symbols, layout);
            for a in args {
                splice_block_calls_expr(a, blocks, symbols, layout);
            }
        }
        ExprKind::Coerce { value, .. } => splice_block_calls_expr(value, blocks, symbols, layout),
        ExprKind::BitNot(inner) | ExprKind::Neg(inner) => {
            splice_block_calls_expr(inner, blocks, symbols, layout)
        }
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                splice_block_calls_expr(e, blocks, symbols, layout);
            }
        }
        ExprKind::TupleIndex { base, .. } => splice_block_calls_expr(base, blocks, symbols, layout),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            splice_block_calls_expr(condition, blocks, symbols, layout);
            splice_block_calls(then_branch, blocks, symbols, layout);
            if let Some(b) = else_branch {
                splice_block_calls(b, blocks, symbols, layout);
            }
        }
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => {
            splice_block_calls(b, blocks, symbols, layout)
        }
        ExprKind::BlockLit { body, .. } => splice_block_calls(body, blocks, symbols, layout),
    }
}

fn discard_tail(block: &mut Block) {
    if let Some(e) = block.expr.take() {
        block.stmts.push(Stmt::Expr(*e));
    }
}

// Small helper needed by the splicer.
impl Block {
    fn empty() -> Self {
        Block {
            stmts: Vec::new(),
            expr: None,
        }
    }
}
