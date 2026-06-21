//! Drop elaboration: insert explicit `Stmt::Drop` for owned locals at the end
//! of their scope (and before early `return`/`break`), implementing RAII.
//!
//! Runs **after** monomorphization, so every type is concrete and `needs_drop`
//! is exact. Placement uses a sound *never-moved* rule: a droppable local that
//! is never moved anywhere in the function is provably live at every scope exit,
//! so dropping it exactly once is safe (no double-free). A local that *might* be
//! moved is left alone — its move transfers ownership, and the new owner drops
//! it. Conditionally-moved values therefore leak rather than double-free; that
//! is the conservative, sound trade for not threading a full move dataflow here.
//!
//! Scope of this first cut: `let`-bound locals and `take` parameters. Values
//! bound by `match` arm payloads (and enums consumed by `match`) are not yet
//! dropped — they leak, which is sound.

use super::{
    Block, DropInfo, Expr, ExprKind, Function, MatchArm, PassMode, Pattern, Program, Stmt,
    SymbolId, Type,
};
use std::collections::HashSet;

pub fn elaborate(program: &mut Program) {
    // Fresh symbol ids for synthesized return temporaries start past every
    // existing symbol, so they can't collide with a real local. Reused per
    // function (each function has its own wasm-local space).
    let fresh_base = program.symbols.len() as u32;
    // Take functions out so we can build the (immutable-borrowing) `DropInfo`
    // over the rest of the program while mutating bodies.
    let mut funcs = std::mem::take(&mut program.functions);
    let info = DropInfo::new(program);
    for func in &mut funcs {
        elaborate_function(func, &info, fresh_base);
    }
    drop(info);
    program.functions = funcs;
}

fn elaborate_function(func: &mut Function, info: &DropInfo, fresh_base: u32) {
    let mut ever_moved = HashSet::new();
    collect_moved_block(&func.body, &mut ever_moved);

    let mut elab = Elab {
        info,
        ever_moved,
        scopes: Vec::new(),
        fresh: fresh_base,
    };

    // The function's outermost scope owns its `take` parameters — but only the
    // droppable ones (needs-drop, never moved out) get dropped at function exit.
    let params: Vec<(SymbolId, Type)> = func
        .params
        .iter()
        .filter(|p| matches!(p.mode, PassMode::Take) && elab.droppable(p.name, &p.ty))
        .map(|p| (p.name, p.ty.clone()))
        .collect();

    elab.block(&mut func.body, false, &params);
}

/// A lexical scope: the droppable locals declared in it, in declaration order.
struct Frame {
    locals: Vec<(SymbolId, Type)>,
    /// True for a `loop`/`while` body — the boundary a `break` drops out to.
    is_loop: bool,
}

struct Elab<'a> {
    info: &'a DropInfo<'a>,
    ever_moved: HashSet<SymbolId>,
    scopes: Vec<Frame>,
    /// Next fresh symbol id for synthesized return temporaries.
    fresh: u32,
}

impl Elab<'_> {
    fn droppable(&self, sym: SymbolId, ty: &Type) -> bool {
        !self.ever_moved.contains(&sym) && self.info.needs_drop(ty)
    }

    fn drop_stmt(sym: SymbolId, ty: Type) -> Stmt {
        Stmt::Drop {
            sym,
            ty,
            // Span is only used for diagnostics; drops never error, so reuse 0.
            span: super::SpanId(0),
        }
    }

    /// Elaborate a block as a new scope. `seed` pre-populates the scope with
    /// owned locals already in scope at entry (the function's `take` params for
    /// the top block; empty otherwise).
    fn block(&mut self, block: &mut Block, is_loop: bool, seed: &[(SymbolId, Type)]) {
        self.scopes.push(Frame {
            locals: seed.to_vec(),
            is_loop,
        });
        let mut out = Vec::with_capacity(block.stmts.len());
        for stmt in std::mem::take(&mut block.stmts) {
            self.stmt(stmt, &mut out);
        }
        // Normal fall-through exit: drop this scope's locals, last declared
        // first.
        let frame = self.scopes.last().expect("scope frame");
        for (sym, ty) in frame.locals.iter().rev() {
            out.push(Self::drop_stmt(*sym, ty.clone()));
        }
        self.scopes.pop();
        block.stmts = out;
    }

    fn stmt(&mut self, mut stmt: Stmt, out: &mut Vec<Stmt>) {
        match &mut stmt {
            Stmt::Let { pattern, value, .. } => {
                self.elab_expr(value);
                // Record any droppable bindings — *after* the initializer, in
                // declaration order, so later early-exits drop them.
                let mut binds = Vec::new();
                pattern_bindings(pattern, &mut binds);
                out.push(stmt);
                for (sym, ty) in binds {
                    if self.droppable(sym, &ty) {
                        self.scopes.last_mut().unwrap().locals.push((sym, ty));
                    }
                }
                return;
            }
            Stmt::Assign { value, .. } => self.elab_expr(value),
            Stmt::DerefAssign { ptr, value, .. } => {
                self.elab_expr(ptr);
                self.elab_expr(value);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.elab_expr(object);
                self.elab_expr(value);
            }
            Stmt::Expr(e) => self.elab_expr(e),
            Stmt::Loop { body, .. } => self.block(body, true, &[]),
            Stmt::While {
                condition, body, ..
            } => {
                self.elab_expr(condition);
                self.block(body, true, &[]);
            }
            Stmt::Return { .. } => {
                self.elab_return(stmt, out);
                return;
            }
            Stmt::Break { .. } => self.emit_break_drops(out),
            Stmt::Drop { .. } => {}
        }
        out.push(stmt);
    }

    /// `return v` must drop owned locals *after* evaluating `v` (which may
    /// borrow one of them). When there are drops and a value, bind the value to
    /// a fresh temporary first, drop, then return the temporary.
    fn elab_return(&mut self, stmt: Stmt, out: &mut Vec<Stmt>) {
        let Stmt::Return { value, span } = stmt else {
            unreachable!()
        };
        let has_drops = self.scopes.iter().any(|f| !f.locals.is_empty());
        match value {
            Some(mut v) if has_drops => {
                self.elab_expr(&mut v);
                let ty = v.ty.clone();
                let tmp = SymbolId(self.fresh);
                self.fresh += 1;
                out.push(Stmt::Let {
                    pattern: Pattern::Binding {
                        symbol: tmp,
                        ty: ty.clone(),
                        span,
                    },
                    ty: ty.clone(),
                    value: v,
                    span,
                });
                self.emit_return_drops(out);
                out.push(Stmt::Return {
                    value: Some(Expr {
                        kind: ExprKind::Ident(tmp),
                        ty,
                        span,
                    }),
                    span,
                });
            }
            Some(mut v) => {
                self.elab_expr(&mut v);
                out.push(Stmt::Return {
                    value: Some(v),
                    span,
                });
            }
            None => {
                self.emit_return_drops(out);
                out.push(Stmt::Return { value: None, span });
            }
        }
    }

    /// Drops before a `return`: all owned locals in every enclosing scope,
    /// innermost scope first and last-declared first within each.
    fn emit_return_drops(&self, out: &mut Vec<Stmt>) {
        for frame in self.scopes.iter().rev() {
            for (sym, ty) in frame.locals.iter().rev() {
                out.push(Self::drop_stmt(*sym, ty.clone()));
            }
        }
    }

    /// Drops before a `break`: owned locals in scopes inside the innermost loop
    /// (down to and including the loop body frame), innermost first.
    fn emit_break_drops(&self, out: &mut Vec<Stmt>) {
        for frame in self.scopes.iter().rev() {
            for (sym, ty) in frame.locals.iter().rev() {
                out.push(Self::drop_stmt(*sym, ty.clone()));
            }
            if frame.is_loop {
                break;
            }
        }
    }

    /// Recurse into an expression, elaborating any blocks it contains
    /// (`if` branches, statement-position `match`-arm blocks, block exprs).
    fn elab_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.elab_expr(condition);
                self.block(then_branch, false, &[]);
                if let Some(b) = else_branch {
                    self.block(b, false, &[]);
                }
            }
            ExprKind::Block(b) => self.block(b, false, &[]),
            ExprKind::Match { scrutinee, arms } => {
                self.elab_expr(scrutinee);
                for arm in arms.iter_mut() {
                    self.elab_arm(arm);
                }
            }
            ExprKind::Binary { left, right, .. } => {
                self.elab_expr(left);
                self.elab_expr(right);
            }
            ExprKind::Call { args, .. } => {
                for a in args {
                    self.elab_expr(a);
                }
            }
            ExprKind::DynCall { receiver, args, .. } => {
                self.elab_expr(receiver);
                for a in args {
                    self.elab_expr(a);
                }
            }
            ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
                for (_, v) in fields {
                    self.elab_expr(v);
                }
            }
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                for e in elems {
                    self.elab_expr(e);
                }
            }
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => {
                self.elab_expr(base)
            }
            ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => self.elab_expr(e),
            ExprKind::Coerce { value, .. } => self.elab_expr(value),
            ExprKind::Dbg { inner, .. } => self.elab_expr(inner),
            // Post-mono these carry no nested blocks (or don't survive mono).
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Ident(_)
            | ExprKind::Spawn { .. }
            | ExprKind::MethodCall { .. }
            | ExprKind::TraitBoundCall { .. }
            | ExprKind::Error => {}
        }
    }

    fn elab_arm(&mut self, arm: &mut MatchArm) {
        // An arm body that is a block becomes its own scope (handled by
        // `elab_expr`). Payload bindings are not dropped in this first cut.
        self.elab_expr(&mut arm.body);
    }
}

/// Collect the symbols bound by a pattern, with their types.
fn pattern_bindings(pattern: &Pattern, out: &mut Vec<(SymbolId, Type)>) {
    match pattern {
        Pattern::Binding { symbol, ty, .. } => out.push((*symbol, ty.clone())),
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                pattern_bindings(e, out);
            }
        }
        Pattern::Wildcard { .. }
        | Pattern::Int { .. }
        | Pattern::Bool { .. }
        | Pattern::Variant { .. } => {}
    }
}

// === Syntactic "ever moved" collection ===
//
// A symbol is recorded if it could be moved at runtime: it appears as a bare
// `Ident` (or the root of a field/index place) in a move position — a `take`
// argument, the value of a `let`/assign/return/field-store/deref-store, an
// aggregate-literal element, or a `match` scrutinee. This is a sound
// over-approximation; missing a move would risk a double-free, so when in doubt
// we mark moved (and merely leak).

fn collect_moved_block(block: &Block, out: &mut HashSet<SymbolId>) {
    for stmt in &block.stmts {
        collect_moved_stmt(stmt, out);
    }
}

fn collect_moved_stmt(stmt: &Stmt, out: &mut HashSet<SymbolId>) {
    match stmt {
        Stmt::Let { value, .. } => moved_value(value, out),
        Stmt::Assign { value, .. } => moved_value(value, out),
        Stmt::DerefAssign { ptr, value, .. } => {
            collect_moved_expr(ptr, out);
            moved_value(value, out);
        }
        Stmt::FieldAssign { object, value, .. } => {
            collect_moved_expr(object, out);
            moved_value(value, out);
        }
        Stmt::Expr(e) => collect_moved_expr(e, out),
        Stmt::Loop { body, .. } => collect_moved_block(body, out),
        Stmt::While {
            condition, body, ..
        } => {
            collect_moved_expr(condition, out);
            collect_moved_block(body, out);
        }
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                moved_value(v, out);
            }
        }
        Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

/// `expr` is in a move position: if it is a *non-`Copy`* place naming a local,
/// that local is moved (a `Copy` read — e.g. `r.id` of an `i32` field — never
/// consumes its source). Always also recurse for nested moves.
fn moved_value(expr: &Expr, out: &mut HashSet<SymbolId>) {
    if !is_copy(&expr.ty) {
        if let Some(root) = root_symbol(expr) {
            out.insert(root);
        }
    }
    collect_moved_expr(expr, out);
}

/// Scalars and raw pointers are `Copy` (reading them never moves the source);
/// aggregates and type parameters are not. Mirrors `ownership::is_copy`.
fn is_copy(ty: &Type) -> bool {
    matches!(
        ty,
        Type::U8
            | Type::I8
            | Type::U16
            | Type::I16
            | Type::U32
            | Type::I32
            | Type::U64
            | Type::I64
            | Type::Usize
            | Type::Isize
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::Pointer { .. }
            | Type::IntVar
            | Type::FloatVar
            | Type::Undetermined
    )
}

/// Recurse into an expression collecting moves from its move-position children.
fn collect_moved_expr(expr: &Expr, out: &mut HashSet<SymbolId>) {
    match &expr.kind {
        ExprKind::Binary { left, right, .. } => {
            collect_moved_expr(left, out);
            collect_moved_expr(right, out);
        }
        ExprKind::Call {
            args, arg_modes, ..
        } => {
            for (i, a) in args.iter().enumerate() {
                if matches!(arg_modes.get(i), Some(PassMode::Take)) {
                    moved_value(a, out);
                } else {
                    collect_moved_expr(a, out);
                }
            }
        }
        ExprKind::DynCall {
            receiver,
            args,
            arg_modes,
            ..
        } => {
            // The receiver's mode isn't in `arg_modes`; treat it as a borrow
            // (dispatch receivers are view/edit), and check the rest.
            collect_moved_expr(receiver, out);
            for (i, a) in args.iter().enumerate() {
                if matches!(arg_modes.get(i), Some(PassMode::Take)) {
                    moved_value(a, out);
                } else {
                    collect_moved_expr(a, out);
                }
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, v) in fields {
                moved_value(v, out);
            }
        }
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                moved_value(e, out);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            // A matched non-`Copy` scrutinee may be consumed; over-approximate.
            moved_value(scrutinee, out);
            for arm in arms {
                collect_moved_expr(&arm.body, out);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_moved_expr(condition, out);
            collect_moved_block(then_branch, out);
            if let Some(b) = else_branch {
                collect_moved_block(b, out);
            }
        }
        ExprKind::Block(b) => collect_moved_block(b, out),
        ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => {
            collect_moved_expr(base, out)
        }
        ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => collect_moved_expr(e, out),
        ExprKind::Coerce { value, .. } => moved_value(value, out),
        ExprKind::Dbg { inner, .. } => collect_moved_expr(inner, out),
        ExprKind::MethodCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            collect_moved_expr(receiver, out);
            for a in args {
                collect_moved_expr(a, out);
            }
        }
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
    }
}

/// The local a place expression is rooted at (`x`, `x.f`, `x.0`, `*x`).
fn root_symbol(expr: &Expr) -> Option<SymbolId> {
    match &expr.kind {
        ExprKind::Ident(sym) => Some(*sym),
        ExprKind::Field { base, .. }
        | ExprKind::TupleIndex { base, .. }
        | ExprKind::Deref(base) => root_symbol(base),
        _ => None,
    }
}
