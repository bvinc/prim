//! Drop elaboration: insert `Stmt::Drop` for owned values at scope exits,
//! implementing RAII, with placement decided by a control-flow analysis.
//!
//! Runs after monomorphization, so every type is concrete and `needs_drop` is
//! exact. Three phases per function:
//!
//! 1. **Insert candidates.** For every droppable local in scope, insert a
//!    `Stmt::Drop` at each scope exit (block end, before `return`/`break`), in
//!    reverse declaration order. A `return v` first binds `v` to a fresh temp so
//!    the value is read before its scope's drops run.
//! 2. **Lower to a CFG** (`super::cfg`) recording, in control-flow order, where
//!    each tracked local is initialized, moved, and (candidate-)dropped.
//! 3. **Decide.** The move dataflow classifies each candidate drop as `Keep`
//!    (still owned → real drop), `Remove` (moved on all paths → delete the
//!    candidate), or `Conditional` (moved on some paths only → a compile error:
//!    a `Drop` value must be moved on all paths or none).
//!
//! Each candidate `Stmt::Drop` carries a unique `id` assigned at insertion. The
//! CFG drop action and the apply step both read that id, so the three walks
//! don't have to visit drops in the same order to agree on which is which.

use super::cfg::{self, DropDecision, DropId};
use super::ownership::{MoveError, MoveErrorKind};
use super::{
    Block, DropInfo, Expr, ExprKind, Function, MatchArm, PassMode, Pattern, Program, SpanId, Stmt,
    SymbolId, Type,
};
use prim_tok::{FileId, Span};
use std::collections::{HashMap, HashSet};

pub fn elaborate(program: &mut Program) -> Result<(), MoveError> {
    let fresh_base = program.symbols.len() as u32;
    let mut funcs = std::mem::take(&mut program.functions);
    let info = DropInfo::new(program);
    let mut result = Ok(());
    for func in &mut funcs {
        if let Err(e) = elaborate_function(func, &info, fresh_base, program) {
            // Report the first error (matches type_check / check_ownership).
            if result.is_ok() {
                result = Err(e);
            }
        }
    }
    drop(info);
    program.functions = funcs;
    result
}

fn elaborate_function(
    func: &mut Function,
    info: &DropInfo,
    fresh_base: u32,
    program: &Program,
) -> Result<(), MoveError> {
    // Which locals need dropping: `let`-bound and `take`-param locals of a
    // needs-drop type. Built first so phases 1–2 agree on what to track.
    // Maps each droppable local to its declaration span (for diagnostics).
    let mut droppable: HashMap<SymbolId, SpanId> = HashMap::new();
    for p in &func.params {
        if matches!(p.mode, PassMode::Take) && info.needs_drop(&p.ty) {
            droppable.insert(p.name, p.span);
        }
    }
    collect_droppable_bindings(&func.body, info, &mut droppable);

    // Phase 1: insert candidate drops everywhere a droppable local leaves scope.
    let params: Vec<(SymbolId, Type, SpanId)> = func
        .params
        .iter()
        .filter(|p| droppable.contains_key(&p.name))
        .map(|p| (p.name, p.ty.clone(), p.span))
        .collect();
    let mut inserter = Insert {
        droppable: &droppable,
        scopes: Vec::new(),
        fresh: fresh_base,
        next_id: 0,
    };
    inserter.block(&mut func.body, false, &params);

    // Phase 2: lower the augmented body to the shared CFG, tracking the
    // droppable locals. The builder is the single definition of "what is a
    // move", shared with the ownership pass.
    let tracked: HashSet<SymbolId> = droppable.keys().copied().collect();
    let cfg = cfg::build(&func.body, &tracked);

    // Phase 3: decide each candidate.
    let decisions = cfg::analyze(&cfg);

    // Phase 4: apply — drop the removed candidates, error on conditional ones.
    let mut filter = Filter {
        decisions: &decisions,
        error: None,
    };
    filter.block(&mut func.body);
    match filter.error {
        Some((span_id, kind)) => Err(make_error(program, span_id, kind)),
        None => Ok(()),
    }
}

fn make_error(program: &Program, span_id: SpanId, kind: MoveErrorKind) -> MoveError {
    let (file, span) = program
        .spans
        .get(span_id.0 as usize)
        .copied()
        .unwrap_or((FileId(0), Span::new(0, 0)));
    MoveError { file, span, kind }
}

// === Phase 1: insert candidate drops ===

/// A lexical scope's droppable locals, in declaration order.
struct Frame {
    locals: Vec<(SymbolId, Type, SpanId)>,
    is_loop: bool,
}

struct Insert<'a> {
    droppable: &'a HashMap<SymbolId, SpanId>,
    scopes: Vec<Frame>,
    fresh: u32,
    /// Counter for the unique id stamped on each emitted `Stmt::Drop`.
    next_id: usize,
}

impl Insert<'_> {
    fn drop_stmt(&mut self, sym: SymbolId, ty: Type, span: SpanId) -> Stmt {
        let id = self.next_id;
        self.next_id += 1;
        Stmt::Drop { sym, ty, span, id }
    }

    fn block(&mut self, block: &mut Block, is_loop: bool, seed: &[(SymbolId, Type, SpanId)]) {
        self.scopes.push(Frame {
            locals: seed.to_vec(),
            is_loop,
        });
        let mut out = Vec::with_capacity(block.stmts.len());
        for stmt in std::mem::take(&mut block.stmts) {
            self.stmt(stmt, &mut out);
        }
        // Drop this scope's locals at its end, in reverse declaration order.
        let frame = self.scopes.pop().unwrap();
        for (sym, ty, span) in frame.locals.into_iter().rev() {
            out.push(self.drop_stmt(sym, ty, span));
        }
        block.stmts = out;
    }

    fn stmt(&mut self, mut stmt: Stmt, out: &mut Vec<Stmt>) {
        match &mut stmt {
            Stmt::Let { pattern, value, .. } => {
                self.expr(value);
                let mut binds = Vec::new();
                pattern_bindings(pattern, &mut binds);
                out.push(stmt);
                for (sym, ty) in binds {
                    if let Some(d) = self.droppable.get(&sym) {
                        self.scopes.last_mut().unwrap().locals.push((sym, ty, *d));
                    }
                }
                return;
            }
            Stmt::Assign { value, .. } => self.expr(value),
            Stmt::DerefAssign { ptr, value, .. } => {
                self.expr(ptr);
                self.expr(value);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.expr(object);
                self.expr(value);
            }
            Stmt::Expr(e) => self.expr(e),
            Stmt::Loop { body, .. } => self.block(body, true, &[]),
            Stmt::While {
                condition, body, ..
            } => {
                self.expr(condition);
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

    /// `return v` reads `v` into a fresh temp, then drops, then returns the temp.
    fn elab_return(&mut self, stmt: Stmt, out: &mut Vec<Stmt>) {
        let Stmt::Return { value, span } = stmt else {
            unreachable!()
        };
        let has_drops = self.scopes.iter().any(|f| !f.locals.is_empty());
        match value {
            Some(mut v) if has_drops => {
                self.expr(&mut v);
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
                self.expr(&mut v);
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

    fn emit_return_drops(&mut self, out: &mut Vec<Stmt>) {
        let drops = self.scoped_drops(false);
        out.extend(drops);
    }

    fn emit_break_drops(&mut self, out: &mut Vec<Stmt>) {
        let drops = self.scoped_drops(true);
        out.extend(drops);
    }

    /// Drops for the locals of every enclosing scope, innermost first; when
    /// `stop_at_loop` is set, stop after the nearest loop scope (for `break`).
    fn scoped_drops(&mut self, stop_at_loop: bool) -> Vec<Stmt> {
        let mut sites = Vec::new();
        for frame in self.scopes.iter().rev() {
            sites.extend(frame.locals.iter().rev().cloned());
            if stop_at_loop && frame.is_loop {
                break;
            }
        }
        sites
            .into_iter()
            .map(|(sym, ty, span)| self.drop_stmt(sym, ty, span))
            .collect()
    }

    /// Recurse into an expression to elaborate any blocks it contains.
    fn expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.expr(condition);
                self.block(then_branch, false, &[]);
                if let Some(b) = else_branch {
                    self.block(b, false, &[]);
                }
            }
            ExprKind::Block(b) => self.block(b, false, &[]),
            ExprKind::Match { scrutinee, arms } => {
                self.expr(scrutinee);
                for arm in arms.iter_mut() {
                    self.arm(arm);
                }
            }
            ExprKind::Binary { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            ExprKind::Call { args, .. } => {
                for a in args {
                    self.expr(a);
                }
            }
            ExprKind::DynCall { receiver, args, .. } => {
                self.expr(receiver);
                for a in args {
                    self.expr(a);
                }
            }
            ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
                for (_, v) in fields {
                    self.expr(v);
                }
            }
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                for e in elems {
                    self.expr(e);
                }
            }
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => self.expr(base),
            ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => self.expr(e),
            ExprKind::Coerce { value, .. } => self.expr(value),
            ExprKind::Dbg { inner, .. } => self.expr(inner),
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

    fn arm(&mut self, arm: &mut MatchArm) {
        self.expr(&mut arm.body);
    }
}

// === Phase 4: apply the decisions ===

struct Filter<'a> {
    decisions: &'a HashMap<DropId, DropDecision>,
    error: Option<(SpanId, MoveErrorKind)>,
}

impl Filter<'_> {
    fn block(&mut self, block: &mut Block) {
        let mut out = Vec::with_capacity(block.stmts.len());
        for mut stmt in std::mem::take(&mut block.stmts) {
            if let Stmt::Drop { span, id, .. } = &stmt {
                match self.decisions.get(id).copied() {
                    Some(DropDecision::Remove) => continue, // delete the candidate
                    Some(DropDecision::Conditional) => {
                        if self.error.is_none() {
                            self.error = Some((*span, MoveErrorKind::ConditionalDrop));
                        }
                        continue; // also keep it out of the program
                    }
                    _ => {} // Keep
                }
            } else {
                self.stmt_children(&mut stmt);
            }
            out.push(stmt);
        }
        block.stmts = out;
    }

    fn stmt_children(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Let { value, .. }
            | Stmt::Assign { value, .. }
            | Stmt::Return {
                value: Some(value), ..
            } => self.expr(value),
            Stmt::DerefAssign { ptr, value, .. } => {
                self.expr(ptr);
                self.expr(value);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.expr(object);
                self.expr(value);
            }
            Stmt::Expr(e) => self.expr(e),
            Stmt::Loop { body, .. } => self.block(body),
            Stmt::While {
                condition, body, ..
            } => {
                self.expr(condition);
                self.block(body);
            }
            Stmt::Return { value: None, .. } | Stmt::Break { .. } | Stmt::Drop { .. } => {}
        }
    }

    fn expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.expr(condition);
                self.block(then_branch);
                if let Some(b) = else_branch {
                    self.block(b);
                }
            }
            ExprKind::Block(b) => self.block(b),
            ExprKind::Match { scrutinee, arms } => {
                self.expr(scrutinee);
                for arm in arms.iter_mut() {
                    self.expr(&mut arm.body);
                }
            }
            ExprKind::Binary { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            ExprKind::Call { args, .. } => {
                for a in args {
                    self.expr(a);
                }
            }
            ExprKind::DynCall { receiver, args, .. } => {
                self.expr(receiver);
                for a in args {
                    self.expr(a);
                }
            }
            ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
                for (_, v) in fields {
                    self.expr(v);
                }
            }
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                for e in elems {
                    self.expr(e);
                }
            }
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => self.expr(base),
            ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => self.expr(e),
            ExprKind::Coerce { value, .. } => self.expr(value),
            ExprKind::Dbg { inner, .. } => self.expr(inner),
            ExprKind::MethodCall { receiver, args, .. }
            | ExprKind::TraitBoundCall { receiver, args, .. } => {
                self.expr(receiver);
                for a in args {
                    self.expr(a);
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
}

// === Shared helpers ===

/// Record every `let`-bound local of a needs-drop type (including those in
/// nested blocks, `if`/`match` branches, and loop bodies).
fn collect_droppable_bindings(block: &Block, info: &DropInfo, out: &mut HashMap<SymbolId, SpanId>) {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                let mut binds = Vec::new();
                pattern_binding_spans(pattern, &mut binds);
                for (sym, ty, span) in binds {
                    if info.needs_drop(&ty) {
                        out.insert(sym, span);
                    }
                }
                collect_droppable_expr(value, info, out);
            }
            Stmt::Assign { value, .. }
            | Stmt::Return {
                value: Some(value), ..
            } => collect_droppable_expr(value, info, out),
            Stmt::DerefAssign { ptr, value, .. } => {
                collect_droppable_expr(ptr, info, out);
                collect_droppable_expr(value, info, out);
            }
            Stmt::FieldAssign { object, value, .. } => {
                collect_droppable_expr(object, info, out);
                collect_droppable_expr(value, info, out);
            }
            Stmt::Expr(e) => collect_droppable_expr(e, info, out),
            Stmt::Loop { body, .. } => collect_droppable_bindings(body, info, out),
            Stmt::While {
                condition, body, ..
            } => {
                collect_droppable_expr(condition, info, out);
                collect_droppable_bindings(body, info, out);
            }
            Stmt::Return { value: None, .. } | Stmt::Break { .. } | Stmt::Drop { .. } => {}
        }
    }
}

fn collect_droppable_expr(expr: &Expr, info: &DropInfo, out: &mut HashMap<SymbolId, SpanId>) {
    match &expr.kind {
        ExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            collect_droppable_bindings(then_branch, info, out);
            if let Some(b) = else_branch {
                collect_droppable_bindings(b, info, out);
            }
        }
        ExprKind::Block(b) => collect_droppable_bindings(b, info, out),
        ExprKind::Match { arms, .. } => {
            for arm in arms {
                collect_droppable_expr(&arm.body, info, out);
            }
        }
        _ => {}
    }
}

fn pattern_bindings(pattern: &Pattern, out: &mut Vec<(SymbolId, Type)>) {
    match pattern {
        Pattern::Binding { symbol, ty, .. } => out.push((*symbol, ty.clone())),
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                pattern_bindings(e, out);
            }
        }
        // Struct destructuring binds owned fields (like a tuple), so its
        // bindings are tracked for dropping; `match`/variant bindings still leak.
        Pattern::Struct { fields, .. } => {
            for f in fields {
                pattern_bindings(&f.pattern, out);
            }
        }
        Pattern::Wildcard { .. }
        | Pattern::Int { .. }
        | Pattern::Bool { .. }
        | Pattern::Variant { .. } => {}
    }
}

fn pattern_binding_spans(pattern: &Pattern, out: &mut Vec<(SymbolId, Type, SpanId)>) {
    match pattern {
        Pattern::Binding {
            symbol, ty, span, ..
        } => out.push((*symbol, ty.clone(), *span)),
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                pattern_binding_spans(e, out);
            }
        }
        Pattern::Struct { fields, .. } => {
            for f in fields {
                pattern_binding_spans(&f.pattern, out);
            }
        }
        Pattern::Wildcard { .. }
        | Pattern::Int { .. }
        | Pattern::Bool { .. }
        | Pattern::Variant { .. } => {}
    }
}
