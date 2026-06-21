//! Stage-1 ownership / move checker.
//!
//! Runs after type checking and before monomorphization, so every `MethodCall`
//! has already been rewritten to `Call`/`DynCall`/`TraitBoundCall` (the receiver
//! is `args[0]` of a rewritten `Call`) and generic bodies are checked once with
//! `Type::Param` treated as a non-`Copy` (owned) type.
//!
//! The pass is split in two, sharing one notion of "what is a move" with drop
//! elaboration via [`cfg::build`]:
//!
//!  - **Move dataflow** (`check_moves`): lower the body to a CFG and run a
//!    forward may-moved dataflow. A `Use`/`Move` of a may-moved local is a
//!    use-after-move; running the dataflow with and without loop back-edges
//!    tells a loop-carried move (`MoveInLoop`) from a straight-line one
//!    (`UseAfterMove`). Moving a `view`/`edit` parameter is a borrow escape.
//!  - **Borrow rules** (`check_borrows`): a syntactic walk over call sites for
//!    the per-call rules that don't depend on control flow — `edit` exclusivity,
//!    call-site/declaration mode match, and `edit`-of-a-`view`-parameter.
//!
//! Both read-only walks (`check_borrows` and the `collect_tracked` helper) share
//! one structural recursion via the [`Visitor`] trait at the bottom of the file,
//! so the HIR's shape is spelled out once.
//!
//! Modes are erased after this pass; mono/codegen ignore them.

use super::cfg::{self, Action, is_copy, root_symbol};
use super::{Block, Expr, ExprKind, FuncId, PassMode, Pattern, Program, SpanId, Stmt, SymbolId};
use prim_tok::{FileId, Span};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct MoveError {
    pub file: FileId,
    pub span: Span,
    pub kind: MoveErrorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MoveErrorKind {
    /// Use (read, call, or re-move) of a value already moved at `moved_at`.
    UseAfterMove { moved_at: Span },
    /// Moving a non-`Copy` field/payload out of a borrowed (`view`/`edit`) value.
    MoveOutOfBorrow,
    /// A value moved in a loop body would be moved again on the next iteration.
    MoveInLoop,
    /// A `view`/`edit` parameter was moved out of the function.
    BorrowEscapes,
    /// The same place was `edit`-borrowed more than once in a single call.
    EditAlias,
    /// A call-site mode that doesn't match the callee parameter's declared mode.
    ModeMismatch,
    /// `edit`-borrowing a value reachable only through a `view` parameter.
    EditOfView,
    /// A value that implements `Drop` is moved on some paths but not others, so
    /// the compiler can't statically decide whether to drop it at scope exit.
    ConditionalDrop,
}

impl std::fmt::Display for MoveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            MoveErrorKind::UseAfterMove { .. } => write!(f, "use of moved value"),
            MoveErrorKind::MoveOutOfBorrow => write!(f, "cannot move out of a borrow"),
            MoveErrorKind::MoveInLoop => write!(f, "use of value moved in previous loop iteration"),
            MoveErrorKind::BorrowEscapes => write!(f, "borrow cannot escape the function"),
            MoveErrorKind::EditAlias => write!(f, "cannot edit-borrow the same value twice"),
            MoveErrorKind::ModeMismatch => write!(f, "wrong passing mode for argument"),
            MoveErrorKind::EditOfView => write!(f, "cannot edit-borrow a view parameter"),
            MoveErrorKind::ConditionalDrop => write!(
                f,
                "value may be moved on only some paths; a value that implements \
                 Drop must be moved on all paths or none"
            ),
        }
    }
}

impl std::error::Error for MoveError {}

pub fn check(program: &Program) -> Result<(), MoveError> {
    let mut errors = Vec::new();
    for func in &program.functions {
        let mut checker = Checker {
            program,
            errors: Vec::new(),
        };
        checker.check_moves(func);
        checker.check_borrows(func);
        errors.append(&mut checker.errors);
    }
    // Report deterministically: earliest span first.
    errors.sort_by_key(|e| (e.span.start(), e.span.end()));
    match errors.into_iter().next() {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

struct Checker<'a> {
    program: &'a Program,
    errors: Vec<MoveError>,
}

impl Checker<'_> {
    fn emit(&mut self, span_id: SpanId, kind: MoveErrorKind) {
        let (file, span) = self
            .program
            .spans
            .get(span_id.0 as usize)
            .copied()
            .expect("missing span");
        self.errors.push(MoveError { file, span, kind });
    }

    fn span(&self, span_id: SpanId) -> Span {
        self.program
            .spans
            .get(span_id.0 as usize)
            .copied()
            .expect("missing span")
            .1
    }

    // ---- move dataflow ------------------------------------------------------

    /// Use-after-move, move-in-loop, and borrow-escape via a forward may-moved
    /// dataflow over the shared CFG.
    fn check_moves(&mut self, func: &super::Function) {
        // Track every non-`Copy` local: parameters plus all `let`/`match`
        // bindings. `take` params are owned (movable); `view`/`edit` params are
        // borrows that may not be moved out.
        let mut tracked: HashSet<SymbolId> = HashSet::new();
        let mut borrow_params: HashSet<SymbolId> = HashSet::new();
        for p in &func.params {
            if is_copy(&p.ty) {
                continue;
            }
            tracked.insert(p.name);
            if matches!(p.mode, PassMode::View | PassMode::Edit) {
                borrow_params.insert(p.name);
            }
        }
        collect_tracked(&func.body, &mut tracked);

        let cfg = cfg::build(&func.body, &tracked);
        let fwd = cfg::may_in_sets(&cfg, false);
        let full = cfg::may_in_sets(&cfg, true);

        // Replay each block from its entry state, classifying each use/move.
        // `moved_fwd` ignores back-edges (straight-line, within one iteration);
        // `moved_full` includes them. A local moved only via `moved_full` is
        // loop-carried → `MoveInLoop`; via `moved_fwd` → `UseAfterMove`.
        for b in 0..cfg.blocks.len() {
            let mut moved_fwd: HashMap<SymbolId, SpanId> =
                fwd[b].iter().map(|s| (*s, SpanId(0))).collect();
            let mut moved_full: HashMap<SymbolId, SpanId> =
                full[b].iter().map(|s| (*s, SpanId(0))).collect();
            for action in &cfg.blocks[b].actions {
                match *action {
                    Action::Init(l) => {
                        moved_fwd.remove(&l);
                        moved_full.remove(&l);
                    }
                    Action::Use { local, span } => {
                        self.check_moved(local, span, &moved_fwd, &moved_full);
                    }
                    Action::Move {
                        local,
                        span,
                        partial,
                    } => {
                        if borrow_params.contains(&local) {
                            // Moving a borrow out of the function: a whole-value
                            // move escapes; a field move is "out of a borrow".
                            let kind = if partial {
                                MoveErrorKind::MoveOutOfBorrow
                            } else {
                                MoveErrorKind::BorrowEscapes
                            };
                            self.emit(span, kind);
                            continue; // borrow not consumed; don't mark moved
                        }
                        self.check_moved(local, span, &moved_fwd, &moved_full);
                        moved_fwd.insert(local, span);
                        moved_full.insert(local, span);
                    }
                    Action::Drop { .. } => {} // no Drop actions before this pass
                }
            }
        }
    }

    /// Emit a use-after-move / move-in-loop error if `local` is already moved.
    fn check_moved(
        &mut self,
        local: SymbolId,
        span: SpanId,
        moved_fwd: &HashMap<SymbolId, SpanId>,
        moved_full: &HashMap<SymbolId, SpanId>,
    ) {
        if let Some(&moved_at) = moved_fwd.get(&local) {
            let moved_at = self.span(moved_at);
            self.emit(span, MoveErrorKind::UseAfterMove { moved_at });
        } else if moved_full.contains_key(&local) {
            self.emit(span, MoveErrorKind::MoveInLoop);
        }
    }

    // ---- syntactic borrow rules over call sites -----------------------------

    /// Per-call rules that don't depend on control flow (rules 4, 5, 7).
    fn check_borrows(&mut self, func: &super::Function) {
        let view_params: HashSet<SymbolId> = func
            .params
            .iter()
            .filter(|p| !is_copy(&p.ty) && matches!(p.mode, PassMode::View))
            .map(|p| p.name)
            .collect();
        BorrowWalk {
            chk: self,
            view_params,
        }
        .visit_block(&func.body);
    }

    fn func_param_modes(&self, func: FuncId) -> Vec<(PassMode, bool)> {
        self.program
            .functions
            .get(func.0 as usize)
            .map(|f| f.params.iter().map(|p| (p.mode, is_copy(&p.ty))).collect())
            .unwrap_or_default()
    }
}

/// Checks the syntactic borrow rules at each call site. Only calls need a
/// custom `visit_expr`; everything else uses the default `Visitor` recursion.
struct BorrowWalk<'a, 'p> {
    chk: &'a mut Checker<'p>,
    view_params: HashSet<SymbolId>,
}

impl Visitor for BorrowWalk<'_, '_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Call {
                func,
                args,
                arg_modes,
                ..
            } => {
                // The receiver of a rewritten method call is `args[0]`, so the
                // whole argument list is checked (and recursed) together.
                let param_modes = self.chk.func_param_modes(*func);
                self.check_args(args, arg_modes, &param_modes);
            }
            ExprKind::DynCall {
                receiver,
                trait_id,
                method_idx,
                args,
                arg_modes,
            } => {
                self.visit_expr(receiver);
                let modes = self
                    .chk
                    .program
                    .traits
                    .get(trait_id.0 as usize)
                    .and_then(|t| t.methods.get(*method_idx as usize))
                    .map(|m| m.param_modes.clone())
                    .unwrap_or_default();
                let param_modes: Vec<(PassMode, bool)> =
                    modes.iter().skip(1).map(|m| (*m, false)).collect();
                self.check_args(args, arg_modes, &param_modes);
            }
            ExprKind::TraitBoundCall {
                receiver,
                bound,
                method,
                args,
                arg_modes,
                ..
            } => {
                self.visit_expr(receiver);
                let modes = self
                    .chk
                    .program
                    .traits
                    .get(bound.0 as usize)
                    .and_then(|t| {
                        t.method_idx
                            .get(method)
                            .and_then(|i| t.methods.get(*i as usize))
                    })
                    .map(|m| m.param_modes.clone())
                    .unwrap_or_default();
                let param_modes: Vec<(PassMode, bool)> =
                    modes.iter().skip(1).map(|m| (*m, false)).collect();
                self.check_args(args, arg_modes, &param_modes);
            }
            _ => walk_expr(self, expr),
        }
    }
}

impl BorrowWalk<'_, '_> {
    /// Rules 4, 5, 7 over one call's arguments, then recurse into each.
    fn check_args(
        &mut self,
        args: &[Expr],
        arg_modes: &[PassMode],
        param_modes: &[(PassMode, bool)],
    ) {
        // Rule 5: the same root place may not be `edit`-borrowed twice, nor
        // `edit` together with any other mode.
        let mut seen: HashMap<SymbolId, PassMode> = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let mode = arg_modes.get(i).copied().unwrap_or(PassMode::View);
            if let Some(root) = root_symbol(arg) {
                if let Some(prev) = seen.insert(root, mode) {
                    if prev == PassMode::Edit || mode == PassMode::Edit {
                        self.chk.emit(arg.span, MoveErrorKind::EditAlias);
                    }
                }
            }
            // Rule 7: non-Copy params must be passed with the declared mode.
            if let Some((decl_mode, copy)) = param_modes.get(i).copied() {
                if !copy && mode != decl_mode {
                    self.chk.emit(arg.span, MoveErrorKind::ModeMismatch);
                }
            }
            // Rule 4: `edit`-borrowing through a `view` parameter is illegal.
            if mode == PassMode::Edit {
                if let Some(root) = root_symbol(arg) {
                    if self.view_params.contains(&root) {
                        self.chk.emit(arg.span, MoveErrorKind::EditOfView);
                    }
                }
            }
            self.visit_expr(arg);
        }
    }
}

/// Collect every non-`Copy` local bound in a body (`let` and `match`-arm
/// patterns, recursively). Parameters are added by the caller. These are exactly
/// the locals the CFG must track.
fn collect_tracked(block: &Block, out: &mut HashSet<SymbolId>) {
    TrackedCollector { out }.visit_block(block);
}

struct TrackedCollector<'a> {
    out: &'a mut HashSet<SymbolId>,
}

impl Visitor for TrackedCollector<'_> {
    fn visit_pattern(&mut self, pattern: &Pattern) {
        tracked_pattern(pattern, self.out);
    }
}

/// Add every non-`Copy` binding a pattern introduces to the tracked set.
fn tracked_pattern(pattern: &Pattern, out: &mut HashSet<SymbolId>) {
    match pattern {
        Pattern::Binding { symbol, ty, .. } => {
            if !is_copy(ty) {
                out.insert(*symbol);
            }
        }
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                tracked_pattern(e, out);
            }
        }
        Pattern::Variant { fields, .. } => {
            for f in fields {
                tracked_pattern(&f.pattern, out);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

/// A read-only structural walk over a function body. Implementors override only
/// the nodes they care about; the default methods recurse via the free `walk_*`
/// functions, so the HIR's shape is spelled out exactly once.
trait Visitor: Sized {
    fn visit_block(&mut self, block: &Block) {
        walk_block(self, block);
    }
    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }
    fn visit_pattern(&mut self, _pattern: &Pattern) {}
}

fn walk_block<V: Visitor>(v: &mut V, block: &Block) {
    for stmt in &block.stmts {
        v.visit_stmt(stmt);
    }
    if let Some(e) = &block.expr {
        v.visit_expr(e);
    }
}

fn walk_stmt<V: Visitor>(v: &mut V, stmt: &Stmt) {
    match stmt {
        Stmt::Let { pattern, value, .. } => {
            v.visit_pattern(pattern);
            v.visit_expr(value);
        }
        Stmt::Assign { value, .. } => v.visit_expr(value),
        Stmt::DerefAssign { ptr, value, .. } => {
            v.visit_expr(ptr);
            v.visit_expr(value);
        }
        Stmt::FieldAssign { object, value, .. } => {
            v.visit_expr(object);
            v.visit_expr(value);
        }
        Stmt::Expr(e) => v.visit_expr(e),
        Stmt::Loop { body, .. } => v.visit_block(body),
        Stmt::While {
            condition, body, ..
        } => {
            v.visit_expr(condition);
            v.visit_block(body);
        }
        Stmt::Return {
            value: Some(val), ..
        } => v.visit_expr(val),
        Stmt::Return { value: None, .. } | Stmt::Break { .. } | Stmt::Drop { .. } => {}
    }
}

fn walk_expr<V: Visitor>(v: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Binary { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        ExprKind::Call { args, .. } => {
            for a in args {
                v.visit_expr(a);
            }
        }
        ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. }
        | ExprKind::MethodCall { receiver, args, .. } => {
            v.visit_expr(receiver);
            for a in args {
                v.visit_expr(a);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
            for (_, val) in fields {
                v.visit_expr(val);
            }
        }
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                v.visit_expr(e);
            }
        }
        ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => v.visit_expr(base),
        ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => v.visit_expr(e),
        ExprKind::Coerce { value, .. } => v.visit_expr(value),
        ExprKind::Dbg { inner, .. } => v.visit_expr(inner),
        ExprKind::Match { scrutinee, arms } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                v.visit_pattern(&arm.pattern);
                v.visit_expr(&arm.body);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            v.visit_expr(condition);
            v.visit_block(then_branch);
            if let Some(b) = else_branch {
                v.visit_block(b);
            }
        }
        ExprKind::Block(b) => v.visit_block(b),
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
    }
}
