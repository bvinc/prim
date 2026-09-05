//! Stage-1 ownership / move checker.
//!
//! Runs after type checking and before monomorphization, so every `MethodCall`
//! has already been rewritten to `Call`/`DynCall`/`TraitBoundCall` (the receiver
//! is `args[0]` of a rewritten `Call`) and generic bodies are checked once with
//! `Type::Param` treated as a non-`Copy` (owned) type.
//!
//! The pass is split across two checks, sharing one notion of "what is a move"
//! with drop elaboration via [`cfg::build`]:
//!
//!  - **Move dataflow** (`check_moves`): lower the body to a CFG and run a
//!    forward may-moved dataflow. A `Use`/`Move` of a may-moved local is a
//!    use-after-move; running the dataflow with and without loop back-edges
//!    tells a loop-carried move (`MoveInLoop`) from a straight-line one
//!    (`UseAfterMove`). Moving a `read`/`mut` parameter is a borrow escape;
//!    moving a field/payload out of a borrow is `MoveOutOfBorrow`.
//!  - **Borrow rules** (`check_borrows`): a syntactic walk for the per-call
//!    rules that don't depend on control flow — `mut` exclusivity (`MutAlias`),
//!    call-site/declaration mode match (`ModeMismatch`), and
//!    `mut`-of-a-`read`-parameter (`MutOfRead`) — plus the second-class
//!    guarantee that a borrow is never boxed: coercing a borrow (a `read`/`mut`
//!    parameter or match-arm binding) to a trait object is `CoerceOfBorrow`,
//!    because the box would outlive the borrow's scope.
//!
//! Second-classness itself needs no extra machinery: a borrow parameter or arm
//! binding has the *plain* type `T`, so storing it, returning it, or moving out
//! of it is already rejected by the move dataflow above.
//!
//! Modes are erased after this pass; mono/codegen ignore them.

use super::cfg::{self, Action, CopyCtx, Effect, copy_trait_id, root_symbol};
use super::{
    Block, BlockParam, Expr, ExprKind, FuncId, PassMode, Pattern, Program, SpanId, Stmt, SymbolId,
    TraitId, Type,
};
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
    /// Moving a non-`Copy` field/payload out of a borrowed (`read`/`mut`) value.
    MoveOutOfBorrow,
    /// A value moved in a loop body would be moved again on the next iteration.
    MoveInLoop,
    /// A `read`/`mut` parameter was moved out of the function.
    BorrowEscapes,
    /// The same place was `mut`-borrowed more than once in a single call.
    MutAlias,
    /// A call-site mode that doesn't match the callee parameter's declared mode.
    ModeMismatch,
    /// `mut`-borrowing a value reachable only through a `read` parameter.
    MutOfRead,
    /// A value that implements `Drop` is moved on some paths but not others, so
    /// the compiler can't statically decide whether to drop it at scope exit.
    ConditionalDrop,
    /// A second-class borrow (a `read`/`mut` parameter or match-arm binding)
    /// was coerced to a trait object: the coercion boxes the value, but the
    /// box holds a reference, so it would outlive the borrow's scope. Trait
    /// objects may only be built from owned values.
    CoerceOfBorrow,
}

impl std::fmt::Display for MoveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            MoveErrorKind::UseAfterMove { .. } => write!(f, "use of moved value"),
            MoveErrorKind::MoveOutOfBorrow => write!(f, "cannot move out of a borrow"),
            MoveErrorKind::MoveInLoop => write!(f, "use of value moved in previous loop iteration"),
            MoveErrorKind::BorrowEscapes => write!(f, "borrow cannot escape the function"),
            MoveErrorKind::MutAlias => write!(f, "cannot mutably borrow the same value twice"),
            MoveErrorKind::ModeMismatch => write!(f, "wrong passing mode for argument"),
            MoveErrorKind::MutOfRead => {
                write!(f, "cannot mutably borrow through a read borrow")
            }
            MoveErrorKind::ConditionalDrop => write!(
                f,
                "value may be moved on only some paths; a value that implements \
                 Drop must be moved on all paths or none"
            ),
            MoveErrorKind::CoerceOfBorrow => write!(
                f,
                "cannot box a borrow into a trait object; a trait object owns its \
                 value, and a second-class borrow (parameter or match binding) \
                 cannot outlive the call or arm that holds it"
            ),
        }
    }
}

impl std::error::Error for MoveError {}

pub fn check(program: &Program) -> Result<(), MoveError> {
    let mut errors = Vec::new();
    let copy_trait = copy_trait_id(program);
    for func in &program.functions {
        let mut checker = Checker {
            program,
            copy_trait,
            errors: Vec::new(),
        };
        checker.check_moves(func);
        checker.check_borrows(func);
        errors.append(&mut checker.errors);
    }
    // Report deterministically: earliest span first. `CoerceOfBorrow` is a more
    // specific diagnostic than the dataflow's `BorrowEscapes` (both can fire at
    // the same span for `fn f(read s: S) -> Trait { return s }`), so it is
    // ranked first; otherwise the stable sort keeps the passes' insertion order
    // (so `MoveInLoop` still wins over a same-span `ModeMismatch`).
    errors.sort_by(|a, b| {
        (a.span.start(), a.span.end(), error_priority(&a.kind)).cmp(&(
            b.span.start(),
            b.span.end(),
            error_priority(&b.kind),
        ))
    });
    match errors.into_iter().next() {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

/// Rank a move error for reporting when two errors share a span. Lower ranks
/// are reported first. Only `CoerceOfBorrow` is special-cased (it should
/// supersede the dataflow's whole-value `BorrowEscapes`); everything else ties
/// so the stable sort preserves the passes' insertion order.
fn error_priority(kind: &MoveErrorKind) -> u8 {
    match kind {
        MoveErrorKind::CoerceOfBorrow => 0,
        _ => 1,
    }
}

struct Checker<'a> {
    program: &'a Program,
    /// The `Copy` marker trait, for resolving `T: Copy` bounds in generic bodies.
    copy_trait: Option<TraitId>,
    errors: Vec<MoveError>,
}

impl<'a> Checker<'a> {
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
    fn check_moves(&mut self, func: &'a super::Function) {
        // Track every non-`Copy` local: parameters plus all `let`/`match`
        // bindings. `own` params are owned (movable); `read`/`mut` params are
        // borrows that may not be moved out. Match-arm `read`/`mut`/bare
        // bindings of non-`Copy` payloads are borrows too (second-class, like a
        // parameter) — unmovable, dying at the arm's end.
        let copy_ctx = CopyCtx::new(&self.program.copy_types, self.copy_trait, &func.type_params);
        let mut tracked: HashSet<SymbolId> = HashSet::new();
        let mut borrow_params: HashSet<SymbolId> = HashSet::new();
        for p in &func.params {
            // Block parameters are second-class (no runtime value); they are
            // never tracked as movables or borrows.
            if matches!(p.ty, Type::Block(_)) {
                continue;
            }
            match copy_ctx.effect(p.mode, &p.ty) {
                // Copy params aren't tracked; `own` params are owned (movable);
                // `read`/`mut` params are borrows that may not be moved out.
                Effect::Copy => {}
                Effect::Move => {
                    tracked.insert(p.name);
                }
                Effect::Borrow => {
                    tracked.insert(p.name);
                    borrow_params.insert(p.name);
                }
            }
        }
        collect_tracked(copy_ctx, &func.body, &mut tracked);
        collect_borrow_bindings(copy_ctx, &func.body, &mut borrow_params);

        let cfg = cfg::build(&func.body, &tracked, &copy_ctx);
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

    /// Per-call rules that don't depend on control flow (rules 4, 5, 7), plus
    /// the second-class guard: a borrow (`read`/`mut` parameter or arm binding)
    /// may never be boxed into a trait object.
    fn check_borrows(&mut self, func: &'a super::Function) {
        // `read_borrows` = every *read-only* borrow in scope: `read` parameters
        // (rule 4: no `mut` through them) plus the `read`/bare bindings of the
        // match arm being walked. `borrowed` = every borrow parameter, `read`
        // or `mut` (for the Coerce-of-borrow rule). Only non-`Copy` parameters
        // are real borrows.
        let copy_ctx = CopyCtx::new(&self.program.copy_types, self.copy_trait, &func.type_params);
        let read_borrows: HashSet<SymbolId> = func
            .params
            .iter()
            .filter(|p| {
                !matches!(p.ty, Type::Block(_))
                    && !copy_ctx.is_copy(&p.ty)
                    && matches!(p.mode, PassMode::Read)
            })
            .map(|p| p.name)
            .collect();
        let borrowed: HashSet<SymbolId> = func
            .params
            .iter()
            .filter(|p| {
                !matches!(p.ty, Type::Block(_))
                    && !copy_ctx.is_copy(&p.ty)
                    && matches!(p.mode, PassMode::Read | PassMode::Mut)
            })
            .map(|p| p.name)
            .collect();
        BorrowWalk {
            chk: self,
            copy_ctx,
            func,
            read_borrows,
            borrowed,
        }
        .visit_block(&func.body);
    }

    fn func_param_modes(&self, func: FuncId) -> Vec<(PassMode, bool)> {
        self.program
            .functions
            .get(func.0 as usize)
            .map(|f| {
                let ctx = CopyCtx::new(&self.program.copy_types, self.copy_trait, &f.type_params);
                f.params
                    .iter()
                    .map(|p| (p.mode, ctx.is_copy(&p.ty)))
                    .collect()
            })
            .unwrap_or_default()
    }
}

/// Checks the syntactic borrow rules at each call site. Only calls (and the
/// Coerce/Match guards above) need a custom `visit_expr`; everything else uses
/// the default `Visitor` recursion.
struct BorrowWalk<'a, 'p> {
    chk: &'a mut Checker<'p>,
    /// The caller's copy-vs-move policy, so `is_copy` on argument types treats
    /// the caller's `T: Copy` bounds correctly.
    copy_ctx: CopyCtx<'p>,
    /// The function being walked — for `BlockCall` declared parameter modes.
    func: &'a super::Function,
    /// Every *read-only* borrow in scope: `read` parameters, plus the `read`/
    /// bare bindings of the match arm currently being walked. Rule 4: a `mut`
    /// borrow (at a call or `match mut`) may not go through any of these.
    read_borrows: HashSet<SymbolId>,
    /// Every borrow in scope: `read`/`mut` parameters, plus the `read`/`mut`
    /// bindings of the match arm currently being walked. Boxing any of these
    /// into a trait object is `CoerceOfBorrow`.
    borrowed: HashSet<SymbolId>,
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
            // Boxing a second-class borrow into a trait object is an error:
            // the box owns its value, so it would outlive the borrow's scope.
            ExprKind::Coerce { value, .. } => {
                if let Some(root) = root_symbol(value)
                    && self.borrowed.contains(&root)
                {
                    self.chk.emit(value.span, MoveErrorKind::CoerceOfBorrow);
                }
                walk_expr(self, value);
            }
            // A `read`/`mut` arm binding is a borrow for the arm's body — while
            // walking the body, treat it like a borrow parameter. A `match mut`
            // scrutinee is also a `mut` borrow of its root place, so rule 4
            // applies to it exactly as to a `mut` call argument: exclusive
            // write-back through a shared `read` borrow (a `read` parameter, or
            // a `read`/bare arm binding in scope) is illegal.
            ExprKind::Match {
                mode,
                scrutinee,
                arms,
            } => {
                if *mode == PassMode::Mut
                    && let Some(root) = root_symbol(scrutinee)
                    && self.read_borrows.contains(&root)
                {
                    self.chk.emit(scrutinee.span, MoveErrorKind::MutOfRead);
                }
                let mut entered: Vec<SymbolId> = Vec::new();
                let mut entered_read: Vec<SymbolId> = Vec::new();
                for arm in arms {
                    collect_pattern_borrows(
                        &self.copy_ctx,
                        &arm.pattern,
                        &mut entered,
                        &mut entered_read,
                    );
                    for &sym in &entered {
                        self.borrowed.insert(sym);
                    }
                    for &sym in &entered_read {
                        self.read_borrows.insert(sym);
                    }
                    self.visit_expr(&arm.body);
                    for &sym in &entered {
                        self.borrowed.remove(&sym);
                    }
                    for &sym in &entered_read {
                        self.read_borrows.remove(&sym);
                    }
                    entered.clear();
                    entered_read.clear();
                }
                // The scrutinee is walked too (arms' patterns bind it, but the
                // scrutinee expression itself is outside the borrow bindings).
                walk_expr_scrutinee_only(self, expr);
            }
            // A block literal's parameters are second-class borrows of the
            // elements the callee hands them — enter them for the block body
            // exactly like match-arm borrow bindings.
            ExprKind::BlockLit { params, body } => {
                let block_params: Vec<BlockParam> = match &expr.ty {
                    Type::Block(bp) => bp.clone(),
                    _ => Vec::new(),
                };
                let mut entered: Vec<SymbolId> = Vec::new();
                let mut entered_read: Vec<SymbolId> = Vec::new();
                for (lit, bp) in params.iter().zip(block_params.iter()) {
                    if self.copy_ctx.effect(bp.mode, &bp.ty) == Effect::Borrow {
                        entered.push(lit.name);
                        if bp.mode != PassMode::Mut {
                            entered_read.push(lit.name);
                        }
                    }
                }
                for &sym in &entered {
                    self.borrowed.insert(sym);
                }
                for &sym in &entered_read {
                    self.read_borrows.insert(sym);
                }
                self.visit_block(body);
                for &sym in &entered {
                    self.borrowed.remove(&sym);
                }
                for &sym in &entered_read {
                    self.read_borrows.remove(&sym);
                }
            }
            // A block invocation passes the elements to the block body; its
            // declared parameter modes come from the block parameter's type.
            ExprKind::BlockCall {
                param,
                args,
                arg_modes,
            } => {
                let block_params: Vec<BlockParam> = self
                    .func
                    .params
                    .iter()
                    .find(|p| p.name == *param)
                    .and_then(|p| match &p.ty {
                        Type::Block(bp) => Some(bp.clone()),
                        _ => None,
                    })
                    .unwrap_or_default();
                let param_modes: Vec<(PassMode, bool)> = block_params
                    .iter()
                    .map(|bp| (bp.mode, self.copy_ctx.is_copy(&bp.ty)))
                    .collect();
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
        // Rule 5: the same root place may not be `mut`-borrowed twice, nor
        // `mut` together with any other mode.
        let mut seen: HashMap<SymbolId, PassMode> = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let mode = arg_modes.get(i).copied().unwrap_or(PassMode::Read);
            if let Some(root) = root_symbol(arg)
                && let Some(prev) = seen.insert(root, mode)
                && (prev == PassMode::Mut || mode == PassMode::Mut)
            {
                self.chk.emit(arg.span, MoveErrorKind::MutAlias);
            }
            // Rule 7: a non-Copy *place* argument must be passed with the
            // parameter's declared mode — so an owned (`own`) parameter forces
            // an explicit `own` at the call, and a borrow forces `read`/`mut`.
            // Copy arguments ignore modes (keyed on the argument's actual type).
            // A temporary (rvalue — literal, constructor or call result) has no
            // place to move from, so any mode is fine.
            if let Some((decl_mode, _)) = param_modes.get(i).copied()
                && root_symbol(arg).is_some()
                && !self.copy_ctx.is_copy(&arg.ty)
                && mode != decl_mode
            {
                self.chk.emit(arg.span, MoveErrorKind::ModeMismatch);
            }
            // Rule 4: `mut`-borrowing through a read-only borrow (`read`
            // parameter, or `read`/bare arm binding in scope) is illegal.
            if mode == PassMode::Mut
                && let Some(root) = root_symbol(arg)
                && self.read_borrows.contains(&root)
            {
                self.chk.emit(arg.span, MoveErrorKind::MutOfRead);
            }
            self.visit_expr(arg);
        }
    }
}

/// Collect the `read`/`mut` (borrow) bindings a pattern introduces. Only
/// non-`Copy` bindings are real borrows; a `Copy` binding is a plain copy.
/// `read_only` receives the subset that are *read-only* borrows (mode `read` or
/// bare) — `mut` bindings are exclusive and excluded from it.
fn collect_pattern_borrows(
    copy_ctx: &CopyCtx,
    pattern: &Pattern,
    out: &mut Vec<SymbolId>,
    read_only: &mut Vec<SymbolId>,
) {
    match pattern {
        Pattern::Binding {
            symbol, ty, mode, ..
        } => {
            if copy_ctx.effect(*mode, ty) == Effect::Borrow {
                out.push(*symbol);
                if *mode != PassMode::Mut {
                    read_only.push(*symbol);
                }
            }
        }
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                collect_pattern_borrows(copy_ctx, e, out, read_only);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                collect_pattern_borrows(copy_ctx, &f.pattern, out, read_only);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

/// Walk just the scrutinee of a `match` — the default `walk_expr` would recurse
/// the arms again, but `visit_expr` already handled them with borrow bindings
/// entered.
fn walk_expr_scrutinee_only<V: Visitor>(v: &mut V, expr: &Expr) {
    if let ExprKind::Match { scrutinee, .. } = &expr.kind {
        v.visit_expr(scrutinee);
    }
}

/// Collect every non-`Copy` local bound in a body (`let` and `match`-arm
/// patterns, recursively). Parameters are added by the caller. These are exactly
/// the locals the CFG must track.
fn collect_tracked<'a>(copy_ctx: CopyCtx<'a>, block: &Block, out: &'a mut HashSet<SymbolId>) {
    TrackedCollector { copy_ctx, out }.visit_block(block);
}

/// Collect the second-class borrow bindings a function's match arms introduce:
/// a non-`Copy` binding in an arm pattern whose mode borrows (`read`, `mut`, or
/// bare). Moving one out of the arm would move a value out of the still-live
/// scrutinee, so it is unmovable for the arm's body — exactly like a borrow
/// parameter.
fn collect_borrow_bindings<'a>(
    copy_ctx: CopyCtx<'a>,
    block: &Block,
    out: &'a mut HashSet<SymbolId>,
) {
    BorrowBindingCollector { copy_ctx, out }.visit_block(block);
}

struct BorrowBindingCollector<'a> {
    copy_ctx: CopyCtx<'a>,
    out: &'a mut HashSet<SymbolId>,
}

impl Visitor for BorrowBindingCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Match {
            scrutinee, arms, ..
        } = &expr.kind
        {
            // Nested matches in the scrutinee expression are visited too.
            self.visit_expr(scrutinee);
            for arm in arms {
                borrow_binding_pattern(&self.copy_ctx, &arm.pattern, self.out);
                // Nested matches in the arm body have their own borrow
                // bindings.
                self.visit_expr(&arm.body);
            }
        } else if let ExprKind::BlockLit { params, .. } = &expr.kind {
            let block_params: Vec<BlockParam> = match &expr.ty {
                Type::Block(bp) => bp.clone(),
                _ => Vec::new(),
            };
            for (lit, bp) in params.iter().zip(block_params.iter()) {
                if self.copy_ctx.effect(bp.mode, &bp.ty) == Effect::Borrow {
                    self.out.insert(lit.name);
                }
            }
            walk_expr(self, expr);
        } else {
            walk_expr(self, expr);
        }
    }
}

/// A binding is a second-class borrow when it borrows (`read`/`mut`, or a bare
/// binding which defaults to `read`) a non-`Copy` payload. `own` bindings move
/// and are owned.
fn borrow_binding_pattern(copy_ctx: &CopyCtx, pattern: &Pattern, out: &mut HashSet<SymbolId>) {
    match pattern {
        Pattern::Binding {
            symbol, ty, mode, ..
        } => {
            if copy_ctx.effect(*mode, ty) == Effect::Borrow {
                out.insert(*symbol);
            }
        }
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                borrow_binding_pattern(copy_ctx, e, out);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                borrow_binding_pattern(copy_ctx, &f.pattern, out);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

struct TrackedCollector<'a> {
    copy_ctx: CopyCtx<'a>,
    out: &'a mut HashSet<SymbolId>,
}

impl Visitor for TrackedCollector<'_> {
    fn visit_pattern(&mut self, pattern: &Pattern) {
        tracked_pattern(&self.copy_ctx, pattern, self.out);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::BlockLit { params, .. } = &expr.kind {
            let block_params: Vec<BlockParam> = match &expr.ty {
                Type::Block(bp) => bp.clone(),
                _ => Vec::new(),
            };
            for (lit, bp) in params.iter().zip(block_params.iter()) {
                if !self.copy_ctx.is_copy(&bp.ty) {
                    self.out.insert(lit.name);
                }
            }
        }
        walk_expr(self, expr);
    }
}

/// Add every non-`Copy` binding a pattern introduces to the tracked set.
fn tracked_pattern(copy_ctx: &CopyCtx, pattern: &Pattern, out: &mut HashSet<SymbolId>) {
    match pattern {
        Pattern::Binding { symbol, ty, .. } => {
            if !copy_ctx.is_copy(ty) {
                out.insert(*symbol);
            }
        }
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                tracked_pattern(copy_ctx, e, out);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                tracked_pattern(copy_ctx, &f.pattern, out);
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
        ExprKind::Match {
            mode: _,
            scrutinee,
            arms,
        } => {
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
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => v.visit_block(b),
        ExprKind::BlockLit { body, .. } => v.visit_block(body),
        ExprKind::BlockCall { args, .. } => {
            for a in args {
                v.visit_expr(a);
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
    }
}
