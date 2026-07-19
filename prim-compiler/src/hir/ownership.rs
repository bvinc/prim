//! Stage-1 ownership / move checker.
//!
//! Runs after type checking and before monomorphization, so every `MethodCall`
//! has already been rewritten to `Call`/`DynCall`/`TraitBoundCall` (the receiver
//! is `args[0]` of a rewritten `Call`) and generic bodies are checked once with
//! `Type::Param` treated as a non-`Copy` (owned) type.
//!
//! The pass is split across a few checks, sharing one notion of "what is a move"
//! with drop elaboration via [`cfg::build`]:
//!
//!  - **Move dataflow** (`check_moves`): lower the body to a CFG and run a
//!    forward may-moved dataflow. A `Use`/`Move` of a may-moved local is a
//!    use-after-move; running the dataflow with and without loop back-edges
//!    tells a loop-carried move (`MoveInLoop`) from a straight-line one
//!    (`UseAfterMove`). Moving a `read`/`mut` parameter is a borrow escape.
//!  - **Borrow rules** (`check_borrows`): a syntactic walk over call sites for
//!    the per-call rules that don't depend on control flow — `mut` exclusivity,
//!    call-site/declaration mode match, and `mut`-of-a-`read`-parameter.
//!  - **Kind rules** (`check_view_markers`, `check_view_escapes`): the
//!    `data`/`view` kind system — a struct/enum holding a borrow must be
//!    declared `view` (`cfg::is_view`), and a nominal `view` value may be
//!    returned only with provenance (a `from <param>` clause or an elided sole
//!    borrowed parameter); otherwise it would escape the frame it borrows from.
//!
//! Both read-only walks (`check_borrows` and the `collect_tracked` helper) share
//! one structural recursion via the [`Visitor`] trait at the bottom of the file,
//! so the HIR's shape is spelled out once.
//!
//! Modes are erased after this pass; mono/codegen ignore them.

use super::cfg::{self, Action, Effect, effect, is_copy, root_symbol};
use super::{
    Block, Expr, ExprKind, FuncId, PassMode, Pattern, Program, RefKind, SpanId, Stmt, SymbolId,
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
    /// A `let` or `match` binds a non-`Copy` value out of a named place without
    /// `take`. Moving an owned value out of something that still holds it must be
    /// explicit (`take`); borrowing it out (`read`/`mut`) awaits lifetimes.
    /// Ignore it with `_` to neither.
    BindWithoutTake,
    /// A place was mutated (assigned, field-assigned, or passed `mut`/`take`)
    /// while a borrow of it was still live.
    MutateWhileBorrowed,
    /// A place was borrowed while it was already exclusively (`mut`) borrowed,
    /// or `mut`-borrowed while any borrow of it was live.
    BorrowConflict,
    /// A struct/enum's `view` marker doesn't match its fields: `declared` is what
    /// the source said (`struct view` vs plain `struct`), which disagrees with
    /// whether a field is actually view-kinded.
    ViewMarker { declared: bool },
    /// A view-kinded value (one holding a borrow) escapes the frame it borrows
    /// from — returned as a non-view type, or stored into a data-kinded place
    /// that outlives it (the four bans).
    ViewEscapes,
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
            MoveErrorKind::MutOfRead => write!(f, "cannot mutably borrow a read parameter"),
            MoveErrorKind::ConditionalDrop => write!(
                f,
                "value may be moved on only some paths; a value that implements \
                 Drop must be moved on all paths or none"
            ),
            MoveErrorKind::BindWithoutTake => write!(
                f,
                "cannot bind a non-Copy value out of a place without `own`; \
                 use `own` to move it out, or `_` to ignore it"
            ),
            MoveErrorKind::MutateWhileBorrowed => {
                write!(f, "cannot mutate a value while it is borrowed")
            }
            MoveErrorKind::BorrowConflict => {
                write!(f, "cannot borrow a value that is already borrowed")
            }
            MoveErrorKind::ViewMarker { declared } => {
                if declared {
                    write!(f, "`view` is only for a struct or enum that holds a borrow")
                } else {
                    write!(
                        f,
                        "a struct or enum holding a borrow must be declared `view`"
                    )
                }
            }
            MoveErrorKind::ViewEscapes => {
                write!(f, "a view cannot escape the frame it borrows from")
            }
        }
    }
}

impl std::error::Error for MoveError {}

pub fn check(program: &Program) -> Result<(), MoveError> {
    let mut errors = Vec::new();
    // Type definitions: the `view` marker must match the fields.
    let mut def_checker = Checker {
        program,
        errors: Vec::new(),
    };
    def_checker.check_view_markers();
    errors.append(&mut def_checker.errors);
    for func in &program.functions {
        let mut checker = Checker {
            program,
            errors: Vec::new(),
        };
        checker.check_moves(func);
        checker.check_borrows(func);
        checker.check_take_modes(func);
        checker.check_loans(func);
        checker.check_view_escapes(func);
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

/// A live borrow: the root place it borrows and whether it is shared or
/// exclusive. Held on a stack that mirrors lexical scope nesting.
#[derive(Clone, Copy)]
struct Loan {
    root: SymbolId,
    kind: RefKind,
}

/// The borrow kind a type carries, if any: a `Ref` directly, or nested inside an
/// aggregate (`Option[read T]`). `mut` wins over `read` when both appear.
fn type_ref_kind(ty: &super::Type) -> Option<RefKind> {
    use super::Type;
    let mut kind = None;
    let mut note = |k: RefKind| {
        if k == RefKind::Mut || kind.is_none() {
            kind = Some(k);
        }
    };
    fn walk(ty: &Type, note: &mut impl FnMut(RefKind)) {
        match ty {
            Type::Ref { kind, inner } => {
                note(*kind);
                walk(inner, note);
            }
            Type::Struct(_, args) | Type::Enum(_, args) => args.iter().for_each(|t| walk(t, note)),
            Type::Tuple(elems) => elems.iter().for_each(|t| walk(t, note)),
            Type::Array(elem, _) => walk(elem, note),
            Type::Pointer { pointee, .. } => walk(pointee, note),
            _ => {}
        }
    }
    walk(ty, &mut note);
    kind
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

    // ---- type-definition rules ----------------------------------------------

    /// A struct/enum must be declared `view` exactly when it holds a view-kinded
    /// field (§7, "nothing silently assigned"): the marker makes the kind — and
    /// so the escape restriction — visible at the declaration.
    fn check_view_markers(&mut self) {
        let structs = &self.program.structs;
        let enums = &self.program.enums;
        let mut wrong: Vec<(SpanId, bool)> = Vec::new();
        for s in structs {
            let holds = s.fields.iter().any(|f| cfg::is_view(&f.ty, structs, enums));
            if holds != s.is_view {
                wrong.push((s.span, s.is_view));
            }
        }
        for e in enums {
            let holds = e
                .variants
                .iter()
                .flat_map(|v| &v.fields)
                .any(|f| cfg::is_view(&f.ty, structs, enums));
            if holds != e.is_view {
                wrong.push((e.span, e.is_view));
            }
        }
        for (span, declared) in wrong {
            self.emit(span, MoveErrorKind::ViewMarker { declared });
        }
    }

    /// Ban 4 (partial): a view-kinded *nominal* value — a `view` struct/enum —
    /// may not be returned *unless* the function has provenance (a `from <param>`
    /// clause, or an elided sole borrowed parameter) for the caller to pin the
    /// source against. Without provenance the borrow would dangle. A visible
    /// `read T` / `Option[read T]` return is always fine — its provenance is
    /// handled by the loan checker directly.
    fn check_view_escapes(&mut self, func: &super::Function) {
        if self.borrow_provenance(func.id).is_some() {
            return;
        }
        ViewEscapeWalk { chk: self }.visit_block(&func.body);
        // The body's tail expression is the implicit return.
        if let Some(e) = &func.body.expr
            && self.is_nominal_view(&e.ty)
        {
            self.emit(e.span, MoveErrorKind::ViewEscapes);
        }
    }

    /// A value that is view-kinded, but whose borrow is *not* visible as a `Ref`
    /// in the type (so elision can't reach it) — i.e. a nominal `view`
    /// struct/enum. These are the returns Stage 1 rejects.
    fn is_nominal_view(&self, ty: &super::Type) -> bool {
        cfg::is_view(ty, &self.program.structs, &self.program.enums) && type_ref_kind(ty).is_none()
    }

    // ---- move dataflow ------------------------------------------------------

    /// Use-after-move, move-in-loop, and borrow-escape via a forward may-moved
    /// dataflow over the shared CFG.
    fn check_moves(&mut self, func: &super::Function) {
        // Track every non-`Copy` local: parameters plus all `let`/`match`
        // bindings. `take` params are owned (movable); `read`/`mut` params are
        // borrows that may not be moved out.
        let mut tracked: HashSet<SymbolId> = HashSet::new();
        let mut borrow_params: HashSet<SymbolId> = HashSet::new();
        for p in &func.params {
            match effect(p.mode, &p.ty) {
                // Copy params aren't tracked; `take` params are owned (movable);
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
            .filter(|p| !is_copy(&p.ty) && matches!(p.mode, PassMode::Read))
            .map(|p| p.name)
            .collect();
        BorrowWalk {
            chk: self,
            view_params,
        }
        .visit_block(&func.body);
    }

    // ---- lexical loan checker (Tier A borrows) ------------------------------

    /// Enforce shared-xor-mutable for borrows held in locals. A `let r = read x`
    /// / `let r = mut x` opens a loan of `x` that lives to the end of its
    /// enclosing block; while it is live, `x` may not be mutated (assigned,
    /// field-assigned, or passed `mut`/`take`), and an `mut` loan additionally
    /// forbids any further borrow of `x`. Lexical, so the active loans are just
    /// the stack of not-yet-closed scopes — no dataflow.
    fn check_loans(&mut self, func: &super::Function) {
        let mut active: Vec<Loan> = Vec::new();
        self.loans_block(&func.body, &mut active);
    }

    fn loans_block(&mut self, block: &Block, active: &mut Vec<Loan>) {
        let base = active.len();
        for stmt in &block.stmts {
            self.loans_stmt(stmt, active);
        }
        if let Some(e) = &block.expr {
            self.loans_expr(e, active);
        }
        // Loans opened in this block close at its end.
        active.truncate(base);
    }

    fn loans_stmt(&mut self, stmt: &Stmt, active: &mut Vec<Loan>) {
        match stmt {
            Stmt::Let { value, .. } => {
                self.loans_expr(value, active);
                match &value.kind {
                    // A direct borrow of a place opens a loan of that place.
                    ExprKind::Borrow { kind, place } => {
                        if let Some(root) = root_symbol(place) {
                            active.push(Loan { root, kind: *kind });
                        }
                    }
                    // A call that returns a borrow opens a loan of the argument
                    // the borrow comes from (elision: the sole borrowed param).
                    ExprKind::Call { func, args, .. } => {
                        if let Some((idx, kind)) = self.borrow_provenance(*func) {
                            if let Some(root) = args.get(idx).and_then(root_symbol) {
                                active.push(Loan { root, kind });
                            }
                        }
                    }
                    _ => {}
                }
            }
            Stmt::Assign { target, value, .. } => {
                self.loans_expr(value, active);
                self.check_mutate(*target, value.span, active);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.loans_expr(value, active);
                self.loans_expr(object, active);
                if let Some(root) = root_symbol(object) {
                    self.check_mutate(root, object.span, active);
                }
            }
            Stmt::DerefAssign { ptr, value, .. } => {
                self.loans_expr(ptr, active);
                self.loans_expr(value, active);
            }
            Stmt::Expr(e) => self.loans_expr(e, active),
            Stmt::Return { value: Some(e), .. } => self.loans_expr(e, active),
            Stmt::Loop { body, .. } => self.loans_block(body, active),
            Stmt::While {
                condition, body, ..
            } => {
                self.loans_expr(condition, active);
                self.loans_block(body, active);
            }
            Stmt::Return { value: None, .. } | Stmt::Break { .. } | Stmt::Drop { .. } => {}
        }
    }

    fn loans_expr(&mut self, expr: &Expr, active: &mut Vec<Loan>) {
        match &expr.kind {
            ExprKind::Borrow { kind, place } => {
                if let Some(root) = root_symbol(place) {
                    self.check_borrow(root, *kind, expr.span, active);
                }
                self.loans_expr(place, active);
            }
            ExprKind::Call {
                args, arg_modes, ..
            } => {
                for (i, a) in args.iter().enumerate() {
                    let mode = arg_modes.get(i).copied().unwrap_or(PassMode::Read);
                    if matches!(mode, PassMode::Mut | PassMode::Own) {
                        if let Some(root) = root_symbol(a) {
                            self.check_mutate(root, a.span, active);
                        }
                    }
                    self.loans_expr(a, active);
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.loans_expr(condition, active);
                self.loans_block(then_branch, active);
                if let Some(b) = else_branch {
                    self.loans_block(b, active);
                }
            }
            ExprKind::Block(b) => self.loans_block(b, active),
            ExprKind::Match { scrutinee, arms } => {
                self.loans_expr(scrutinee, active);
                for arm in arms {
                    self.loans_expr(&arm.body, active);
                }
            }
            ExprKind::Binary { left, right, .. } => {
                self.loans_expr(left, active);
                self.loans_expr(right, active);
            }
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => {
                self.loans_expr(base, active)
            }
            ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => {
                self.loans_expr(e, active)
            }
            ExprKind::Coerce { value, .. } => self.loans_expr(value, active),
            ExprKind::TupleLit(es) | ExprKind::ArrayLit(es) => {
                for e in es {
                    self.loans_expr(e, active);
                }
            }
            // Leaves and Tier-B shapes (literals, idents, struct/variant
            // literals, dyn/trait calls) carry no held borrows in Tier A.
            _ => {}
        }
    }

    /// Where a borrow-returning function's result comes from: the `(parameter
    /// index, loan kind)` a returned view is derived from. The return carries a
    /// borrow if it is view-kinded — a visible `Ref` (`-> read T`,
    /// `-> Option[read T]`) or a nominal `view` struct/enum (`-> Parser`). The
    /// provenance parameter is an explicit `from <param>` clause, else elision
    /// (the sole borrowed parameter). The loan kind is the return's own borrow
    /// kind when visible, otherwise the provenance parameter's kind (a `mut`
    /// parameter yields an exclusive loan). `None` if the return holds no borrow,
    /// or provenance is ambiguous (many borrowed parameters, no `from`).
    fn borrow_provenance(&self, func: FuncId) -> Option<(usize, RefKind)> {
        let f = self.program.functions.get(func.0 as usize)?;
        let ret = f.ret.as_ref()?;
        if !cfg::is_view(ret, &self.program.structs, &self.program.enums) {
            return None;
        }
        let idx = match f.provenance {
            Some(i) => i,
            None => {
                // Only a non-`Copy` borrowed parameter is a real borrow source;
                // a Copy parameter (e.g. `i: usize`) reads by default now but its
                // mode is a no-op, so it can't be the provenance.
                let mut borrowed = f
                    .params
                    .iter()
                    .enumerate()
                    .filter(|(_, p)| matches!(p.mode, PassMode::Read | PassMode::Mut))
                    .filter(|(_, p)| !is_copy(&p.ty));
                let (i, _) = borrowed.next()?;
                if borrowed.next().is_some() {
                    return None;
                }
                i
            }
        };
        let kind = type_ref_kind(ret).unwrap_or_else(|| match f.params.get(idx).map(|p| p.mode) {
            Some(PassMode::Mut) => RefKind::Mut,
            _ => RefKind::Read,
        });
        Some((idx, kind))
    }

    fn check_mutate(&mut self, root: SymbolId, span: SpanId, active: &[Loan]) {
        if active.iter().any(|l| l.root == root) {
            self.emit(span, MoveErrorKind::MutateWhileBorrowed);
        }
    }

    fn check_borrow(&mut self, root: SymbolId, kind: RefKind, span: SpanId, active: &[Loan]) {
        // An `mut` loan is exclusive (any further borrow conflicts); a new
        // `mut` borrow conflicts with any existing loan of the same place.
        let conflict = active
            .iter()
            .any(|l| l.root == root && (l.kind == RefKind::Mut || kind == RefKind::Mut));
        if conflict {
            self.emit(span, MoveErrorKind::BorrowConflict);
        }
    }

    fn func_param_modes(&self, func: FuncId) -> Vec<(PassMode, bool)> {
        self.program
            .functions
            .get(func.0 as usize)
            .map(|f| f.params.iter().map(|p| (p.mode, is_copy(&p.ty))).collect())
            .unwrap_or_default()
    }

    /// Binding a non-`Copy` value out of a named place must use `take`: every
    /// `match` arm (the scrutinee is the place), and every `let` whose RHS is a
    /// place. `let`s of a fresh value originate ownership and are not checked.
    fn check_take_modes(&mut self, func: &super::Function) {
        TakeModeWalk { chk: self }.visit_block(&func.body);
    }
}

/// Checks `take` modes on the binding sites that move a value out of a place:
/// `match` arm patterns, and `let` patterns over a place RHS. The default
/// `visit_pattern` no-op skips `let`s of a fresh value (handled in `visit_stmt`).
struct TakeModeWalk<'a, 'p> {
    chk: &'a mut Checker<'p>,
}

impl Visitor for TakeModeWalk<'_, '_> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        // `let` binding a non-`Copy` value *out of a named place* (a variable or
        // field the source still owns) must use `take`. Binding a fresh value (an
        // rvalue with no named root) originates ownership and needs no mode.
        if let Stmt::Let { pattern, value, .. } = stmt {
            if root_symbol(value).is_some() {
                check_binding_modes(self.chk, pattern);
            }
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Match { arms, .. } = &expr.kind {
            for arm in arms {
                check_binding_modes(self.chk, &arm.pattern);
            }
        }
        walk_expr(self, expr);
    }
}

/// Recurse a pattern, emitting `BindWithoutTake` for each non-`Copy` binding
/// that lacks `take`. Destructure nodes are walked through; only their leaf
/// bindings carry a mode.
fn check_binding_modes(chk: &mut Checker, pattern: &Pattern) {
    match pattern {
        Pattern::Binding { ty, mode, span, .. } => {
            // A non-`Copy` binding that would borrow (`read`/`mut`, or an
            // unwritten mode) instead of move is rejected: borrows out of a
            // place await lifetimes, so the only legal mode here is `take`.
            if effect(*mode, ty) == Effect::Borrow {
                chk.emit(*span, MoveErrorKind::BindWithoutTake);
            }
        }
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                check_binding_modes(chk, e);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                check_binding_modes(chk, &f.pattern);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

/// Flags each explicit `return e` of a nominal `view` value (see
/// [`Checker::check_view_escapes`]). The function-tail expression is checked
/// separately, so this only overrides `visit_stmt`.
struct ViewEscapeWalk<'a, 'p> {
    chk: &'a mut Checker<'p>,
}

impl Visitor for ViewEscapeWalk<'_, '_> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if let Stmt::Return { value: Some(e), .. } = stmt
            && self.chk.is_nominal_view(&e.ty)
        {
            self.chk.emit(e.span, MoveErrorKind::ViewEscapes);
        }
        walk_stmt(self, stmt);
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
        // Rule 5: the same root place may not be `mut`-borrowed twice, nor
        // `mut` together with any other mode.
        let mut seen: HashMap<SymbolId, PassMode> = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let mode = arg_modes.get(i).copied().unwrap_or(PassMode::Read);
            if let Some(root) = root_symbol(arg) {
                if let Some(prev) = seen.insert(root, mode) {
                    if prev == PassMode::Mut || mode == PassMode::Mut {
                        self.chk.emit(arg.span, MoveErrorKind::MutAlias);
                    }
                }
            }
            // Rule 7: a non-Copy *place* argument must be passed with the
            // parameter's declared mode — so an owned (`take`) parameter forces
            // an explicit `take` at the call, and a borrow forces `read`/`mut`.
            // Copy arguments ignore modes (keyed on the argument's actual type).
            // A temporary (rvalue — literal, constructor or call result) has no
            // place to move from, so any mode is fine.
            if let Some((decl_mode, _)) = param_modes.get(i).copied() {
                if root_symbol(arg).is_some() && !is_copy(&arg.ty) && mode != decl_mode {
                    self.chk.emit(arg.span, MoveErrorKind::ModeMismatch);
                }
            }
            // Rule 4: `mut`-borrowing through a `read` parameter is illegal.
            if mode == PassMode::Mut {
                if let Some(root) = root_symbol(arg) {
                    if self.view_params.contains(&root) {
                        self.chk.emit(arg.span, MoveErrorKind::MutOfRead);
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
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
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
        ExprKind::Borrow { place, .. } => v.visit_expr(place),
        ExprKind::Coerce { value, .. } => v.visit_expr(value),
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
        | ExprKind::ConstParam(_)
        | ExprKind::Spawn { .. }
        | ExprKind::Error => {}
    }
}
