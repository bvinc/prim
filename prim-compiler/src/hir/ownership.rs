//! Stage-1 ownership / move checker.
//!
//! Runs after type checking and before monomorphization, so every `MethodCall`
//! has already been rewritten to `Call`/`DynCall`/`TraitBoundCall` (the receiver
//! is `args[0]` of a rewritten `Call`) and generic bodies are checked once with
//! `Type::Param` treated as a non-`Copy` (owned) type.
//!
//! The discipline, with no lifetimes (borrows are call-scoped and cannot
//! escape):
//!
//! - **Move semantics** for non-`Copy` aggregates. A value is *moved* when it is
//!   bound (`let b = a`), assigned, returned, stored into an aggregate literal,
//!   passed as a `take` argument, or matched-with-payload-extraction. After a
//!   move the source is dead; using it again is an error (rule 1).
//! - **Borrows can't escape** (rule 6): a `view`/`edit` parameter may not be
//!   moved out of the function.
//! - **`edit` exclusivity** (rule 5): the same place may not be `edit`-borrowed
//!   twice (nor `edit` together with another mode) in one call.
//! - **Mode match** (rule 7): a non-`Copy` argument's call-site mode must equal
//!   the callee parameter's declared mode. `Copy` params accept any mode.
//! - **`edit` of a `view` parameter** (rule 4): you may not `edit`-borrow a
//!   value reachable only through a shared (`view`) parameter.
//!
//! Modes are erased after this pass; mono/codegen ignore them.

use super::{
    Block, Expr, ExprKind, FuncId, PassMode, Pattern, Program, SpanId, Stmt, SymbolId, Type,
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

/// Whether a value of this type is `Copy` (passed by value, ignores modes,
/// never tracked for moves). Scalars and raw pointers are `Copy`; aggregates
/// and type parameters (conservatively) are not.
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

#[derive(Clone, Copy)]
enum LocalState {
    Live,
    Moved(SpanId),
}

/// Per-function move-checking state. `borrow_params`/`view_params` are fixed for
/// the function; `env` flows through the walk.
struct Checker<'a> {
    program: &'a Program,
    errors: Vec<MoveError>,
    env: HashMap<SymbolId, LocalState>,
    /// `view`/`edit` parameters — moving any of these escapes the borrow.
    borrow_params: HashSet<SymbolId>,
    /// `view` parameters specifically — may not be `edit`-borrowed (rule 4).
    view_params: HashSet<SymbolId>,
    /// When re-walking a loop body, the symbols moved by the first iteration;
    /// a use/move of one of these is reported as `MoveInLoop`.
    loop_moved: HashSet<SymbolId>,
}

pub fn check(program: &Program) -> Result<(), MoveError> {
    let mut errors = Vec::new();
    for func in &program.functions {
        let mut checker = Checker {
            program,
            errors: Vec::new(),
            env: HashMap::new(),
            borrow_params: HashSet::new(),
            view_params: HashSet::new(),
            loop_moved: HashSet::new(),
        };
        checker.check_function(func);
        errors.append(&mut checker.errors);
    }
    // Report deterministically: earliest span first.
    errors.sort_by_key(|e| (e.span.start(), e.span.end()));
    match errors.into_iter().next() {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

impl<'a> Checker<'a> {
    fn check_function(&mut self, func: &super::Function) {
        for p in &func.params {
            if is_copy(&p.ty) {
                continue;
            }
            self.env.insert(p.name, LocalState::Live);
            match p.mode {
                PassMode::View => {
                    self.borrow_params.insert(p.name);
                    self.view_params.insert(p.name);
                }
                PassMode::Edit => {
                    self.borrow_params.insert(p.name);
                }
                // `take` params are owned and may be moved freely.
                PassMode::Take => {}
            }
        }
        self.walk_block(&func.body);
    }

    fn span_of(&self, span_id: SpanId) -> (FileId, Span) {
        self.program
            .spans
            .get(span_id.0 as usize)
            .copied()
            .expect("missing span")
    }

    fn emit(&mut self, span_id: SpanId, kind: MoveErrorKind) {
        let (file, span) = self.span_of(span_id);
        self.errors.push(MoveError { file, span, kind });
    }

    // ---- walking ------------------------------------------------------------

    /// Walk a block, returning whether it always diverges (returns/breaks).
    /// Inner `let` bindings persist in `env` (shadowing is by fresh `SymbolId`).
    fn walk_block(&mut self, block: &Block) -> bool {
        for stmt in &block.stmts {
            if self.walk_stmt(stmt) {
                return true;
            }
        }
        if let Some(e) = &block.expr {
            return self.eval(e);
        }
        false
    }

    fn walk_stmt(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                if self.eval_moved(value) {
                    return true;
                }
                self.bind_pattern(pattern);
                false
            }
            Stmt::Assign { target, value, .. } => {
                if self.eval_moved(value) {
                    return true;
                }
                // Reassignment revives the binding (if tracked).
                if self.env.contains_key(target) {
                    self.env.insert(*target, LocalState::Live);
                }
                false
            }
            Stmt::DerefAssign { ptr, value, .. } => {
                if self.eval(ptr) {
                    return true;
                }
                self.eval_moved(value)
            }
            Stmt::FieldAssign { object, value, .. } => {
                // Writing a field reads the base (it must be live) and moves the
                // new value into the field.
                if self.eval(object) {
                    return true;
                }
                self.eval_moved(value)
            }
            Stmt::Expr(e) => self.eval(e),
            Stmt::Loop { body, .. } => {
                self.walk_loop_body(body);
                false
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.eval(condition);
                self.walk_loop_body(body);
                false
            }
            Stmt::Break { .. } => true,
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.eval_moved(v);
                }
                true
            }
            // Drop elaboration runs after this pass; no Drop statements exist.
            Stmt::Drop { .. } => false,
        }
    }

    /// Rule 3: walk a loop body, then flag any outer symbol it moves — on the
    /// next iteration that symbol would already be dead.
    fn walk_loop_body(&mut self, body: &Block) {
        let before: HashMap<SymbolId, bool> = self
            .env
            .iter()
            .map(|(k, v)| (*k, matches!(v, LocalState::Live)))
            .collect();
        self.walk_block(body);
        let mut looped = HashSet::new();
        for (sym, was_live) in &before {
            if *was_live {
                if let Some(LocalState::Moved(span_id)) = self.env.get(sym).copied() {
                    looped.insert(*sym);
                    self.emit(span_id, MoveErrorKind::MoveInLoop);
                }
            }
        }
        self.loop_moved.extend(looped);
    }

    /// Evaluate `expr` in read/borrow context, returning whether it diverges.
    fn eval(&mut self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Spawn { .. }
            | ExprKind::Error => false,
            ExprKind::Ident(sym) => {
                self.check_live(*sym, expr.span);
                false
            }
            ExprKind::Binary { left, right, .. } => self.eval(left) || self.eval(right),
            ExprKind::BitNot(e) | ExprKind::Neg(e) | ExprKind::Deref(e) => self.eval(e),
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => self.eval(base),
            ExprKind::Dbg { inner, .. } => self.eval(inner),
            // Coercing a struct into a trait fat pointer references the struct's
            // data rather than copying it, so it is a borrow, not a move.
            ExprKind::Coerce { value, .. } => self.eval(value),
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                for e in elems {
                    if self.eval_moved(e) {
                        return true;
                    }
                }
                false
            }
            ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
                for (_, v) in fields {
                    if self.eval_moved(v) {
                        return true;
                    }
                }
                false
            }
            ExprKind::Call {
                func,
                args,
                arg_modes,
                ..
            } => self.eval_call(*func, args, arg_modes),
            ExprKind::DynCall {
                receiver,
                trait_id,
                method_idx,
                args,
                arg_modes,
            } => {
                let modes = self
                    .program
                    .traits
                    .get(trait_id.0 as usize)
                    .and_then(|t| t.methods.get(*method_idx as usize))
                    .map(|m| m.param_modes.clone())
                    .unwrap_or_default();
                self.eval_dispatch(receiver, args, arg_modes, &modes)
            }
            ExprKind::TraitBoundCall {
                receiver,
                bound,
                method,
                args,
                arg_modes,
                ..
            } => {
                let modes = self
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
                self.eval_dispatch(receiver, args, arg_modes, &modes)
            }
            // Typecheck rewrites every `MethodCall` to `Call`/`DynCall`/
            // `TraitBoundCall` before this pass runs; handle it defensively as a
            // plain read of receiver + args so the match stays exhaustive.
            ExprKind::MethodCall { receiver, args, .. } => {
                if self.eval(receiver) {
                    return true;
                }
                for a in args {
                    if self.eval(a) {
                        return true;
                    }
                }
                false
            }
            ExprKind::Match { scrutinee, arms } => self.eval_match(scrutinee, arms),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.eval(condition) {
                    return true;
                }
                let base = self.env.clone();
                let then_div = self.walk_block(then_branch);
                let then_env = std::mem::replace(&mut self.env, base.clone());
                let else_div = match else_branch {
                    Some(b) => self.walk_block(b),
                    None => false,
                };
                let else_env = std::mem::replace(&mut self.env, base);
                self.env = join_envs(&then_env, &else_env, then_div, else_div);
                then_div && else_div
            }
            ExprKind::Block(b) => self.walk_block(b),
        }
    }

    /// Evaluate `expr` where its value is consumed (moved) by the context.
    /// Differs from `eval` only for non-`Copy` place expressions.
    fn eval_moved(&mut self, expr: &Expr) -> bool {
        if is_copy(&expr.ty) {
            return self.eval(expr);
        }
        match &expr.kind {
            ExprKind::Ident(sym) => {
                self.move_symbol(*sym, expr.span);
                false
            }
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => {
                self.move_place(expr, base);
                false
            }
            // Any other shape produces a fresh temporary; moving it is a no-op
            // on the environment, but its subexpressions still need walking.
            _ => self.eval(expr),
        }
    }

    fn eval_call(&mut self, func: FuncId, args: &[Expr], arg_modes: &[PassMode]) -> bool {
        let param_modes: Vec<(PassMode, bool)> = self
            .program
            .functions
            .get(func.0 as usize)
            .map(|f| f.params.iter().map(|p| (p.mode, is_copy(&p.ty))).collect())
            .unwrap_or_default();
        self.check_call_args(args, arg_modes, &param_modes)
    }

    /// A dynamically/bound-dispatched call: the receiver is separate, and
    /// `trait_modes` is the trait method signature's modes (receiver at 0).
    fn eval_dispatch(
        &mut self,
        receiver: &Expr,
        args: &[Expr],
        arg_modes: &[PassMode],
        trait_modes: &[PassMode],
    ) -> bool {
        let recv_mode = trait_modes.first().copied().unwrap_or(PassMode::View);
        if self.move_arg(receiver, recv_mode) {
            return true;
        }
        // Non-receiver params are `trait_modes[1..]`.
        let param_modes: Vec<(PassMode, bool)> =
            trait_modes.iter().skip(1).map(|m| (*m, false)).collect();
        self.check_call_args(args, arg_modes, &param_modes)
    }

    /// Shared argument-list checking: rules 4, 5, 7, plus the per-argument move
    /// (for `take`) or read (for `view`/`edit`).
    fn check_call_args(
        &mut self,
        args: &[Expr],
        arg_modes: &[PassMode],
        param_modes: &[(PassMode, bool)],
    ) -> bool {
        // Rule 5: the same root place may not be `edit`-borrowed twice, nor
        // `edit` together with any other mode.
        let mut seen: HashMap<SymbolId, PassMode> = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let mode = arg_modes.get(i).copied().unwrap_or(PassMode::View);
            if let Some(root) = root_symbol(arg) {
                if let Some(prev) = seen.insert(root, mode) {
                    if prev == PassMode::Edit || mode == PassMode::Edit {
                        self.emit(arg.span, MoveErrorKind::EditAlias);
                    }
                }
            }
            // Rule 7: non-Copy params must be passed with the declared mode.
            if let Some((decl_mode, copy)) = param_modes.get(i).copied() {
                if !copy && mode != decl_mode {
                    self.emit(arg.span, MoveErrorKind::ModeMismatch);
                }
            }
            // Rule 4: `edit`-borrowing through a `view` parameter is illegal.
            if mode == PassMode::Edit {
                if let Some(root) = root_symbol(arg) {
                    if self.view_params.contains(&root) {
                        self.emit(arg.span, MoveErrorKind::EditOfView);
                    }
                }
            }
            if self.move_arg(arg, mode) {
                return true;
            }
        }
        false
    }

    /// Walk a single argument: `take` moves it, `view`/`edit` read-borrow it.
    fn move_arg(&mut self, arg: &Expr, mode: PassMode) -> bool {
        match mode {
            PassMode::Take => self.eval_moved(arg),
            PassMode::View | PassMode::Edit => self.eval(arg),
        }
    }

    fn eval_match(&mut self, scrutinee: &Expr, arms: &[super::MatchArm]) -> bool {
        // A match consumes its scrutinee only when an arm binds a non-`Copy`
        // payload out of it; a discriminant-only match (`_`/`Copy` bindings) is
        // a read-borrow, so `view` values can still be matched.
        let consumes = arms.iter().any(|a| pattern_binds_noncopy(&a.pattern));
        if consumes {
            if self.eval_moved(scrutinee) {
                return true;
            }
        } else if self.eval(scrutinee) {
            return true;
        }

        let base = self.env.clone();
        let mut arm_envs: Vec<(HashMap<SymbolId, LocalState>, bool)> = Vec::new();
        for arm in arms {
            self.env = base.clone();
            self.bind_pattern(&arm.pattern);
            let div = self.eval(&arm.body);
            arm_envs.push((std::mem::take(&mut self.env), div));
        }
        // Join all non-diverging arms; if every arm diverges, so does the match.
        let all_diverge = !arm_envs.is_empty() && arm_envs.iter().all(|(_, d)| *d);
        let mut result = base;
        let mut first = true;
        for (env, div) in &arm_envs {
            if *div {
                continue;
            }
            if first {
                result = env.clone();
                first = false;
            } else {
                result = join_envs(env, &result, false, false);
            }
        }
        self.env = result;
        all_diverge
    }

    // ---- moves & reads ------------------------------------------------------

    fn check_live(&mut self, sym: SymbolId, span_id: SpanId) {
        if let Some(LocalState::Moved(moved_id)) = self.env.get(&sym).copied() {
            self.emit_use_after_move(sym, span_id, moved_id);
        }
    }

    fn emit_use_after_move(&mut self, sym: SymbolId, span_id: SpanId, moved_id: SpanId) {
        let kind = if self.loop_moved.contains(&sym) {
            MoveErrorKind::MoveInLoop
        } else {
            let (_, moved_at) = self.span_of(moved_id);
            MoveErrorKind::UseAfterMove { moved_at }
        };
        self.emit(span_id, kind);
    }

    fn move_symbol(&mut self, sym: SymbolId, span_id: SpanId) {
        if self.borrow_params.contains(&sym) {
            self.emit(span_id, MoveErrorKind::BorrowEscapes);
            return;
        }
        match self.env.get(&sym).copied() {
            Some(LocalState::Moved(moved_id)) => {
                self.emit_use_after_move(sym, span_id, moved_id);
            }
            Some(LocalState::Live) => {
                self.env.insert(sym, LocalState::Moved(span_id));
            }
            None => {} // untracked (Copy / global) — nothing to do
        }
    }

    /// Rule 8: move a non-`Copy` field/payload out of `place` (whose base chain
    /// is `base`). Allowed only from a fully owned, live local — it consumes the
    /// whole base. Moving out of a borrow is rejected.
    fn move_place(&mut self, place: &Expr, base: &Expr) {
        match root_symbol(place) {
            Some(root) if self.env.contains_key(&root) => {
                if self.borrow_params.contains(&root) {
                    self.emit(place.span, MoveErrorKind::MoveOutOfBorrow);
                } else {
                    self.move_symbol(root, place.span);
                }
            }
            // Base is a temporary (e.g. `foo().field`): moving out is fine, but
            // the base still needs walking for nested effects.
            _ => {
                self.eval(base);
            }
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Binding { symbol, ty, .. } => {
                if !is_copy(ty) {
                    self.env.insert(*symbol, LocalState::Live);
                }
            }
            Pattern::Tuple { elems, .. } => {
                for e in elems {
                    self.bind_pattern(e);
                }
            }
            Pattern::Variant { fields, .. } => {
                for f in fields {
                    self.bind_pattern(&f.pattern);
                }
            }
            Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
        }
    }

    // ---- span helpers for loop reporting ------------------------------------
}

/// The root local/parameter a place expression is rooted at, following
/// field/tuple projections. `None` for anything not rooted at a bare binding
/// (e.g. a temporary, a deref, or a call result).
fn root_symbol(expr: &Expr) -> Option<SymbolId> {
    match &expr.kind {
        ExprKind::Ident(sym) => Some(*sym),
        ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => root_symbol(base),
        _ => None,
    }
}

/// Whether a pattern binds at least one non-`Copy` value by name (which forces
/// a move out of the matched scrutinee).
fn pattern_binds_noncopy(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Binding { ty, .. } => !is_copy(ty),
        Pattern::Tuple { elems, .. } => elems.iter().any(pattern_binds_noncopy),
        Pattern::Variant { fields, .. } => fields.iter().any(|f| pattern_binds_noncopy(&f.pattern)),
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => false,
    }
}

/// Join two control-flow branches: a symbol is `Moved` after the join if it is
/// `Moved` on any branch that can reach it. A diverging branch contributes
/// nothing.
fn join_envs(
    a: &HashMap<SymbolId, LocalState>,
    b: &HashMap<SymbolId, LocalState>,
    a_div: bool,
    b_div: bool,
) -> HashMap<SymbolId, LocalState> {
    if a_div {
        return b.clone();
    }
    if b_div {
        return a.clone();
    }
    let mut out = a.clone();
    for (sym, state) in b {
        match (out.get(sym).copied(), state) {
            (Some(LocalState::Moved(s)), _) => {
                out.insert(*sym, LocalState::Moved(s));
            }
            (_, LocalState::Moved(s)) => {
                out.insert(*sym, LocalState::Moved(*s));
            }
            (Some(LocalState::Live), LocalState::Live) => {}
            (None, LocalState::Live) => {
                out.insert(*sym, LocalState::Live);
            }
        }
    }
    out
}
