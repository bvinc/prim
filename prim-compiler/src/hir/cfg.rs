//! A small control-flow graph over a function body's ownership-relevant events.
//!
//! This is the *single source of truth* for where values are moved: both the
//! Stage-1 ownership checker (`ownership.rs`, pre-mono) and drop elaboration
//! (`drop_elab.rs`, post-mono) lower a function body to this CFG via [`build`]
//! and then run their own dataflow over it, so the two passes can never disagree
//! about what counts as a move.
//!
//! The CFG is intentionally minimal: basic blocks hold a sequence of `Action`s
//! (a local becomes owned, is read, is moved out, or a candidate drop), and each
//! block ends in a `Terminator` that names its successors. Control-flow
//! conditions are *not* modelled — only the edges — which is all the move
//! analyses need. Loop back-edges are recorded separately so a consumer can tell
//! a loop-carried move from a straight-line one.
//!
//! Two dataflows run over the graph:
//!   - [`analyze`] decides drop placement (forward may/must-moved),
//!   - [`may_in_sets`] gives the may-moved set entering each block, which the
//!     ownership checker uses (with and without back-edges) for use-after-move.

use super::{
    Block as HirBlock, Enum, Expr, ExprKind, MatchArm, PassMode, Pattern, RefKind, SpanId, Stmt,
    Struct, SymbolId, Type,
};
use std::collections::{HashMap, HashSet};

pub type BlockId = usize;
pub type DropId = usize;

/// An ownership-relevant event within a basic block, in execution order.
#[derive(Clone, Copy, Debug)]
pub enum Action {
    /// `local` becomes owned: a `let`/`match`-arm binding or an assignment.
    Init(SymbolId),
    /// `local` is read in place (borrowed) at `span` — used for use-after-move.
    Use { local: SymbolId, span: SpanId },
    /// `local`'s value is moved out at `span`. `partial` is set when the move is
    /// of a field/element projection (`x.f`) rather than the whole binding `x`.
    Move {
        local: SymbolId,
        span: SpanId,
        partial: bool,
    },
    /// A candidate drop of `local` at a scope exit. `id` ties it to the HIR
    /// `Stmt::Drop` emitted in lockstep during lowering.
    Drop { id: DropId, local: SymbolId },
}

impl Action {
    /// The local this action concerns (for collecting the dataflow universe).
    fn local(&self) -> SymbolId {
        match self {
            Action::Init(l)
            | Action::Use { local: l, .. }
            | Action::Move { local: l, .. }
            | Action::Drop { local: l, .. } => *l,
        }
    }
}

/// How a block hands control to its successors.
#[derive(Clone, Debug)]
pub enum Terminator {
    /// One successor (fall-through / unconditional jump).
    Goto(BlockId),
    /// Two or more successors (an `if`/`else` or `match`). Condition not modeled.
    Switch(Vec<BlockId>),
    /// Function return — no successors.
    Return,
    /// Unreachable (after a diverging call or an endless loop) — no successors.
    Unreachable,
}

impl Terminator {
    fn successors(&self) -> &[BlockId] {
        match self {
            Terminator::Goto(b) => std::slice::from_ref(b),
            Terminator::Switch(bs) => bs,
            Terminator::Return | Terminator::Unreachable => &[],
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub actions: Vec<Action>,
    pub term: Terminator,
}

impl Block {
    pub fn new() -> Self {
        Block {
            actions: Vec::new(),
            // A block under construction defaults to unreachable until its real
            // terminator is set (so a forgotten terminator can't fall through).
            term: Terminator::Unreachable,
        }
    }
}

impl Default for Block {
    fn default() -> Self {
        Block::new()
    }
}

/// A function body's CFG. Block 0 is the entry.
#[derive(Debug, Default)]
pub struct Cfg {
    pub blocks: Vec<Block>,
    /// Edges `(from, to)` that close a loop (a body's back-edge to its header).
    /// Recorded so the move analysis can distinguish a loop-carried move from a
    /// straight-line one without a separate dominator computation.
    pub back_edges: HashSet<(BlockId, BlockId)>,
}

impl Cfg {
    pub fn new() -> Self {
        Cfg::default()
    }

    pub fn add_block(&mut self) -> BlockId {
        self.blocks.push(Block::new());
        self.blocks.len() - 1
    }

    pub fn block(&mut self, id: BlockId) -> &mut Block {
        &mut self.blocks[id]
    }
}

/// The decision for one candidate drop after dataflow.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DropDecision {
    /// The value is still owned on every path here — emit the drop.
    Keep,
    /// The value was moved out on every path here — omit the drop.
    Remove,
    /// The value was moved on some paths but not others — not statically
    /// droppable; the caller reports this as an error.
    Conditional,
}

// === Drop dataflow: decide every candidate drop ===

/// Run the move dataflow and decide every candidate drop.
pub fn analyze(cfg: &Cfg) -> HashMap<DropId, DropDecision> {
    let n = cfg.blocks.len();
    let mut decisions = HashMap::new();
    if n == 0 {
        return decisions;
    }

    let preds = predecessors(cfg, true);

    // The universe of locals, for the must-moved (∩) lattice's top element.
    let universe: HashSet<SymbolId> = cfg
        .blocks
        .iter()
        .flat_map(|b| b.actions.iter())
        .map(Action::local)
        .collect();

    // Block-exit sets. `may` starts empty (⊥ for ∪); `must` starts as the full
    // universe (⊤ for ∩) so intersection can shrink toward the fixpoint.
    let mut exit_may: Vec<HashSet<SymbolId>> = vec![HashSet::new(); n];
    let mut exit_must: Vec<HashSet<SymbolId>> = vec![universe.clone(); n];

    // Iterate to a fixpoint. The lattices have finite height, so this
    // terminates; for a structured (reducible) CFG it converges quickly.
    // Indexed loops: the dataflow reads other blocks' exit state by id, so
    // random access is the natural form.
    #[allow(clippy::needless_range_loop)]
    loop {
        let mut changed = false;
        for b in 0..n {
            let (may_in, must_in) = drop_entry(&preds[b], &exit_may, &exit_must);
            let (may_out, must_out) = drop_transfer(&cfg.blocks[b], may_in, must_in, None);
            if may_out != exit_may[b] || must_out != exit_must[b] {
                exit_may[b] = may_out;
                exit_must[b] = must_out;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // Final pass: replay each block from its fixed entry state, recording a
    // decision at every `Drop`.
    #[allow(clippy::needless_range_loop)]
    for b in 0..n {
        let (may_in, must_in) = drop_entry(&preds[b], &exit_may, &exit_must);
        drop_transfer(&cfg.blocks[b], may_in, must_in, Some(&mut decisions));
    }
    decisions
}

/// The dataflow state entering a block: ∪ of predecessors' may-moved, ∩ of
/// their must-moved. A block with no predecessors starts with nothing moved.
fn drop_entry(
    preds: &[BlockId],
    exit_may: &[HashSet<SymbolId>],
    exit_must: &[HashSet<SymbolId>],
) -> (HashSet<SymbolId>, HashSet<SymbolId>) {
    if preds.is_empty() {
        return (HashSet::new(), HashSet::new());
    }
    let mut may = HashSet::new();
    for &p in preds {
        may.extend(exit_may[p].iter().copied());
    }
    let mut must = exit_must[preds[0]].clone();
    for &p in &preds[1..] {
        must.retain(|x| exit_must[p].contains(x));
    }
    (may, must)
}

/// Apply a block's actions to the incoming state, returning the outgoing state.
/// When `decisions` is given, record a `DropDecision` at each `Drop`.
fn drop_transfer(
    block: &Block,
    mut may: HashSet<SymbolId>,
    mut must: HashSet<SymbolId>,
    mut decisions: Option<&mut HashMap<DropId, DropDecision>>,
) -> (HashSet<SymbolId>, HashSet<SymbolId>) {
    for action in &block.actions {
        match *action {
            Action::Init(l) => {
                // (Re)initialization revives the local: it is no longer moved.
                may.remove(&l);
                must.remove(&l);
            }
            Action::Move { local, .. } => {
                may.insert(local);
                must.insert(local);
            }
            Action::Drop { id, local } => {
                if let Some(decisions) = decisions.as_deref_mut() {
                    let decision = if must.contains(&local) {
                        DropDecision::Remove
                    } else if may.contains(&local) {
                        DropDecision::Conditional
                    } else {
                        DropDecision::Keep
                    };
                    decisions.insert(id, decision);
                }
            }
            Action::Use { .. } => {} // reads don't change move state
        }
    }
    (may, must)
}

// === May-moved dataflow: the set of locals moved entering each block ===

/// For every block, the set of locals that are moved on at least one path
/// reaching its entry (the ∪ may-moved lattice). When `include_back_edges` is
/// false, loop back-edges are dropped from the graph first, so the result
/// reflects only straight-line (within-iteration) flow — the difference between
/// the two runs is exactly the loop-carried moves.
pub fn may_in_sets(cfg: &Cfg, include_back_edges: bool) -> Vec<HashSet<SymbolId>> {
    let n = cfg.blocks.len();
    if n == 0 {
        return Vec::new();
    }
    let preds = predecessors(cfg, include_back_edges);
    let mut exit: Vec<HashSet<SymbolId>> = vec![HashSet::new(); n];

    #[allow(clippy::needless_range_loop)]
    loop {
        let mut changed = false;
        for b in 0..n {
            let mut out = may_entry(&preds[b], &exit);
            for action in &cfg.blocks[b].actions {
                match *action {
                    Action::Init(l) => {
                        out.remove(&l);
                    }
                    Action::Move { local, .. } => {
                        out.insert(local);
                    }
                    Action::Use { .. } | Action::Drop { .. } => {}
                }
            }
            if out != exit[b] {
                exit[b] = out;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    (0..n).map(|b| may_entry(&preds[b], &exit)).collect()
}

fn may_entry(preds: &[BlockId], exit: &[HashSet<SymbolId>]) -> HashSet<SymbolId> {
    let mut may = HashSet::new();
    for &p in preds {
        may.extend(exit[p].iter().copied());
    }
    may
}

/// Predecessor lists, inverted from successors. When `include_back_edges` is
/// false, edges recorded in `cfg.back_edges` are skipped.
fn predecessors(cfg: &Cfg, include_back_edges: bool) -> Vec<Vec<BlockId>> {
    let n = cfg.blocks.len();
    let mut preds: Vec<Vec<BlockId>> = vec![Vec::new(); n];
    for (b, block) in cfg.blocks.iter().enumerate() {
        for &s in block.term.successors() {
            if !include_back_edges && cfg.back_edges.contains(&(b, s)) {
                continue;
            }
            preds[s].push(b);
        }
    }
    preds
}

// === Lowering: HIR body → CFG ===

/// Lower a function body to a CFG, recording moves/uses/inits/drops for the
/// `tracked` locals (the ownership pass tracks every non-`Copy` local; drop
/// elaboration tracks the droppable ones). Each `Stmt::Drop`, if present,
/// becomes a `Drop` action carrying the node's own id.
pub fn build(body: &HirBlock, tracked: &HashSet<SymbolId>) -> Cfg {
    let mut b = Builder {
        cfg: Cfg::new(),
        current: 0,
        loop_exits: Vec::new(),
        tracked,
    };
    b.cfg.add_block(); // entry = block 0
    b.block(body);
    // The body falls through to an implicit return.
    if matches!(b.cfg.blocks[b.current].term, Terminator::Unreachable) {
        b.cfg.blocks[b.current].term = Terminator::Return;
    }
    b.cfg
}

struct Builder<'a> {
    cfg: Cfg,
    current: BlockId,
    /// Exit block of each enclosing loop (innermost last) — a `break` target.
    loop_exits: Vec<BlockId>,
    tracked: &'a HashSet<SymbolId>,
}

impl Builder<'_> {
    fn act(&mut self, action: Action) {
        self.cfg.blocks[self.current].actions.push(action);
    }

    fn set_term(&mut self, term: Terminator) {
        self.cfg.blocks[self.current].term = term;
    }

    /// Set the current block's terminator to `Goto(target)` unless it already
    /// diverged via an inner return/break.
    fn goto(&mut self, target: BlockId) {
        if matches!(self.cfg.blocks[self.current].term, Terminator::Unreachable) {
            self.cfg.blocks[self.current].term = Terminator::Goto(target);
        }
    }

    /// Like `goto`, but records the edge as a loop back-edge.
    fn back_edge(&mut self, header: BlockId) {
        if matches!(self.cfg.blocks[self.current].term, Terminator::Unreachable) {
            self.cfg.blocks[self.current].term = Terminator::Goto(header);
            self.cfg.back_edges.insert((self.current, header));
        }
    }

    fn block(&mut self, block: &HirBlock) {
        for stmt in &block.stmts {
            self.stmt(stmt);
        }
        if let Some(e) = &block.expr {
            self.read(e);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                self.moved(value);
                self.init_pattern(pattern);
            }
            Stmt::Assign { target, value, .. } => {
                self.moved(value);
                if self.tracked.contains(target) {
                    self.act(Action::Init(*target));
                }
            }
            Stmt::DerefAssign { ptr, value, .. } => {
                self.read(ptr);
                self.moved(value);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.read(object);
                self.moved(value);
            }
            Stmt::Expr(e) => self.read(e),
            Stmt::Loop { body, .. } => {
                let header = self.cfg.add_block();
                let exit = self.cfg.add_block();
                self.goto(header);
                self.loop_exits.push(exit);
                self.current = header;
                self.block(body);
                self.back_edge(header);
                self.loop_exits.pop();
                self.current = exit;
            }
            Stmt::While {
                condition, body, ..
            } => {
                let header = self.cfg.add_block();
                let body_b = self.cfg.add_block();
                let exit = self.cfg.add_block();
                self.goto(header);
                self.current = header;
                self.read(condition);
                self.set_term(Terminator::Switch(vec![body_b, exit]));
                self.loop_exits.push(exit);
                self.current = body_b;
                self.block(body);
                self.back_edge(header);
                self.loop_exits.pop();
                self.current = exit;
            }
            Stmt::Break { .. } => {
                let target = *self.loop_exits.last().expect("break outside loop");
                self.set_term(Terminator::Goto(target));
                self.current = self.cfg.add_block(); // dead code after break
            }
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.moved(v);
                }
                self.set_term(Terminator::Return);
                self.current = self.cfg.add_block(); // dead code after return
            }
            Stmt::Drop { sym, id, .. } => {
                self.act(Action::Drop {
                    id: *id,
                    local: *sym,
                });
            }
        }
    }

    /// Emit an `Init` for each tracked binding introduced by a pattern.
    fn init_pattern(&mut self, pattern: &Pattern) {
        let mut binds = Vec::new();
        pattern_bindings(pattern, &mut binds);
        for sym in binds {
            if self.tracked.contains(&sym) {
                self.act(Action::Init(sym));
            }
        }
    }

    /// An expression in move position: a tracked place is moved out; anything
    /// else (a temporary, or a `Copy` value) is just read for its sub-effects.
    fn moved(&mut self, expr: &Expr) {
        if !is_copy(&expr.ty) {
            if let Some(root) = root_symbol(expr) {
                if self.tracked.contains(&root) {
                    let partial = !matches!(expr.kind, ExprKind::Ident(_));
                    self.act(Action::Move {
                        local: root,
                        span: expr.span,
                        partial,
                    });
                }
                return;
            }
        }
        self.read(expr);
    }

    /// An expression in read/borrow position: recurse, emitting `Use` for tracked
    /// place roots and moves from move-position children, and splitting the CFG
    /// at `if`/`match`.
    fn read(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(sym) => {
                if self.tracked.contains(sym) {
                    self.act(Action::Use {
                        local: *sym,
                        span: expr.span,
                    });
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                // A matched non-`Copy` scrutinee is consumed only when an arm
                // binds a non-`Copy` payload out of it; otherwise it is borrowed.
                if match_consumes(arms) {
                    self.moved(scrutinee);
                } else {
                    self.read(scrutinee);
                }
                let join = self.cfg.add_block();
                let mut arm_blocks = Vec::with_capacity(arms.len());
                for _ in arms {
                    arm_blocks.push(self.cfg.add_block());
                }
                self.set_term(Terminator::Switch(arm_blocks.clone()));
                for (arm, &b) in arms.iter().zip(&arm_blocks) {
                    self.current = b;
                    self.init_pattern(&arm.pattern);
                    self.read(&arm.body);
                    self.goto(join);
                }
                self.current = join;
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.read(condition);
                let then_b = self.cfg.add_block();
                let else_b = self.cfg.add_block();
                let join = self.cfg.add_block();
                self.set_term(Terminator::Switch(vec![then_b, else_b]));
                self.current = then_b;
                self.block(then_branch);
                self.goto(join);
                self.current = else_b;
                if let Some(b) = else_branch {
                    self.block(b);
                }
                self.goto(join);
                self.current = join;
            }
            ExprKind::Block(b) => self.block(b),
            ExprKind::Binary { left, right, .. } => {
                self.read(left);
                self.read(right);
            }
            ExprKind::Call {
                args, arg_modes, ..
            } => self.read_args(args, arg_modes),
            ExprKind::DynCall {
                receiver,
                args,
                arg_modes,
                ..
            } => {
                self.read(receiver);
                self.read_args(args, arg_modes);
            }
            ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
                for (_, v) in fields {
                    self.moved(v);
                }
            }
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                for e in elems {
                    self.moved(e);
                }
            }
            ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => self.read(base),
            ExprKind::Deref(e) | ExprKind::BitNot(e) | ExprKind::Neg(e) => self.read(e),
            ExprKind::Borrow { place, .. } => self.read(place),
            ExprKind::Coerce { value, .. } => self.read(value),
            // Typecheck rewrites every `MethodCall` to `Call`/`DynCall`/
            // `TraitBoundCall` before either consumer runs; handle the leftover
            // arms defensively as plain reads so the match stays exhaustive.
            ExprKind::MethodCall { receiver, args, .. }
            | ExprKind::TraitBoundCall { receiver, args, .. } => {
                self.read(receiver);
                for a in args {
                    self.read(a);
                }
            }
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::ConstParam(_)
            | ExprKind::Spawn { .. }
            | ExprKind::Error => {}
        }
    }

    /// Walk a call's arguments: a `take` of a non-`Copy` value moves; everything
    /// else (borrows, copies) is a read.
    fn read_args(&mut self, args: &[Expr], arg_modes: &[PassMode]) {
        for (i, a) in args.iter().enumerate() {
            let mode = arg_modes.get(i).copied().unwrap_or(PassMode::Read);
            if effect(mode, &a.ty) == Effect::Move {
                self.moved(a);
            } else {
                self.read(a);
            }
        }
    }
}

// === Shared helpers (the single definition reused by both passes) ===

/// Scalars and raw pointers are `Copy`; aggregates and (conservatively) type
/// parameters are not.
pub fn is_copy(ty: &Type) -> bool {
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
            // A shared `read` borrow copies freely (like `&T`); an exclusive
            // `mut` borrow is unique and does not.
            | Type::Ref { kind: RefKind::Read, .. }
    )
}

/// Whether a type is *view-kinded*: it transitively holds a borrow, so a value
/// of this type may not escape the frame it was created in (the four bans in
/// `ownership.rs`). A borrow (`Ref`) is view-kinded axiomatically; a struct or
/// enum is view-kinded when it is declared `view` or any of its generic
/// arguments is (per-instantiation, so `Wrapper[read T]` is view even when
/// `Wrapper` isn't); a composite is view-kinded when any component is.
///
/// `Param` is treated as data here: pre-monomorphization we don't know its
/// instantiation, and the only way a concrete borrow enters a `Param` slot is a
/// borrow expression whose own type is a `Ref` — caught at that site. The
/// remaining case (a generic that stores a borrow *through* a type parameter)
/// is deferred; no current program constructs it.
pub fn is_view(ty: &Type, structs: &[Struct], enums: &[Enum]) -> bool {
    match ty {
        Type::Ref { .. } => true,
        Type::Struct(id, args) => {
            structs[id.0 as usize].is_view || args.iter().any(|a| is_view(a, structs, enums))
        }
        Type::Enum(id, args) => {
            enums[id.0 as usize].is_view || args.iter().any(|a| is_view(a, structs, enums))
        }
        Type::Tuple(elems) => elems.iter().any(|e| is_view(e, structs, enums)),
        Type::Array(elem, _) => is_view(elem, structs, enums),
        _ => false,
    }
}

/// What a binding (parameter, `let`, or `match` arm) or call argument does to
/// its source, given its passing mode and type. The single definition of mode
/// meaning shared across the move analysis, the ownership checker, and drop
/// elaboration — so the three binding sites can't drift in how they read a mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Effect {
    /// A `Copy` value: duplicated, mode is a no-op, the source is unaffected.
    Copy,
    /// A `take` of a non-`Copy` value: ownership moves out, the source dies.
    Move,
    /// A `read`/`mut` of a non-`Copy` value: the source is borrowed and kept.
    Borrow,
}

/// Classify a moded binding/argument by its effect on the source. `Copy` types
/// ignore the mode; otherwise `take` moves and `read`/`mut` borrow.
pub fn effect(mode: PassMode, ty: &Type) -> Effect {
    if is_copy(ty) {
        Effect::Copy
    } else if matches!(mode, PassMode::Own) {
        Effect::Move
    } else {
        Effect::Borrow
    }
}

/// The local a place expression is rooted at (`x`, `x.f`, `x.0`, `*x`); `None`
/// for anything not rooted at a bare binding (a temporary, a call result).
pub fn root_symbol(expr: &Expr) -> Option<SymbolId> {
    match &expr.kind {
        ExprKind::Ident(sym) => Some(*sym),
        ExprKind::Field { base, .. }
        | ExprKind::TupleIndex { base, .. }
        | ExprKind::Deref(base) => root_symbol(base),
        _ => None,
    }
}

/// Whether a `match` over `arms` consumes (moves) its scrutinee. True when any
/// arm `take`s a non-`Copy` value out of the payload — that move ends the
/// scrutinee's ownership, so the match becomes responsible for freeing its box
/// and dropping the parts no arm took. The single predicate the move analysis,
/// drop elaboration, and codegen consult.
pub fn match_consumes(arms: &[MatchArm]) -> bool {
    arms.iter().any(|a| pattern_moves(&a.pattern))
}

/// Whether a pattern moves at least one value out of the matched value — i.e.
/// has a binding whose [`effect`] is [`Effect::Move`].
pub fn pattern_moves(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Binding { ty, mode, .. } => effect(*mode, ty) == Effect::Move,
        Pattern::Tuple { elems, .. } => elems.iter().any(pattern_moves),
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            fields.iter().any(|f| pattern_moves(&f.pattern))
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => false,
    }
}

/// Collect every `SymbolId` a pattern binds (recursively through tuples and
/// variant fields).
pub fn pattern_bindings(pattern: &Pattern, out: &mut Vec<SymbolId>) {
    match pattern {
        Pattern::Binding { symbol, .. } => out.push(*symbol),
        Pattern::Tuple { elems, .. } => {
            for e in elems {
                pattern_bindings(e, out);
            }
        }
        Pattern::Variant { fields, .. } | Pattern::Struct { fields, .. } => {
            for f in fields {
                pattern_bindings(&f.pattern, out);
            }
        }
        Pattern::Wildcard { .. } | Pattern::Int { .. } | Pattern::Bool { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(n: u32) -> SymbolId {
        SymbolId(n)
    }

    fn span() -> SpanId {
        SpanId(0)
    }

    fn mv(local: SymbolId) -> Action {
        Action::Move {
            local,
            span: span(),
            partial: false,
        }
    }

    /// `let a; drop a` → the value is still owned, so keep the drop.
    #[test]
    fn straight_line_keep() {
        let mut cfg = Cfg::new();
        let b = cfg.add_block();
        cfg.block(b).actions = vec![
            Action::Init(sym(1)),
            Action::Drop {
                id: 0,
                local: sym(1),
            },
        ];
        cfg.block(b).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Keep);
    }

    /// `let a; move a; drop a` → moved on the only path, so remove the drop.
    #[test]
    fn straight_line_moved_remove() {
        let mut cfg = Cfg::new();
        let b = cfg.add_block();
        cfg.block(b).actions = vec![
            Action::Init(sym(1)),
            mv(sym(1)),
            Action::Drop {
                id: 0,
                local: sym(1),
            },
        ];
        cfg.block(b).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Remove);
    }

    /// `let a; move a; a = ...; drop a` → reassignment revives it, so keep.
    #[test]
    fn moved_then_reassigned_keep() {
        let mut cfg = Cfg::new();
        let b = cfg.add_block();
        cfg.block(b).actions = vec![
            Action::Init(sym(1)),
            mv(sym(1)),
            Action::Init(sym(1)),
            Action::Drop {
                id: 0,
                local: sym(1),
            },
        ];
        cfg.block(b).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Keep);
    }

    /// Diamond, moved on one arm only → conditional (an error for the caller).
    #[test]
    fn conditional_move_is_conditional() {
        let mut cfg = Cfg::new();
        let entry = cfg.add_block();
        let then_b = cfg.add_block();
        let else_b = cfg.add_block();
        let join = cfg.add_block();
        cfg.block(entry).actions = vec![Action::Init(sym(1))];
        cfg.block(entry).term = Terminator::Switch(vec![then_b, else_b]);
        cfg.block(then_b).actions = vec![mv(sym(1))];
        cfg.block(then_b).term = Terminator::Goto(join);
        cfg.block(else_b).term = Terminator::Goto(join);
        cfg.block(join).actions = vec![Action::Drop {
            id: 0,
            local: sym(1),
        }];
        cfg.block(join).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Conditional);
    }

    /// Diamond, moved on both arms → must-moved, so remove the drop.
    #[test]
    fn moved_on_both_arms_remove() {
        let mut cfg = Cfg::new();
        let entry = cfg.add_block();
        let then_b = cfg.add_block();
        let else_b = cfg.add_block();
        let join = cfg.add_block();
        cfg.block(entry).actions = vec![Action::Init(sym(1))];
        cfg.block(entry).term = Terminator::Switch(vec![then_b, else_b]);
        cfg.block(then_b).actions = vec![mv(sym(1))];
        cfg.block(then_b).term = Terminator::Goto(join);
        cfg.block(else_b).actions = vec![mv(sym(1))];
        cfg.block(else_b).term = Terminator::Goto(join);
        cfg.block(join).actions = vec![Action::Drop {
            id: 0,
            local: sym(1),
        }];
        cfg.block(join).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Remove);
    }

    /// Several drops in one body are decided independently and keyed by id: a
    /// moved local removes its drop while a live local beside it keeps its own.
    #[test]
    fn multiple_drops_decided_independently() {
        let mut cfg = Cfg::new();
        let b = cfg.add_block();
        cfg.block(b).actions = vec![
            Action::Init(sym(1)),
            Action::Init(sym(2)),
            mv(sym(1)),
            Action::Drop {
                id: 0,
                local: sym(1),
            },
            Action::Drop {
                id: 1,
                local: sym(2),
            },
        ];
        cfg.block(b).term = Terminator::Return;
        let decisions = analyze(&cfg);
        assert_eq!(decisions[&0], DropDecision::Remove);
        assert_eq!(decisions[&1], DropDecision::Keep);
    }

    /// A loop that re-inits the local each iteration and drops it at the body
    /// end keeps the drop (it is owned every iteration); the back-edge converges.
    #[test]
    fn loop_reinit_each_iteration_keep() {
        let mut cfg = Cfg::new();
        let entry = cfg.add_block();
        let body = cfg.add_block();
        let exit = cfg.add_block();
        // entry -> body
        cfg.block(entry).term = Terminator::Goto(body);
        // body: let r; drop r; loop back or exit
        cfg.block(body).actions = vec![
            Action::Init(sym(1)),
            Action::Drop {
                id: 0,
                local: sym(1),
            },
        ];
        cfg.block(body).term = Terminator::Switch(vec![body, exit]);
        cfg.block(exit).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Keep);
    }

    /// A value moved in a loop body that every path to the exit passes through
    /// is must-moved at a drop after the loop → remove (the back-edge converges).
    #[test]
    fn loop_move_reaching_exit_remove() {
        let mut cfg = Cfg::new();
        let entry = cfg.add_block();
        let body = cfg.add_block();
        let exit = cfg.add_block();
        cfg.block(entry).actions = vec![Action::Init(sym(1))];
        cfg.block(entry).term = Terminator::Goto(body);
        // body may move `a` then loop back (so on the back-edge a is moved) or
        // exit; `a` is moved on some paths to the exit drop, not all.
        cfg.block(body).actions = vec![mv(sym(1))];
        cfg.block(body).term = Terminator::Switch(vec![body, exit]);
        cfg.block(exit).actions = vec![Action::Drop {
            id: 0,
            local: sym(1),
        }];
        cfg.block(exit).term = Terminator::Return;
        // a is moved on every path that reaches exit (it passes through body's
        // Move at least once), so it's must-moved → Remove.
        assert_eq!(analyze(&cfg)[&0], DropDecision::Remove);
    }

    /// `may_in_sets`: a straight-line move is visible to the following block on
    /// both the back-edge-inclusive and -exclusive runs.
    #[test]
    fn may_in_sees_straight_line_move() {
        let mut cfg = Cfg::new();
        let a = cfg.add_block();
        let b = cfg.add_block();
        cfg.block(a).actions = vec![mv(sym(1))];
        cfg.block(a).term = Terminator::Goto(b);
        cfg.block(b).term = Terminator::Return;
        assert!(may_in_sets(&cfg, true)[b].contains(&sym(1)));
        assert!(may_in_sets(&cfg, false)[b].contains(&sym(1)));
    }

    /// `may_in_sets`: a move carried only by a loop back-edge appears at the
    /// header with back-edges included, but not without — this is exactly how
    /// the ownership pass tells `MoveInLoop` from a straight-line use-after-move.
    #[test]
    fn may_in_isolates_loop_carried_move() {
        let mut cfg = Cfg::new();
        let entry = cfg.add_block();
        let header = cfg.add_block();
        let exit = cfg.add_block();
        cfg.block(entry).term = Terminator::Goto(header);
        // header moves `a` then loops back to itself or exits.
        cfg.block(header).actions = vec![mv(sym(1))];
        cfg.block(header).term = Terminator::Switch(vec![header, exit]);
        cfg.block(exit).term = Terminator::Return;
        cfg.back_edges.insert((header, header));
        // With back-edges, the self-loop carries the move into the header entry.
        assert!(may_in_sets(&cfg, true)[header].contains(&sym(1)));
        // Without back-edges, the header entry has nothing moved yet.
        assert!(!may_in_sets(&cfg, false)[header].contains(&sym(1)));
    }
}
