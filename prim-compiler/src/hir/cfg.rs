//! A small control-flow graph over a function body's ownership-relevant events,
//! plus the forward move dataflow that drives precise drop placement.
//!
//! The CFG is intentionally minimal: basic blocks hold a sequence of `Action`s
//! (a local becomes owned, a local is moved out, or a candidate drop), and each
//! block ends in a `Terminator` that names its successors. Control-flow
//! conditions are *not* modelled — only the edges — which is all the move
//! analysis needs.
//!
//! The analysis is a textbook forward dataflow over two lattices:
//!   - **may-moved**: a local moved on *at least one* path to a point (∪ at
//!     joins),
//!   - **must-moved**: a local moved on *every* path to a point (∩ at joins).
//!
//! At each candidate drop of a local `L`:
//!   - `L` must-moved  → the value is gone on all paths        → `Remove`,
//!   - `L` may- but not must-moved → moved on some paths only  → `Conditional`,
//!   - otherwise (moved on no path) → still owned              → `Keep`.
//!
//! `Conditional` is the case a single static drop can't express without a
//! runtime flag; the caller reports it as an error (a droppable value must be
//! moved on all paths or none).

use super::SymbolId;
use std::collections::{HashMap, HashSet};

pub type BlockId = usize;
pub type DropId = usize;

/// An ownership-relevant event within a basic block, in execution order.
#[derive(Clone, Copy, Debug)]
pub enum Action {
    /// `local` becomes owned: a `let` initializer or an assignment to it.
    Init(SymbolId),
    /// `local`'s value is moved out.
    Move(SymbolId),
    /// A candidate drop of `local` at a scope exit. `id` ties it to the HIR
    /// `Stmt::Drop` emitted in lockstep during lowering.
    Drop { id: DropId, local: SymbolId },
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
}

impl Cfg {
    pub fn new() -> Self {
        Cfg { blocks: Vec::new() }
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

/// Run the move dataflow and decide every candidate drop.
pub fn analyze(cfg: &Cfg) -> HashMap<DropId, DropDecision> {
    let n = cfg.blocks.len();
    let mut decisions = HashMap::new();
    if n == 0 {
        return decisions;
    }

    // Predecessors, inverted from each block's successors.
    let mut preds: Vec<Vec<BlockId>> = vec![Vec::new(); n];
    for (b, block) in cfg.blocks.iter().enumerate() {
        for &s in block.term.successors() {
            preds[s].push(b);
        }
    }

    // The universe of locals, for the must-moved (∩) lattice's top element.
    let universe: HashSet<SymbolId> = cfg
        .blocks
        .iter()
        .flat_map(|b| b.actions.iter())
        .map(|a| match a {
            Action::Init(l) | Action::Move(l) | Action::Drop { local: l, .. } => *l,
        })
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
            let (may_in, must_in) = entry_state(&preds[b], &exit_may, &exit_must);
            let (may_out, must_out) = transfer(&cfg.blocks[b], may_in, must_in, None);
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
        let (may_in, must_in) = entry_state(&preds[b], &exit_may, &exit_must);
        transfer(&cfg.blocks[b], may_in, must_in, Some(&mut decisions));
    }
    decisions
}

/// The dataflow state entering a block: ∪ of predecessors' may-moved, ∩ of
/// their must-moved. A block with no predecessors starts with nothing moved.
fn entry_state(
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
fn transfer(
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
            Action::Move(l) => {
                may.insert(l);
                must.insert(l);
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
        }
    }
    (may, must)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(n: u32) -> SymbolId {
        SymbolId(n)
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
            Action::Move(sym(1)),
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
            Action::Move(sym(1)),
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
        cfg.block(then_b).actions = vec![Action::Move(sym(1))];
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
        cfg.block(then_b).actions = vec![Action::Move(sym(1))];
        cfg.block(then_b).term = Terminator::Goto(join);
        cfg.block(else_b).actions = vec![Action::Move(sym(1))];
        cfg.block(else_b).term = Terminator::Goto(join);
        cfg.block(join).actions = vec![Action::Drop {
            id: 0,
            local: sym(1),
        }];
        cfg.block(join).term = Terminator::Return;
        assert_eq!(analyze(&cfg)[&0], DropDecision::Remove);
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

    /// Several drops in one body are decided independently and keyed by id: a
    /// moved local removes its drop while a live local beside it keeps its own.
    #[test]
    fn multiple_drops_decided_independently() {
        let mut cfg = Cfg::new();
        let b = cfg.add_block();
        cfg.block(b).actions = vec![
            Action::Init(sym(1)),
            Action::Init(sym(2)),
            Action::Move(sym(1)),
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
        cfg.block(body).actions = vec![Action::Move(sym(1))];
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
}
