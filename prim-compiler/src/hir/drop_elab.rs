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
//! Candidate drops and CFG drop actions are produced from the *same* augmented
//! HIR walked in the same order, so the k-th `Stmt::Drop` in DFS order is the
//! k-th drop action — the `DropId` is just that position.

use super::cfg::{self, Action, BlockId, Cfg, DropDecision, DropId, Terminator};
use super::ownership::{MoveError, MoveErrorKind};
use super::{
    Block, DropInfo, Expr, ExprKind, Function, MatchArm, PassMode, Pattern, Program, SpanId, Stmt,
    SymbolId, Type,
};
use prim_tok::{FileId, Span};
use std::collections::HashMap;

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
    };
    inserter.block(&mut func.body, false, &params);

    // Phase 2: lower the augmented body to a CFG.
    let mut builder = CfgBuilder {
        cfg: Cfg::new(),
        current: 0,
        loop_exits: Vec::new(),
        droppable: &droppable,
        next_drop: 0,
    };
    builder.cfg.add_block(); // entry = block 0
    builder.block(&func.body);
    builder.cfg.block(builder.current).term = Terminator::Return;
    let cfg = builder.cfg;

    // Phase 3: decide each candidate.
    let decisions = cfg::analyze(&cfg);

    // Phase 4: apply — drop the removed candidates, error on conditional ones.
    let mut filter = Filter {
        decisions: &decisions,
        droppable: &droppable,
        next_drop: 0,
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
}

impl Insert<'_> {
    fn drop_stmt(sym: SymbolId, ty: Type, span: SpanId) -> Stmt {
        Stmt::Drop { sym, ty, span }
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
        for (sym, ty, span) in self.scopes.last().unwrap().locals.iter().rev() {
            out.push(Self::drop_stmt(*sym, ty.clone(), *span));
        }
        self.scopes.pop();
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

    fn emit_return_drops(&self, out: &mut Vec<Stmt>) {
        for frame in self.scopes.iter().rev() {
            for (sym, ty, span) in frame.locals.iter().rev() {
                out.push(Self::drop_stmt(*sym, ty.clone(), *span));
            }
        }
    }

    fn emit_break_drops(&self, out: &mut Vec<Stmt>) {
        for frame in self.scopes.iter().rev() {
            for (sym, ty, span) in frame.locals.iter().rev() {
                out.push(Self::drop_stmt(*sym, ty.clone(), *span));
            }
            if frame.is_loop {
                break;
            }
        }
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

// === Phase 2: lower the augmented HIR to a CFG ===

struct CfgBuilder<'a> {
    cfg: Cfg,
    current: BlockId,
    /// Exit block of each enclosing loop (innermost last) — a `break` target.
    loop_exits: Vec<BlockId>,
    droppable: &'a HashMap<SymbolId, SpanId>,
    next_drop: DropId,
}

impl CfgBuilder<'_> {
    fn act(&mut self, action: Action) {
        self.cfg.block(self.current).actions.push(action);
    }

    fn block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                self.read(value);
                let mut binds = Vec::new();
                pattern_bindings(pattern, &mut binds);
                for (sym, _) in binds {
                    if self.droppable.contains_key(&sym) {
                        self.act(Action::Init(sym));
                    }
                }
            }
            Stmt::Assign { target, value, .. } => {
                self.read(value);
                if self.droppable.contains_key(target) {
                    self.act(Action::Init(*target));
                }
            }
            Stmt::DerefAssign { ptr, value, .. } => {
                self.read(ptr);
                self.read(value);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.read(object);
                self.read(value);
            }
            Stmt::Expr(e) => self.read(e),
            Stmt::Loop { body, .. } => {
                let header = self.cfg.add_block();
                let exit = self.cfg.add_block();
                self.goto(header);
                self.loop_exits.push(exit);
                self.current = header;
                self.block(body);
                self.goto(header); // back-edge
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
                self.cfg.block(header).term = Terminator::Switch(vec![body_b, exit]);
                self.loop_exits.push(exit);
                self.current = body_b;
                self.block(body);
                self.goto(header); // back-edge
                self.loop_exits.pop();
                self.current = exit;
            }
            Stmt::Break { .. } => {
                let target = *self.loop_exits.last().expect("break outside loop");
                self.cfg.block(self.current).term = Terminator::Goto(target);
                self.current = self.cfg.add_block(); // dead code after break
            }
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.read(v);
                }
                self.cfg.block(self.current).term = Terminator::Return;
                self.current = self.cfg.add_block(); // dead code after return
            }
            Stmt::Drop { sym, .. } => {
                let id = self.next_drop;
                self.next_drop += 1;
                self.act(Action::Drop { id, local: *sym });
            }
        }
    }

    /// Set the current block's terminator to `Goto(target)` (unless it already
    /// diverged via an inner return/break).
    fn goto(&mut self, target: BlockId) {
        if matches!(self.cfg.block(self.current).term, Terminator::Unreachable) {
            self.cfg.block(self.current).term = Terminator::Goto(target);
        }
    }

    /// An expression in move position: a droppable place is moved out.
    fn moved(&mut self, expr: &Expr) {
        if !is_copy(&expr.ty) {
            if let Some(root) = root_symbol(expr) {
                if self.droppable.contains_key(&root) {
                    self.act(Action::Move(root));
                }
                return;
            }
        }
        self.read(expr);
    }

    /// An expression in read/borrow position: recurse, emitting moves from its
    /// move-position children and splitting the CFG at `match`/`if`.
    fn read(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Match { scrutinee, arms } => {
                // A matched non-`Copy` scrutinee is consumed.
                self.moved(scrutinee);
                let join = self.cfg.add_block();
                let mut arm_blocks = Vec::with_capacity(arms.len());
                for _ in arms {
                    arm_blocks.push(self.cfg.add_block());
                }
                self.cfg.block(self.current).term = Terminator::Switch(arm_blocks.clone());
                for (arm, &b) in arms.iter().zip(&arm_blocks) {
                    self.current = b;
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
                self.cfg.block(self.current).term = Terminator::Switch(vec![then_b, else_b]);
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
            } => {
                for (i, a) in args.iter().enumerate() {
                    if matches!(arg_modes.get(i), Some(PassMode::Take)) {
                        self.moved(a);
                    } else {
                        self.read(a);
                    }
                }
            }
            ExprKind::DynCall {
                receiver,
                args,
                arg_modes,
                ..
            } => {
                self.read(receiver);
                for (i, a) in args.iter().enumerate() {
                    if matches!(arg_modes.get(i), Some(PassMode::Take)) {
                        self.moved(a);
                    } else {
                        self.read(a);
                    }
                }
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
            ExprKind::Coerce { value, .. } => self.read(value),
            ExprKind::Dbg { inner, .. } => self.read(inner),
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
            | ExprKind::Ident(_)
            | ExprKind::Spawn { .. }
            | ExprKind::Error => {}
        }
    }
}

// === Phase 4: apply the decisions ===

struct Filter<'a> {
    decisions: &'a HashMap<DropId, DropDecision>,
    droppable: &'a HashMap<SymbolId, SpanId>,
    next_drop: DropId,
    error: Option<(SpanId, MoveErrorKind)>,
}

impl Filter<'_> {
    fn block(&mut self, block: &mut Block) {
        let mut out = Vec::with_capacity(block.stmts.len());
        for mut stmt in std::mem::take(&mut block.stmts) {
            if let Stmt::Drop { sym, span, .. } = &stmt {
                let id = self.next_drop;
                self.next_drop += 1;
                match self.decisions.get(&id).copied() {
                    Some(DropDecision::Remove) => continue, // delete the candidate
                    Some(DropDecision::Conditional) => {
                        if self.error.is_none() {
                            self.error = Some((*span, MoveErrorKind::ConditionalDrop));
                        }
                        // also keep it out of the program
                        let _ = self.droppable.get(sym);
                        continue;
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
        Pattern::Wildcard { .. }
        | Pattern::Int { .. }
        | Pattern::Bool { .. }
        | Pattern::Variant { .. } => {}
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

/// Scalars and raw pointers are `Copy`; aggregates are not.
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
