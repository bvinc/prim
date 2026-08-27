//! Emission of user code: per-function context, expression/statement
//! lowering to wasm instructions, and the dispatch for runtime-bound calls.

use crate::WasmError;
use crate::builtins::Builtins;
use crate::layout::{
    CLOCK_SCRATCH, EnumLayout, POLL_NEVENTS, StructLayout, compute_enum_layout,
    compute_struct_layout, compute_tuple_layout, emit_field_load, emit_field_store,
};
use crate::types::{hir_type_to_valtype, is_signed_int, produces_value};
use crate::walks::{collect_locals, collect_scratch_types_body};
use prim_compiler::hir;
use prim_compiler::hir::inline::InlinePolicy;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

/// Static-memory location of a string literal's bytes.
#[derive(Clone, Copy)]
pub(crate) struct StrSite {
    pub ptr: u32,
    pub len: u32,
}

#[derive(Clone, Copy)]
pub(crate) struct StringLayout {
    pub struct_id: hir::StructId,
    pub size: u32,
    pub data_offset: u32,
    pub len_offset: u32,
    pub cap_offset: u32,
}

/// Per-function emission state. Holds references to immutable program-wide
/// inputs (`program`, `funcs`, `runtime`, `builtins`, `struct_layouts`) plus
/// this function's own local scope and pre-allocated scratch slots.
pub(crate) struct EmitCtx<'a> {
    pub program: &'a hir::Program,
    /// Which aggregate types are stored inline vs. boxed. Shared with layout.
    pub policy: &'a InlinePolicy<'a>,
    /// Drop impls, for the scalar-ABI flatness check (a needs-drop type must
    /// stay a box so its destructor runs).
    pub info: &'a hir::DropInfo<'a>,
    /// Per-function: which parameters use the by-value scalar ABI (phase 3).
    /// Consulted when emitting call arguments.
    scalar_abi: &'a HashMap<hir::FuncId, Vec<bool>>,
    /// Per-function: the leaf fields of a by-value scalar-ABI return. Consulted
    /// at call sites to materialize a multi-value result.
    scalar_ret: &'a HashMap<hir::FuncId, Vec<ScalarField>>,
    /// This function's own scalar-ABI return fields, if it returns by value.
    ret_fields: Option<Vec<ScalarField>>,
    pub locals: HashMap<hir::SymbolId, u32>,
    /// Locals (phase 2a) held in wasm locals instead of a heap box: their leaf
    /// fields occupy consecutive local slots. The whole value is never
    /// materialized, so these symbols are absent from `locals`.
    scalarized: HashMap<hir::SymbolId, ScalarLocals>,
    /// Wasm valtypes for this function's body locals, in index order, with each
    /// scalarized aggregate expanded to one slot per leaf field. Used to declare
    /// locals; scratch slots follow.
    body_local_valtypes: Vec<ValType>,
    pub funcs: &'a HashMap<hir::FuncId, u32>,
    /// Concrete needs-drop type → wasm index of its synthesized `drop_T`
    /// function. A `Stmt::Drop` of type `T` lowers to a call of `drop_fns[T]`.
    pub drop_fns: &'a HashMap<hir::Type, u32>,
    /// Trait → wasm index of its synthesized `drop_trait_Trait(fat_ptr)`, which
    /// dispatches through the vtable's drop slot to free the boxed value and
    /// the fat pointer. A `Stmt::Drop` of `Type::Trait(tid)` lowers to this
    /// (trait objects are never in `drop_fns`).
    pub drop_trait_fns: &'a HashMap<hir::TraitId, u32>,
    pub runtime: &'a HashMap<hir::FuncId, hir::RuntimeAbi>,
    pub builtins: &'a Builtins,
    pub struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    pub enum_layouts: &'a HashMap<hir::EnumId, EnumLayout>,
    pub string_layout: Option<StringLayout>,
    /// HIR GlobalId → wasm global index. User globals come after the heap
    /// pointer (wasm global 0).
    pub global_wasm_idx: &'a HashMap<hir::GlobalId, u32>,
    /// Per-trait wasm type index for `call_indirect` when dispatching that
    /// trait's methods. Method signature is uniform across the trait: an
    /// `i32` receiver (the boxed struct's data pointer) followed by the
    /// declared param types (excluding the receiver position).
    pub dyn_call_types: &'a HashMap<(hir::TraitId, u32), u32>,
    /// Closure function → its function-table slot. A `ClosureRef { func }`
    /// emits this slot as an `i32`.
    pub closure_table_slots: &'a HashMap<hir::FuncId, u32>,
    /// `(param valtypes, result valtypes)` → wasm type index for a block
    /// signature, used by `call_indirect` at `IndirectCall` sites.
    pub indirect_call_types: &'a HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    /// `(TraitId, StructId)` → static-memory address of the vtable (4
    /// bytes per slot, indexed by trait method position).
    pub vtable_addr: &'a HashMap<(hir::TraitId, hir::StructId), u32>,
    pub scratch_base: u32,
    pub scratch_counter: Cell<u32>,
    /// Number of wasm structured blocks (`block`/`loop`/`if`) currently open
    /// at the emission cursor. Used to compute correct relative branch
    /// targets for `break`.
    pub ctrl_depth: Cell<u32>,
    /// For each enclosing loop (innermost last), the `ctrl_depth` just before
    /// its exit `block` was opened — i.e. that block's branch level. `break`
    /// targets the innermost.
    pub loop_exits: RefCell<Vec<u32>>,
    pub str_sites: &'a [StrSite],
    pub str_counter: Cell<u32>,
    /// Static bytes of the allocator's out-of-memory message, laid out by
    /// `lib.rs` next to the string literals. The `Oom` runtime ABI writes
    /// them straight to stdout/stderr — no String box — so the abort path
    /// never allocates (the heap may be the thing that failed).
    pub oom_msg: StrSite,
    pub oom_sentinel: StrSite,
    /// First codegen invariant violation hit while emitting this function. The
    /// `()`-returning emit helpers record it here (and still emit a placeholder
    /// `unreachable` to keep the partial function well-formed); `emit_user_function`
    /// turns it into a hard build failure instead of shipping a trap.
    pub codegen_error: RefCell<Option<crate::WasmError>>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_emit_ctx<'a>(
    program: &'a hir::Program,
    policy: &'a InlinePolicy<'a>,
    info: &'a hir::DropInfo<'a>,
    scalar_abi: &'a HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &'a HashMap<hir::FuncId, Vec<ScalarField>>,
    func: &hir::Function,
    func_map: &'a HashMap<hir::FuncId, u32>,
    drop_fns: &'a HashMap<hir::Type, u32>,
    drop_trait_fns: &'a HashMap<hir::TraitId, u32>,
    runtime_map: &'a HashMap<hir::FuncId, hir::RuntimeAbi>,
    builtins: &'a Builtins,
    struct_layouts: &'a HashMap<hir::StructId, StructLayout>,
    enum_layouts: &'a HashMap<hir::EnumId, EnumLayout>,
    string_layout: Option<StringLayout>,
    global_wasm_idx: &'a HashMap<hir::GlobalId, u32>,
    dyn_call_types: &'a HashMap<(hir::TraitId, u32), u32>,
    closure_table_slots: &'a HashMap<hir::FuncId, u32>,
    indirect_call_types: &'a HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    vtable_addr: &'a HashMap<(hir::TraitId, hir::StructId), u32>,
    str_sites: &'a [StrSite],
    oom_msg: StrSite,
    oom_sentinel: StrSite,
) -> EmitCtx<'a> {
    let mut locals = HashMap::new();
    let mut scalarized: HashMap<hir::SymbolId, ScalarLocals> = HashMap::new();

    // Parameters first. A by-value scalar-ABI parameter (phase 3) occupies one
    // wasm-param slot per leaf field and is recorded as scalarized; every other
    // parameter takes a single slot.
    let abi = scalar_abi.get(&func.id);
    let mut next = 0u32;
    for (i, param) in func.params.iter().enumerate() {
        if abi.is_some_and(|v| v[i]) {
            let fields = flat_scalar_fields(&param.ty, program, policy, info)
                .expect("scalar-ABI param must be a flat scalar aggregate");
            let base = next;
            next += fields.len() as u32;
            scalarized.insert(param.name, ScalarLocals { base, fields });
        } else {
            locals.insert(param.name, next);
            next += 1;
        }
    }

    // Assign body-local slots after the params. A scalarized aggregate expands to
    // one slot per leaf field (recorded in `scalarized`, absent from `locals`);
    // every other local takes a single slot.
    let scalar_fields = scalarizable_locals(func, program, policy, info, scalar_abi, scalar_ret);
    let mut body_local_valtypes: Vec<ValType> = Vec::new();
    for (sym, vt) in collect_locals(&func.body) {
        if let Some(fields) = scalar_fields.get(&sym) {
            let base = next;
            for sf in fields {
                body_local_valtypes.push(sf.valtype);
                next += 1;
            }
            scalarized.insert(
                sym,
                ScalarLocals {
                    base,
                    fields: fields.clone(),
                },
            );
        } else {
            locals.insert(sym, next);
            body_local_valtypes.push(vt);
            next += 1;
        }
    }
    let scratch_base = next;
    EmitCtx {
        program,
        policy,
        scalar_abi,
        scalar_ret,
        ret_fields: scalar_ret.get(&func.id).cloned(),
        locals,
        scalarized,
        body_local_valtypes,
        funcs: func_map,
        drop_fns,
        drop_trait_fns,
        runtime: runtime_map,
        builtins,
        struct_layouts,
        enum_layouts,
        string_layout,
        global_wasm_idx,
        dyn_call_types,
        closure_table_slots,
        indirect_call_types,
        vtable_addr,
        info,
        scratch_base,
        scratch_counter: Cell::new(0),
        ctrl_depth: Cell::new(0),
        loop_exits: RefCell::new(Vec::new()),
        str_sites,
        str_counter: Cell::new(0),
        oom_msg,
        oom_sentinel,
        codegen_error: RefCell::new(None),
    }
}

// === Scalarization (phase 2a): hold a local aggregate in wasm locals ===

/// One leaf field of a scalarized aggregate local: which field, and its scalar
/// wasm type. Structs key by field symbol, tuples by position.
#[derive(Clone)]
pub(crate) struct ScalarField {
    key: ScalarKey,
    pub(crate) valtype: ValType,
}

#[derive(Clone, PartialEq)]
enum ScalarKey {
    Field(hir::InternSymbol),
    Index(usize),
}

/// A local aggregate held directly in wasm locals (one per leaf field) instead
/// of a heap box. Field `i` is wasm local `base + i`.
#[derive(Clone)]
struct ScalarLocals {
    base: u32,
    fields: Vec<ScalarField>,
}

impl ScalarLocals {
    fn local_of(&self, key: &ScalarKey) -> Option<u32> {
        self.fields
            .iter()
            .position(|sf| &sf.key == key)
            .map(|i| self.base + i as u32)
    }
}

/// If `ty` is a small, non-`Drop` struct/tuple whose every field is a scalar, it
/// can live in wasm locals (one per field): return those leaf fields. A field
/// that is itself an aggregate disqualifies it (a later phase flattens those).
pub(crate) fn flat_scalar_fields(
    ty: &hir::Type,
    program: &hir::Program,
    policy: &InlinePolicy,
    info: &hir::DropInfo,
) -> Option<Vec<ScalarField>> {
    // A needs-drop value must stay a single owned box so its destructor runs
    // (and is passed as a pointer); only a non-Drop flat aggregate may be
    // scalarized into independent wasm values.
    if !policy.is_inline(ty) || info.needs_drop(ty) {
        return None;
    }
    // "Flat" means every leaf is a scalar or raw pointer — never a nested
    // aggregate (even a `Copy` one). A nested aggregate is stored inline as
    // *bytes*, so it cannot be flattened into a single wasm value here; those
    // types fall back to the box/place path (Phase 2) instead of scalarization.
    let scalar = |t: &hir::Type| -> Option<ValType> {
        if matches!(t, hir::Type::Pointer { .. }) || is_scalar_ty(t) {
            Some(hir_type_to_valtype(t))
        } else {
            None
        }
    };
    match ty {
        hir::Type::Struct(sid, _) => {
            let s = program.structs.get(sid.0 as usize)?;
            s.fields
                .iter()
                .map(|fld| {
                    Some(ScalarField {
                        key: ScalarKey::Field(fld.name),
                        valtype: scalar(&fld.ty)?,
                    })
                })
                .collect()
        }
        hir::Type::Tuple(elems) => elems
            .iter()
            .enumerate()
            .map(|(i, ety)| {
                Some(ScalarField {
                    key: ScalarKey::Index(i),
                    valtype: scalar(ety)?,
                })
            })
            .collect(),
        _ => None,
    }
}

/// The local an access chain is rooted at — `L`, `L.f`, `L.0.x` — but `None`
/// through a deref, call, or anything that isn't a pure field path off a name.
fn ident_root(e: &hir::Expr) -> Option<hir::SymbolId> {
    match &e.kind {
        hir::ExprKind::Ident(s) => Some(*s),
        hir::ExprKind::Field { base, .. } | hir::ExprKind::TupleIndex { base, .. } => {
            ident_root(base)
        }
        _ => None,
    }
}

/// Locals safe to keep in wasm locals instead of a box: a `let`-bound flat
/// scalar aggregate, literal-initialized, whose *every* use is a field read.
/// Any whole-value use (passed, returned, stored, matched, written, reassigned)
/// disqualifies it, so the whole value never needs materializing. Conservative
/// by construction: a use it doesn't recognize as a field read disqualifies.
fn scalarizable_locals(
    func: &hir::Function,
    program: &hir::Program,
    policy: &InlinePolicy,
    info: &hir::DropInfo,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
) -> HashMap<hir::SymbolId, Vec<ScalarField>> {
    let mut candidates: HashMap<hir::SymbolId, Vec<ScalarField>> = HashMap::new();
    scalar_candidates_block(
        &func.body,
        program,
        policy,
        info,
        scalar_ret,
        &mut candidates,
    );
    if candidates.is_empty() {
        return candidates;
    }
    let mut disq: HashSet<hir::SymbolId> = HashSet::new();
    // A function returning by value may return a local by name without forcing a
    // box, so relax the walk at return positions for scalar-ABI returns.
    let ret_scalar = scalar_ret.contains_key(&func.id);
    scalar_disqualify_body(&func.body, scalar_abi, ret_scalar, &mut disq);
    candidates.retain(|sym, _| !disq.contains(sym));
    candidates
}

/// Which of a function's parameters use the by-value scalar ABI (phase 3): each
/// is passed as one wasm value per leaf field rather than a heap pointer. A
/// parameter qualifies when it is a flat scalar aggregate, *not* `mut` (an
/// mut borrow needs a pointer to write through), and used only by field reads
/// in the body (so it can stay scalarized — never materialized). This is the
/// single source of truth shared by signature registration and call sites.
pub(crate) fn scalar_abi_params(
    func: &hir::Function,
    program: &hir::Program,
    policy: &InlinePolicy,
    info: &hir::DropInfo,
) -> Vec<bool> {
    // No scalar-ABI map yet (this computes it); use the unrelaxed walk, where
    // any whole-value argument disqualifies. A parameter therefore qualifies
    // only if used purely by field reads.
    let empty: HashMap<hir::FuncId, Vec<bool>> = HashMap::new();
    let mut disq: HashSet<hir::SymbolId> = HashSet::new();
    scalar_disqualify_block(&func.body, &empty, false, &mut disq);
    func.params
        .iter()
        .map(|p| {
            p.mode != hir::PassMode::Mut
                && !disq.contains(&p.name)
                && flat_scalar_fields(&p.ty, program, policy, info).is_some()
        })
        .collect()
}

/// Collect `let L = <literal>` bindings of flat-scalar aggregate type, at every
/// nesting level.
fn scalar_candidates_block(
    block: &hir::Block,
    program: &hir::Program,
    policy: &InlinePolicy,
    info: &hir::DropInfo,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    out: &mut HashMap<hir::SymbolId, Vec<ScalarField>>,
) {
    for stmt in &block.stmts {
        if let hir::Stmt::Let {
            pattern: hir::Pattern::Binding { symbol, ty, .. },
            value,
            ..
        } = stmt
        {
            // Scalarizable initializers: a struct/tuple literal, or a call whose
            // scalar-ABI return arrives as field values (consumed straight into
            // the locals, no box).
            let init_ok = match &value.kind {
                hir::ExprKind::StructLit { .. } | hir::ExprKind::TupleLit(_) => true,
                hir::ExprKind::Call { func, .. } => scalar_ret.contains_key(func),
                _ => false,
            };
            if init_ok && let Some(fields) = flat_scalar_fields(ty, program, policy, info) {
                out.insert(*symbol, fields);
            }
        }
        scalar_candidates_stmt(stmt, program, policy, info, scalar_ret, out);
    }
    if let Some(e) = &block.expr {
        scalar_candidates_expr(e, program, policy, info, scalar_ret, out);
    }
}

fn scalar_candidates_stmt(
    stmt: &hir::Stmt,
    program: &hir::Program,
    policy: &InlinePolicy,
    info: &hir::DropInfo,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    out: &mut HashMap<hir::SymbolId, Vec<ScalarField>>,
) {
    match stmt {
        hir::Stmt::Let { value, .. }
        | hir::Stmt::Assign { value, .. }
        | hir::Stmt::Expr(value)
        | hir::Stmt::Return {
            value: Some(value), ..
        } => scalar_candidates_expr(value, program, policy, info, scalar_ret, out),
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            scalar_candidates_expr(ptr, program, policy, info, scalar_ret, out);
            scalar_candidates_expr(value, program, policy, info, scalar_ret, out);
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            scalar_candidates_expr(object, program, policy, info, scalar_ret, out);
            scalar_candidates_expr(value, program, policy, info, scalar_ret, out);
        }
        hir::Stmt::Loop { body, .. } => {
            scalar_candidates_block(body, program, policy, info, scalar_ret, out)
        }
        hir::Stmt::While {
            condition, body, ..
        } => {
            scalar_candidates_expr(condition, program, policy, info, scalar_ret, out);
            scalar_candidates_block(body, program, policy, info, scalar_ret, out);
        }
        hir::Stmt::Return { value: None, .. }
        | hir::Stmt::Break { .. }
        | hir::Stmt::Drop { .. } => {}
    }
}

fn scalar_candidates_expr(
    expr: &hir::Expr,
    program: &hir::Program,
    policy: &InlinePolicy,
    info: &hir::DropInfo,
    scalar_ret: &HashMap<hir::FuncId, Vec<ScalarField>>,
    out: &mut HashMap<hir::SymbolId, Vec<ScalarField>>,
) {
    match &expr.kind {
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            scalar_candidates_expr(condition, program, policy, info, scalar_ret, out);
            scalar_candidates_block(then_branch, program, policy, info, scalar_ret, out);
            if let Some(b) = else_branch {
                scalar_candidates_block(b, program, policy, info, scalar_ret, out);
            }
        }
        hir::ExprKind::Block(b) | hir::ExprKind::UnsafeBlock(b) => {
            scalar_candidates_block(b, program, policy, info, scalar_ret, out)
        }
        hir::ExprKind::Match {
            mode: _,
            scrutinee,
            arms,
        } => {
            scalar_candidates_expr(scrutinee, program, policy, info, scalar_ret, out);
            for arm in arms {
                scalar_candidates_expr(&arm.body, program, policy, info, scalar_ret, out);
            }
        }
        _ => {}
    }
}

/// Disqualify a local on any use that isn't a pure field read. A `Field`/
/// `TupleIndex` rooted at a name is a read of that name (recorded by *not*
/// recursing into the chain); a bare `Ident` reached anywhere else is a
/// whole-value use.
/// A returned value at a by-value scalar-ABI return position: a whole local
/// returned by name does not force a box (`emit_scalar_value` pushes its field
/// locals), so it is not disqualified. Any other returned value is a normal use.
fn disqualify_return_value(
    value: &hir::Expr,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    ret_scalar: bool,
    disq: &mut HashSet<hir::SymbolId>,
) {
    if ret_scalar && matches!(&value.kind, hir::ExprKind::Ident(_)) {
        return;
    }
    scalar_disqualify_expr(value, scalar_abi, ret_scalar, disq);
}

/// Disqualify locals over a whole function body. The trailing expression is the
/// implicit return value, so it is treated like an explicit `return` (relaxed at
/// a scalar-ABI return). Inner-block trailing expressions are ordinary block
/// values and stay in `scalar_disqualify_block`.
fn scalar_disqualify_body(
    body: &hir::Block,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    ret_scalar: bool,
    disq: &mut HashSet<hir::SymbolId>,
) {
    for stmt in &body.stmts {
        scalar_disqualify_stmt(stmt, scalar_abi, ret_scalar, disq);
    }
    if let Some(e) = &body.expr {
        disqualify_return_value(e, scalar_abi, ret_scalar, disq);
    }
}

fn scalar_disqualify_block(
    block: &hir::Block,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    ret_scalar: bool,
    disq: &mut HashSet<hir::SymbolId>,
) {
    for stmt in &block.stmts {
        scalar_disqualify_stmt(stmt, scalar_abi, ret_scalar, disq);
    }
    if let Some(e) = &block.expr {
        scalar_disqualify_expr(e, scalar_abi, ret_scalar, disq);
    }
}

fn scalar_disqualify_stmt(
    stmt: &hir::Stmt,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    ret_scalar: bool,
    disq: &mut HashSet<hir::SymbolId>,
) {
    match stmt {
        hir::Stmt::Let { value, .. } | hir::Stmt::Expr(value) => {
            scalar_disqualify_expr(value, scalar_abi, ret_scalar, disq)
        }
        hir::Stmt::Assign { target, value, .. } => {
            disq.insert(*target);
            scalar_disqualify_expr(value, scalar_abi, ret_scalar, disq);
        }
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            scalar_disqualify_expr(ptr, scalar_abi, ret_scalar, disq);
            scalar_disqualify_expr(value, scalar_abi, ret_scalar, disq);
        }
        hir::Stmt::FieldAssign { object, value, .. } => {
            // A write through `L.f` is not supported by 2a; if `L` is the
            // root, disqualify it. Otherwise the object is a normal expr.
            if let Some(root) = ident_root(object) {
                disq.insert(root);
            } else {
                scalar_disqualify_expr(object, scalar_abi, ret_scalar, disq);
            }
            scalar_disqualify_expr(value, scalar_abi, ret_scalar, disq);
        }
        hir::Stmt::Return {
            value: Some(value), ..
        } => disqualify_return_value(value, scalar_abi, ret_scalar, disq),
        hir::Stmt::Loop { body, .. } => scalar_disqualify_block(body, scalar_abi, ret_scalar, disq),
        hir::Stmt::While {
            condition, body, ..
        } => {
            scalar_disqualify_expr(condition, scalar_abi, ret_scalar, disq);
            scalar_disqualify_block(body, scalar_abi, ret_scalar, disq);
        }
        hir::Stmt::Return { value: None, .. }
        | hir::Stmt::Break { .. }
        | hir::Stmt::Drop { .. } => {}
    }
}

fn scalar_disqualify_expr(
    expr: &hir::Expr,
    scalar_abi: &HashMap<hir::FuncId, Vec<bool>>,
    ret_scalar: bool,
    disq: &mut HashSet<hir::SymbolId>,
) {
    match &expr.kind {
        // A field-access chain rooted at a name is a read of that name; do not
        // recurse into the chain (its only content is the field path).
        hir::ExprKind::Field { base, .. } | hir::ExprKind::TupleIndex { base, .. } => {
            if ident_root(base).is_none() {
                scalar_disqualify_expr(base, scalar_abi, ret_scalar, disq);
            }
        }
        // A bare name reached here (not shortcut by a parent field access) is a
        // whole-value use.
        hir::ExprKind::Ident(s) => {
            disq.insert(*s);
        }
        hir::ExprKind::Binary { left, right, .. } => {
            scalar_disqualify_expr(left, scalar_abi, ret_scalar, disq);
            scalar_disqualify_expr(right, scalar_abi, ret_scalar, disq);
        }
        hir::ExprKind::Call { func, args, .. } => {
            // A whole local passed by value at a by-value scalar-ABI position
            // does not force a box: `emit_scalar_arg` pushes its field locals.
            // Any other argument is a normal (boxing) use.
            let abi = scalar_abi.get(func);
            for (i, a) in args.iter().enumerate() {
                let by_value = abi.is_some_and(|v| v.get(i).copied().unwrap_or(false))
                    && matches!(&a.kind, hir::ExprKind::Ident(_));
                if !by_value {
                    scalar_disqualify_expr(a, scalar_abi, ret_scalar, disq);
                }
            }
        }
        hir::ExprKind::DynCall { receiver, args, .. }
        | hir::ExprKind::MethodCall { receiver, args, .. }
        | hir::ExprKind::TraitBoundCall { receiver, args, .. } => {
            scalar_disqualify_expr(receiver, scalar_abi, ret_scalar, disq);
            for a in args {
                scalar_disqualify_expr(a, scalar_abi, ret_scalar, disq);
            }
        }
        hir::ExprKind::StructLit { fields, .. } | hir::ExprKind::VariantLit { fields, .. } => {
            for (_, v) in fields {
                scalar_disqualify_expr(v, scalar_abi, ret_scalar, disq);
            }
        }
        hir::ExprKind::TupleLit(elems) | hir::ExprKind::ArrayLit(elems) => {
            for e in elems {
                scalar_disqualify_expr(e, scalar_abi, ret_scalar, disq);
            }
        }
        hir::ExprKind::Deref(e) | hir::ExprKind::BitNot(e) | hir::ExprKind::Neg(e) => {
            scalar_disqualify_expr(e, scalar_abi, ret_scalar, disq)
        }
        hir::ExprKind::Coerce { value, .. } => {
            scalar_disqualify_expr(value, scalar_abi, ret_scalar, disq)
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            scalar_disqualify_expr(condition, scalar_abi, ret_scalar, disq);
            scalar_disqualify_block(then_branch, scalar_abi, ret_scalar, disq);
            if let Some(b) = else_branch {
                scalar_disqualify_block(b, scalar_abi, ret_scalar, disq);
            }
        }
        hir::ExprKind::Block(b) | hir::ExprKind::UnsafeBlock(b) => {
            scalar_disqualify_block(b, scalar_abi, ret_scalar, disq)
        }
        hir::ExprKind::IndirectCall { callee, args } => {
            scalar_disqualify_expr(callee, scalar_abi, ret_scalar, disq);
            for a in args {
                scalar_disqualify_expr(a, scalar_abi, ret_scalar, disq);
            }
        }
        hir::ExprKind::Closure { .. } | hir::ExprKind::ClosureRef { .. } => {}
        hir::ExprKind::Match {
            mode: _,
            scrutinee,
            arms,
        } => {
            scalar_disqualify_expr(scrutinee, scalar_abi, ret_scalar, disq);
            for arm in arms {
                scalar_disqualify_expr(&arm.body, scalar_abi, ret_scalar, disq);
            }
        }
        hir::ExprKind::Int(_)
        | hir::ExprKind::Float(_)
        | hir::ExprKind::Bool(_)
        | hir::ExprKind::Str(_)
        | hir::ExprKind::ConstParam(_)
        | hir::ExprKind::Spawn { .. }
        | hir::ExprKind::Error => {}
    }
}

impl EmitCtx<'_> {
    /// Record a codegen invariant violation (keeping the first one). Used by the
    /// `()`-returning emit helpers, which can't return a `Result`; the function
    /// emitter checks `take_error` afterwards and fails the build.
    fn fail(&self, msg: impl Into<String>) {
        let mut slot = self.codegen_error.borrow_mut();
        if slot.is_none() {
            *slot = Some(crate::WasmError::Internal(msg.into()));
        }
    }

    /// Take the recorded codegen error, if any.
    fn take_error(&self) -> Option<crate::WasmError> {
        self.codegen_error.borrow_mut().take()
    }

    /// Record that a wasm structured block has just been opened.
    fn enter_ctrl(&self) {
        self.ctrl_depth.set(self.ctrl_depth.get() + 1);
    }

    /// Record that a wasm structured block has just been closed (`end`).
    fn exit_ctrl(&self) {
        self.ctrl_depth.set(self.ctrl_depth.get() - 1);
    }

    /// Enter a loop whose exit `block` was opened at the current depth.
    fn enter_loop(&self) {
        self.loop_exits.borrow_mut().push(self.ctrl_depth.get());
    }

    fn exit_loop(&self) {
        self.loop_exits.borrow_mut().pop();
    }

    /// Relative branch target for `break`: the distance from the current
    /// depth out to the innermost enclosing loop's exit block. Accounts for
    /// any `if`/`match` blocks between the `break` and the loop.
    fn break_target(&self) -> u32 {
        let exit_level = *self
            .loop_exits
            .borrow()
            .last()
            .expect("break outside of a loop");
        self.ctrl_depth.get() - 1 - exit_level
    }
}

/// If `sym` resolves to a module-level global, return its wasm global
/// index. Returns `None` if `sym` is anything else (local, function, etc.).
fn global_wasm_index(ctx: &EmitCtx, sym: hir::SymbolId) -> Option<u32> {
    let info = ctx.program.symbols.get(sym.0 as usize)?;
    let gid = match info.kind {
        hir::SymbolKind::Global(gid) => gid,
        _ => return None,
    };
    ctx.global_wasm_idx.get(&gid).copied()
}

// === Function body emission ===

pub(crate) fn emit_user_function(
    func: &hir::Function,
    ctx: &EmitCtx,
) -> Result<Function, WasmError> {
    let mut scratch_types: Vec<ValType> = Vec::new();
    collect_scratch_types_body(
        &func.body,
        ctx.runtime,
        ctx.scalar_abi,
        ctx.scalar_ret,
        ctx.ret_fields.is_some(),
        ctx.policy,
        &mut scratch_types,
    );
    // Body locals (scalarized aggregates already expanded to one slot per leaf
    // field by `build_emit_ctx`), then scratch slots.
    let mut wasm_locals: Vec<(u32, ValType)> =
        ctx.body_local_valtypes.iter().map(|vt| (1, *vt)).collect();
    for vt in &scratch_types {
        wasm_locals.push((1, *vt));
    }
    let mut f = Function::new(wasm_locals);
    // The body's trailing expression is the implicit return value; for a
    // scalar-ABI return it must be emitted as N field values, not a pointer.
    for stmt in &func.body.stmts {
        emit_stmt(&mut f, stmt, ctx)?;
    }
    if let Some(expr) = &func.body.expr {
        if ctx.ret_fields.is_some() {
            emit_scalar_value(&mut f, expr, ctx)?;
        } else {
            emit_expr(&mut f, expr, ctx)?;
        }
    }
    if let Some(ret_ty) = &func.ret {
        let needs_default = match &func.body.expr {
            Some(expr) => !produces_value(&expr.ty),
            None => true,
        };
        if needs_default {
            if let Some(fields) = &ctx.ret_fields {
                // Unreachable fall-through (all paths return); balance the stack
                // with one default per result field.
                for sf in fields {
                    emit_default_for(&mut f, sf.valtype);
                }
            } else {
                emit_default_value(&mut f, ret_ty);
            }
        }
    }
    f.instruction(&Instruction::End);
    // A `()`-returning helper may have recorded an unlowerable shape; surface it
    // as a build failure rather than shipping the placeholder trap.
    if let Some(err) = ctx.take_error() {
        return Err(err);
    }
    Ok(f)
}

/// Record a codegen invariant violation and emit a placeholder `unreachable` so
/// the partial function stays well-formed. The recorded error is turned into a
/// build failure by `emit_user_function`. For use in `()`-returning emit helpers
/// that cannot return a `Result`.
fn bail(f: &mut Function, ctx: &EmitCtx, msg: impl Into<String>) {
    ctx.fail(msg);
    f.instruction(&Instruction::Unreachable);
}

fn emit_default_value(f: &mut Function, ty: &hir::Type) {
    emit_default_for(f, hir_type_to_valtype(ty));
}

/// Push a zero value of the given wasm type.
fn emit_default_for(f: &mut Function, vt: ValType) {
    match vt {
        ValType::I32 => f.instruction(&Instruction::I32Const(0)),
        ValType::I64 => f.instruction(&Instruction::I64Const(0)),
        ValType::F32 => f.instruction(&Instruction::F32Const(0.0_f32.into())),
        ValType::F64 => f.instruction(&Instruction::F64Const(0.0_f64.into())),
        _ => f.instruction(&Instruction::I32Const(0)),
    };
}

fn emit_block(f: &mut Function, block: &hir::Block, ctx: &EmitCtx) -> Result<(), WasmError> {
    for stmt in &block.stmts {
        emit_stmt(f, stmt, ctx)?;
    }
    if let Some(expr) = &block.expr {
        emit_expr(f, expr, ctx)?;
    }
    Ok(())
}

fn emit_stmt(f: &mut Function, stmt: &hir::Stmt, ctx: &EmitCtx) -> Result<(), WasmError> {
    match stmt {
        hir::Stmt::Let { pattern, value, .. } => {
            // A scalarized aggregate is set field-by-field into its wasm locals
            // (no box); any other binding evaluates the initializer onto the
            // stack and binds it via the (irrefutable) pattern — the same binder
            // `match` uses, never hitting a refutable arm.
            let scalar = match pattern {
                hir::Pattern::Binding { symbol, .. } if ctx.scalarized.contains_key(symbol) => {
                    Some(*symbol)
                }
                _ => None,
            };
            if let Some(symbol) = scalar {
                emit_scalar_let(f, symbol, value, ctx)?;
            } else {
                // A simple `let x = <Copy aggregate>` binds a *copy* of the
                // value (the source stays alive), and a simple `let x = <field>`
                // of an inline aggregate moves its bytes out into a fresh box —
                // either way deep-copy rather than alias the source's box.
                // Destructuring patterns bind scalar fields (already independent
                // copies) and need no deep copy here.
                let copy = matches!(pattern, hir::Pattern::Binding { .. })
                    && needs_value_copy(value, ctx.policy);
                if copy {
                    emit_copy_value(f, value, ctx)?;
                } else {
                    emit_expr(f, value, ctx)?;
                }
                // `let` patterns never bind `mut` (mode `Read`), so the
                // write-back list stays empty by construction.
                emit_test_bind(
                    f,
                    Src::Stack,
                    &value.ty,
                    pattern,
                    ctx,
                    &mut Vec::new(),
                    true,
                );
            }
        }
        hir::Stmt::Assign { target, value, .. } => {
            // `a = b` for a `Copy` aggregate copies the value; deep-copy its
            // box so the assignment doesn't alias `b`'s box.
            emit_copy_value(f, value, ctx)?;
            if let Some(&idx) = ctx.locals.get(target) {
                f.instruction(&Instruction::LocalSet(idx));
            } else if let Some(g_idx) = global_wasm_index(ctx, *target) {
                f.instruction(&Instruction::GlobalSet(g_idx));
            } else {
                return Err(WasmError::Internal("assignment to unresolved name".into()));
            }
        }
        hir::Stmt::Return { value, .. } => {
            if let Some(expr) = value {
                if ctx.ret_fields.is_some() {
                    emit_scalar_value(f, expr, ctx)?;
                } else {
                    emit_move_value(f, expr, ctx)?;
                }
            }
            f.instruction(&Instruction::Return);
        }
        hir::Stmt::Drop { sym, ty, .. } => {
            emit_drop(f, *sym, ty, ctx);
        }
        hir::Stmt::DerefAssign { ptr, value, .. } => {
            let pointee = match &ptr.ty {
                hir::Type::Pointer { pointee, .. } => (**pointee).clone(),
                _ => hir::Type::U8,
            };
            emit_expr(f, ptr, ctx)?;
            emit_expr(f, value, ctx)?;
            if ctx.policy.is_inline(&pointee) {
                // Inline pointee: copy the value's bytes into the slot
                // (dest = slot address, src = value box/place, len = size).
                let size = ctx.policy.inline_size(&pointee) as i32;
                if store_orphans_box(value, ctx.policy) {
                    // `value` is a whole moved box: stash its pointer, copy its
                    // bytes into the slot, then free the now-orphaned box (the
                    // move checker consumed `value`, so drop elaboration won't).
                    let stash = ctx.scratch_base + ctx.scratch_counter.get();
                    ctx.scratch_counter.set(ctx.scratch_counter.get() + 1);
                    f.instruction(&Instruction::LocalTee(stash));
                    f.instruction(&Instruction::I32Const(size));
                    f.instruction(&Instruction::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
                    f.instruction(&Instruction::LocalGet(stash));
                    f.instruction(&Instruction::Call(ctx.builtins.free));
                } else {
                    // `value` is an inline field read: an address into its
                    // parent box (nothing orphaned), so just copy those bytes.
                    f.instruction(&Instruction::I32Const(size));
                    f.instruction(&Instruction::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
                }
            } else {
                emit_field_store(f, &pointee, 0);
            }
        }
        hir::Stmt::FieldAssign {
            object,
            field,
            value,
            ..
        } => {
            // Push the struct pointer, look up the field's offset/type, push
            // the value, then store it into the field.
            let field_layout = match &object.ty {
                hir::Type::Struct(id, _) => ctx
                    .struct_layouts
                    .get(id)
                    .and_then(|layout| layout.fields.get(field).cloned()),
                _ => None,
            };
            emit_expr(f, object, ctx)?;
            match field_layout {
                Some((offset, ty)) if ctx.policy.is_inline(&ty) => {
                    // Inline field: object pointer is on the stack; copy the
                    // value's bytes into `object + offset` (dest, src, len).
                    if offset != 0 {
                        f.instruction(&Instruction::I32Const(offset as i32));
                        f.instruction(&Instruction::I32Add);
                    }
                    let size = ctx.policy.inline_size(&ty) as i32;
                    if store_orphans_box(value, ctx.policy) {
                        // `value` is a whole moved box: stash its pointer, copy
                        // its bytes into the field, then free the orphaned box.
                        emit_expr(f, value, ctx)?;
                        let stash = ctx.scratch_base + ctx.scratch_counter.get();
                        ctx.scratch_counter.set(ctx.scratch_counter.get() + 1);
                        f.instruction(&Instruction::LocalTee(stash));
                        f.instruction(&Instruction::I32Const(size));
                        f.instruction(&Instruction::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                        f.instruction(&Instruction::LocalGet(stash));
                        f.instruction(&Instruction::Call(ctx.builtins.free));
                    } else {
                        // An inline field read is an address into its parent
                        // box (nothing orphaned), so just copy those bytes.
                        emit_expr(f, value, ctx)?;
                        f.instruction(&Instruction::I32Const(size));
                        f.instruction(&Instruction::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                    }
                }
                Some((offset, ty)) => {
                    emit_expr(f, value, ctx)?;
                    emit_field_store(f, &ty, offset);
                }
                None => {
                    return Err(WasmError::Internal(
                        "field not found in struct layout".into(),
                    ));
                }
            }
        }
        hir::Stmt::Expr(expr) => {
            emit_expr(f, expr, ctx)?;
            if produces_value(&expr.ty) {
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Stmt::Loop { body, .. } => {
            ctx.enter_loop();
            f.instruction(&Instruction::Block(BlockType::Empty));
            ctx.enter_ctrl();
            f.instruction(&Instruction::Loop(BlockType::Empty));
            ctx.enter_ctrl();
            emit_block(f, body, ctx)?;
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            ctx.exit_loop();
        }
        hir::Stmt::While {
            condition, body, ..
        } => {
            ctx.enter_loop();
            f.instruction(&Instruction::Block(BlockType::Empty));
            ctx.enter_ctrl();
            f.instruction(&Instruction::Loop(BlockType::Empty));
            ctx.enter_ctrl();
            emit_expr(f, condition, ctx)?;
            f.instruction(&Instruction::I32Eqz);
            f.instruction(&Instruction::BrIf(1));
            emit_block(f, body, ctx)?;
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            f.instruction(&Instruction::End);
            ctx.exit_ctrl();
            ctx.exit_loop();
        }
        hir::Stmt::Break { .. } => {
            f.instruction(&Instruction::Br(ctx.break_target()));
        }
    }
    Ok(())
}

/// The wasm signature key `(param valtypes, result valtypes)` for an indirect
/// call whose callee has the given `fn(..) -> ..` type. Matches the signature
/// a closure function is emitted with (closures bypass the scalar ABI), so the
/// `call_indirect` type and the callee's own type agree.
fn indirect_sig_key(ty: &hir::Type) -> (Vec<ValType>, Vec<ValType>) {
    match ty {
        hir::Type::Fn { params, ret } => {
            let params = params.iter().map(hir_type_to_valtype).collect();
            let results = if produces_value(ret) {
                vec![hir_type_to_valtype(ret)]
            } else {
                Vec::new()
            };
            (params, results)
        }
        _ => unreachable!("indirect call callee must have fn type"),
    }
}

fn emit_expr(f: &mut Function, expr: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    match &expr.kind {
        hir::ExprKind::Int(n) => match hir_type_to_valtype(&expr.ty) {
            ValType::I64 => {
                f.instruction(&Instruction::I64Const(*n));
            }
            _ => {
                f.instruction(&Instruction::I32Const(*n as i32));
            }
        },
        hir::ExprKind::Float(v) => match &expr.ty {
            hir::Type::F32 => {
                f.instruction(&Instruction::F32Const((*v as f32).into()));
            }
            _ => {
                f.instruction(&Instruction::F64Const((*v).into()));
            }
        },
        hir::ExprKind::Bool(b) => {
            f.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
        }
        hir::ExprKind::Ident(sym) => {
            if let Some(&idx) = ctx.locals.get(sym) {
                f.instruction(&Instruction::LocalGet(idx));
            } else if let Some(g_idx) = global_wasm_index(ctx, *sym) {
                f.instruction(&Instruction::GlobalGet(g_idx));
            } else {
                return Err(WasmError::Internal("unresolved name in expression".into()));
            }
        }
        hir::ExprKind::Binary { op, left, right } => {
            emit_expr(f, left, ctx)?;
            emit_expr(f, right, ctx)?;
            emit_binary_op(f, *op, &left.ty);
        }
        hir::ExprKind::Call {
            func,
            args,
            arg_modes,
            ..
        } => {
            if let Some(&runtime) = ctx.runtime.get(func) {
                if runtime == hir::RuntimeAbi::DropInPlace {
                    emit_drop_in_place(f, &args[0], ctx)?;
                } else {
                    emit_runtime_call(f, runtime, args, ctx)?;
                }
            } else if ctx.funcs.contains_key(func) {
                emit_raw_call(f, func, args, arg_modes, ctx)?;
                // A scalar-ABI return arrives as N stack values; in this general
                // context, materialize them into a box so the value reads as the
                // usual aggregate pointer.
                if let Some(fields) = ctx.scalar_ret.get(func) {
                    materialize_scalar_box(f, &expr.ty, fields, ctx)?;
                }
            } else {
                return Err(WasmError::Internal("call to unresolved function".into()));
            }
        }
        // spawn(f): make a task continuation from `f` and append it to the
        // task table; table.grow leaves the new slot index (the handle).
        hir::ExprKind::Spawn { func } => {
            let fidx = *ctx
                .funcs
                .get(func)
                .expect("spawn target missing from func map");
            f.instruction(&Instruction::RefFunc(fidx));
            f.instruction(&Instruction::ContNew(ctx.builtins.cont_type));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::TableGrow(ctx.builtins.cont_table));
        }
        // A first-class block value is its function-table slot index (i32).
        hir::ExprKind::ClosureRef { func } => {
            let slot = *ctx
                .closure_table_slots
                .get(func)
                .expect("missing closure table slot");
            f.instruction(&Instruction::I32Const(slot as i32));
        }
        hir::ExprKind::StructLit {
            struct_id, fields, ..
        } => {
            emit_struct_lit(f, *struct_id, fields, ctx)?;
        }
        hir::ExprKind::VariantLit {
            enum_id,
            variant_idx,
            fields,
            ..
        } => {
            emit_variant_lit(f, *enum_id, *variant_idx, fields, ctx)?;
        }
        hir::ExprKind::Match {
            mode,
            scrutinee,
            arms,
        } => {
            emit_match(f, *mode, scrutinee, arms, &expr.ty, ctx)?;
        }
        hir::ExprKind::Field { base, field } => {
            if let Some(local) = scalar_base_local(base, &ScalarKey::Field(*field), ctx)? {
                f.instruction(&Instruction::LocalGet(local));
            } else {
                emit_expr(f, base, ctx)?;
                let struct_id = match &base.ty {
                    hir::Type::Struct(id, _) => *id,
                    _ => {
                        return Err(WasmError::Internal(
                            "field access on non-struct type".into(),
                        ));
                    }
                };
                let layout = match ctx.struct_layouts.get(&struct_id) {
                    Some(l) => l,
                    None => {
                        return Err(WasmError::Internal("missing struct layout".into()));
                    }
                };
                let (offset, ty) = match layout.fields.get(field) {
                    Some(t) => t.clone(),
                    None => {
                        return Err(WasmError::Internal(
                            "field not found in struct layout".into(),
                        ));
                    }
                };
                emit_field_value(f, &ty, offset, ctx);
            }
        }
        hir::ExprKind::TupleLit(elems) => {
            emit_tuple_lit(f, &expr.ty, elems, ctx)?;
        }
        hir::ExprKind::ArrayLit(elems) => {
            // A fixed array is a homogeneous tuple `(T, T, ..., T)`; build it
            // with the tuple machinery (layout, inline/box, per-element init).
            let (elem, n) = match &expr.ty {
                hir::Type::Array(e, len) => ((**e).clone(), array_const_len(len)),
                _ => {
                    return Err(WasmError::Internal(
                        "array literal with non-array type".into(),
                    ));
                }
            };
            let tuple_ty = hir::Type::Tuple(vec![elem; n]);
            emit_tuple_lit(f, &tuple_ty, elems, ctx)?;
        }
        hir::ExprKind::TupleIndex { base, index } => {
            if let Some(local) = scalar_base_local(base, &ScalarKey::Index(*index as usize), ctx)? {
                f.instruction(&Instruction::LocalGet(local));
            } else {
                emit_expr(f, base, ctx)?;
                let elem_types = match &base.ty {
                    hir::Type::Tuple(elems) => elems,
                    _ => {
                        return Err(WasmError::Internal("tuple index on non-tuple type".into()));
                    }
                };
                let layout = compute_tuple_layout(elem_types, ctx.policy);
                match layout.elems.get(*index as usize) {
                    Some((offset, ty)) => emit_field_value(f, ty, *offset, ctx),
                    None => {
                        return Err(WasmError::Internal("tuple index out of range".into()));
                    }
                }
            }
        }
        hir::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            emit_expr(f, condition, ctx)?;
            if let Some(else_block) = else_branch {
                if produces_value(&expr.ty) {
                    let vt = hir_type_to_valtype(&expr.ty);
                    f.instruction(&Instruction::If(BlockType::Result(vt)));
                } else {
                    f.instruction(&Instruction::If(BlockType::Empty));
                }
                ctx.enter_ctrl();
                emit_block(f, then_branch, ctx)?;
                f.instruction(&Instruction::Else);
                emit_block(f, else_block, ctx)?;
                f.instruction(&Instruction::End);
                ctx.exit_ctrl();
            } else {
                f.instruction(&Instruction::If(BlockType::Empty));
                ctx.enter_ctrl();
                emit_block(f, then_branch, ctx)?;
                f.instruction(&Instruction::End);
                ctx.exit_ctrl();
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::UnsafeBlock(block) => {
            emit_block(f, block, ctx)?;
        }
        hir::ExprKind::Str(_) => {
            emit_str_lit(f, &expr.ty, ctx);
        }
        hir::ExprKind::Deref(operand) => {
            // An inline-aggregate pointee is stored as its bytes in the slot;
            // reading it must produce an independent copy, so allocate a fresh
            // box and copy the slot's bytes into it. Two scratch i32 slots are
            // claimed up front (matching the pre-order walk in `walks.rs`).
            let (slot_local, box_local) = if ctx.policy.is_inline(&expr.ty) {
                let c = ctx.scratch_counter.get();
                ctx.scratch_counter.set(c + 2);
                (ctx.scratch_base + c, ctx.scratch_base + c + 1)
            } else {
                (0, 0)
            };
            emit_expr(f, operand, ctx)?;
            if ctx.policy.is_inline(&expr.ty) {
                emit_inline_deref_read(f, &expr.ty, slot_local, box_local, ctx)?;
            } else {
                emit_field_load(f, &expr.ty, 0);
            }
        }
        hir::ExprKind::Coerce {
            value,
            source_struct,
            target_trait,
        } => {
            // Materialize a fat pointer `{vtable_addr: i32, data_ptr: i32}`
            // on the bump heap from a concrete struct value.
            //
            // Three scratch i32 locals must be claimed before evaluating the
            // inner expression so the order matches the pre-order walk that
            // reserved them in `collect_scratch_types_expr`: `src_local` holds
            // the source box when a `Copy` aggregate must be deep-copied,
            // `data_local` holds the data pointer stored into the fat pointer,
            // and `fat_local` holds the fat pointer itself.
            let src_local = ctx.scratch_base + ctx.scratch_counter.get();
            let data_local = src_local + 1;
            let fat_local = src_local + 2;
            ctx.scratch_counter.set(ctx.scratch_counter.get() + 3);

            emit_expr(f, value, ctx)?;
            // The source evaluates to either an owned box pointer or (for an
            // inline field read) an interior address into a still-live parent
            // box. `data_ptr` must be an independently-owned box the trait
            // object can free, so:
            //   - an inline field read is boxed out (bytes copied to a fresh box),
            //   - a `Copy` aggregate rooted at a local stays live after the move
            //     (the move analysis treats it as a read), so it is deep-copied,
            //   - any other value is already a fresh/owned box and is taken as-is.
            if is_inline_field_read(value, ctx.policy)
                || (is_boxed_copy_aggregate(&value.ty, ctx.policy) && ident_root(value).is_some())
            {
                let size = ctx.policy.inline_size(&value.ty) as i32;
                f.instruction(&Instruction::LocalSet(src_local));
                f.instruction(&Instruction::I32Const(size));
                f.instruction(&Instruction::Call(ctx.builtins.alloc));
                f.instruction(&Instruction::LocalSet(data_local));
                f.instruction(&Instruction::LocalGet(data_local));
                f.instruction(&Instruction::LocalGet(src_local));
                f.instruction(&Instruction::I32Const(size));
                f.instruction(&Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            } else {
                // The struct value is already a pointer to its heap-allocated
                // data; the trait object takes ownership of that box directly.
                f.instruction(&Instruction::LocalSet(data_local));
            }

            f.instruction(&Instruction::I32Const(8));
            f.instruction(&Instruction::Call(ctx.builtins.alloc));
            f.instruction(&Instruction::LocalTee(fat_local));

            let vt_addr = *ctx
                .vtable_addr
                .get(&(*target_trait, *source_struct))
                .expect("missing vtable for coercion");
            f.instruction(&Instruction::I32Const(vt_addr as i32));
            f.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));

            f.instruction(&Instruction::LocalGet(fat_local));
            f.instruction(&Instruction::LocalGet(data_local));
            f.instruction(&Instruction::I32Store(MemArg {
                offset: 4,
                align: 2,
                memory_index: 0,
            }));

            f.instruction(&Instruction::LocalGet(fat_local));
        }
        hir::ExprKind::DynCall {
            receiver,
            trait_id,
            method_idx,
            args,
            arg_modes: _,
        } => {
            // Stash the fat pointer once so we can load data_ptr (for the
            // receiver argument) and vtable_addr (to index for the function
            // table slot) without reevaluating the receiver expression.
            // Scratch local is claimed before walking children.
            let fat_local = ctx.scratch_base + ctx.scratch_counter.get();
            ctx.scratch_counter.set(ctx.scratch_counter.get() + 1);

            emit_expr(f, receiver, ctx)?;
            f.instruction(&Instruction::LocalSet(fat_local));

            f.instruction(&Instruction::LocalGet(fat_local));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 4,
                align: 2,
                memory_index: 0,
            }));

            for arg in args {
                emit_expr(f, arg, ctx)?;
            }

            f.instruction(&Instruction::LocalGet(fat_local));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            f.instruction(&Instruction::I32Const((*method_idx as i32) * 4));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));

            let type_idx = *ctx
                .dyn_call_types
                .get(&(*trait_id, *method_idx))
                .expect("missing dyn call type index");
            f.instruction(&Instruction::CallIndirect {
                type_index: type_idx,
                table_index: 0,
            });
        }
        hir::ExprKind::IndirectCall { callee, args } => {
            // Stash the callee's function-table index (an i32) while the
            // arguments are pushed underneath it, then `call_indirect`.
            let callee_local = ctx.scratch_base + ctx.scratch_counter.get();
            ctx.scratch_counter.set(ctx.scratch_counter.get() + 1);

            emit_expr(f, callee, ctx)?;
            f.instruction(&Instruction::LocalSet(callee_local));

            for arg in args {
                emit_expr(f, arg, ctx)?;
            }

            f.instruction(&Instruction::LocalGet(callee_local));

            let key = indirect_sig_key(&callee.ty);
            let type_idx = *ctx
                .indirect_call_types
                .get(&key)
                .expect("missing indirect call type index");
            f.instruction(&Instruction::CallIndirect {
                type_index: type_idx,
                table_index: 0,
            });
        }
        hir::ExprKind::BitNot(operand) => {
            emit_expr(f, operand, ctx)?;
            if matches!(operand.ty, hir::Type::Bool) {
                // Logical NOT: bool is 0/1, so `== 0` flips it.
                f.instruction(&Instruction::I32Eqz);
                return Ok(());
            }
            match hir_type_to_valtype(&expr.ty) {
                ValType::I32 => {
                    f.instruction(&Instruction::I32Const(-1));
                    f.instruction(&Instruction::I32Xor);
                }
                ValType::I64 => {
                    f.instruction(&Instruction::I64Const(-1));
                    f.instruction(&Instruction::I64Xor);
                }
                _ => {
                    return Err(WasmError::Internal(
                        "bitwise-not on unsupported operand type".into(),
                    ));
                }
            }
        }
        hir::ExprKind::Neg(operand) => {
            // Floats have a dedicated negate; integers have none, so subtract
            // from zero (push 0, then the operand, then sub).
            match hir_type_to_valtype(&expr.ty) {
                ValType::F32 => {
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::F32Neg);
                }
                ValType::F64 => {
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::F64Neg);
                }
                ValType::I64 => {
                    f.instruction(&Instruction::I64Const(0));
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::I64Sub);
                }
                _ => {
                    f.instruction(&Instruction::I32Const(0));
                    emit_expr(f, operand, ctx)?;
                    f.instruction(&Instruction::I32Sub);
                }
            }
        }
        _ => {
            return Err(WasmError::Internal(
                "unsupported expression kind in codegen".into(),
            ));
        }
    }
    Ok(())
}

/// Emit a string literal: bump-allocate a `String { data, len, cap }` struct
/// pointing at the literal's bytes (already laid out in static memory by
/// the program-level pre-walk). `cap == len` since the storage isn't
/// growable in place.
fn emit_str_lit(f: &mut Function, ty: &hir::Type, ctx: &EmitCtx) {
    let scratch_idx = ctx.scratch_counter.get();
    ctx.scratch_counter.set(scratch_idx + 1);
    let str_idx = ctx.str_counter.get();
    ctx.str_counter.set(str_idx + 1);
    let scratch_local = ctx.scratch_base + scratch_idx;

    let site = match ctx.str_sites.get(str_idx as usize) {
        Some(s) => *s,
        None => {
            bail(
                f,
                ctx,
                "string-literal site index out of sync with pre-walk",
            );
            return;
        }
    };

    let struct_id = match ty {
        hir::Type::Struct(id, _) => *id,
        _ => {
            bail(f, ctx, "string literal with non-struct type");
            return;
        }
    };

    let string_layout = match ctx.string_layout {
        Some(layout) if layout.struct_id == struct_id => layout,
        None => {
            bail(f, ctx, "missing string layout");
            return;
        }
        Some(_) => {
            bail(f, ctx, "string layout struct id mismatch");
            return;
        }
    };

    // ptr = __alloc(struct_size)
    f.instruction(&Instruction::I32Const(string_layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(scratch_local));

    // store data ptr (static offset)
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.ptr as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: string_layout.data_offset as u64,
        align: 2,
        memory_index: 0,
    }));

    // store len
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: string_layout.len_offset as u64,
        align: 2,
        memory_index: 0,
    }));

    // store cap = len
    f.instruction(&Instruction::LocalGet(scratch_local));
    f.instruction(&Instruction::I32Const(site.len as i32));
    f.instruction(&Instruction::I32Store(MemArg {
        offset: string_layout.cap_offset as u64,
        align: 2,
        memory_index: 0,
    }));

    // result: ptr to the struct
    f.instruction(&Instruction::LocalGet(scratch_local));
}

/// Emit a user-function call: its arguments (each by-value if the callee's
/// parameter uses the scalar ABI, else as a pointer), then the `call`. Leaves
/// the callee's results on the stack — N values for a scalar-ABI return.
/// An `own` argument that is a read of an inline-aggregate field is moved out
/// of its parent box and must be boxed out (deep-copied) rather than aliased;
/// `read`/`mut` args alias their field.
fn emit_raw_call(
    f: &mut Function,
    func: &hir::FuncId,
    args: &[hir::Expr],
    arg_modes: &[hir::PassMode],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let idx = *ctx
        .funcs
        .get(func)
        .ok_or_else(|| WasmError::Internal("call to unresolved function".into()))?;
    let abi = ctx.scalar_abi.get(func);
    for (i, arg) in args.iter().enumerate() {
        if abi.is_some_and(|v| v.get(i).copied().unwrap_or(false)) {
            emit_scalar_value(f, arg, ctx)?;
        } else if arg_modes.get(i).copied() == Some(hir::PassMode::Own) {
            // A moved field read needs an independent box; a whole value moves
            // as its pointer.
            emit_move_value(f, arg, ctx)?;
        } else {
            emit_expr(f, arg, ctx)?;
        }
    }
    f.instruction(&Instruction::Call(idx));
    Ok(())
}

/// Materialize a scalar-ABI value (N leaf-field values on the stack, in
/// declaration order) into a fresh heap box, leaving the box pointer. The N
/// field temporaries and the box pointer are reserved for this call site in
/// `collect_scratch_types`.
fn materialize_scalar_box(
    f: &mut Function,
    ty: &hir::Type,
    fields: &[ScalarField],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let n = fields.len() as u32;
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + n + 1);
    let field_temp = |i: u32| ctx.scratch_base + counter + i;
    let box_temp = ctx.scratch_base + counter + n;

    // Pop the N values (the last field is on top) into temporaries.
    for i in (0..n).rev() {
        f.instruction(&Instruction::LocalSet(field_temp(i)));
    }
    let size = match ty {
        hir::Type::Struct(sid, _) => ctx
            .struct_layouts
            .get(sid)
            .map(|l| l.size)
            .ok_or_else(|| WasmError::Internal("missing struct layout".into()))?,
        hir::Type::Tuple(elems) => compute_tuple_layout(elems, ctx.policy).size,
        _ => {
            return Err(WasmError::Internal(
                "scalar-ABI value has no box layout".into(),
            ));
        }
    };
    f.instruction(&Instruction::I32Const(size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(box_temp));
    for (i, sf) in fields.iter().enumerate() {
        let (offset, fty) = scalar_field_offset(ty, &sf.key, ctx)?;
        f.instruction(&Instruction::LocalGet(box_temp));
        f.instruction(&Instruction::LocalGet(field_temp(i as u32)));
        emit_field_store(f, &fty, offset);
    }
    f.instruction(&Instruction::LocalGet(box_temp));
    Ok(())
}

/// Emit a flat-POD aggregate as a by-value scalar-ABI sequence (phase 3): push
/// one wasm value per leaf field, in declaration order. Used for by-value call
/// arguments and by-value returns. A scalarized local/param pushes its field
/// locals directly; a scalar-ABI-returning call passes its results straight
/// through; any other value is realized as a box (an existing local, a freshly
/// built literal, or a stashed pointer) and its fields are loaded.
fn emit_scalar_value(f: &mut Function, arg: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    let fields =
        flat_scalar_fields(&arg.ty, ctx.program, ctx.policy, ctx.info).ok_or_else(|| {
            WasmError::Internal("scalar-ABI value is not a flat scalar aggregate".into())
        })?;
    // A scalarized local/param: push its leaf-field locals directly (no box).
    if let hir::ExprKind::Ident(sym) = &arg.kind
        && let Some(scalar) = ctx.scalarized.get(sym)
    {
        for sf in &scalar.fields {
            let local = scalar
                .local_of(&sf.key)
                .ok_or_else(|| WasmError::Internal("scalar field local missing".into()))?;
            f.instruction(&Instruction::LocalGet(local));
        }
        return Ok(());
    }
    // A scalar-ABI-returning call already leaves N values on the stack — pass
    // them straight through (no box).
    if let hir::ExprKind::Call { func, args, .. } = &arg.kind
        && ctx.scalar_ret.contains_key(func)
    {
        return emit_raw_call(f, func, args, &[], ctx);
    }
    // Otherwise the value lives in a box; find a local holding its pointer.
    let box_local = match &arg.kind {
        hir::ExprKind::Ident(sym) => *ctx
            .locals
            .get(sym)
            .ok_or_else(|| WasmError::Internal("scalar-ABI argument local missing".into()))?,
        hir::ExprKind::StructLit {
            struct_id,
            fields: lit,
            ..
        } => {
            let ptr = emit_struct_lit(f, *struct_id, lit, ctx)?;
            f.instruction(&Instruction::Drop); // the pushed pointer; re-read via `ptr`
            ptr
        }
        hir::ExprKind::TupleLit(elems) => {
            let ptr = emit_tuple_lit(f, &arg.ty, elems, ctx)?;
            f.instruction(&Instruction::Drop);
            ptr
        }
        // General case: evaluate to a box pointer and stash it in a scratch
        // local (reserved for this argument in `collect_scratch_types`).
        _ => {
            emit_expr(f, arg, ctx)?;
            let counter = ctx.scratch_counter.get();
            ctx.scratch_counter.set(counter + 1);
            let stash = ctx.scratch_base + counter;
            f.instruction(&Instruction::LocalSet(stash));
            stash
        }
    };
    for sf in &fields {
        let (offset, fty) = scalar_field_offset(&arg.ty, &sf.key, ctx)?;
        f.instruction(&Instruction::LocalGet(box_local));
        emit_field_load(f, &fty, offset);
    }
    Ok(())
}

/// The `(byte offset, type)` of a leaf field within its aggregate's box layout.
fn scalar_field_offset(
    ty: &hir::Type,
    key: &ScalarKey,
    ctx: &EmitCtx,
) -> Result<(u32, hir::Type), WasmError> {
    match (ty, key) {
        (hir::Type::Struct(sid, _), ScalarKey::Field(sym)) => ctx
            .struct_layouts
            .get(sid)
            .and_then(|l| l.fields.get(sym))
            .cloned()
            .ok_or_else(|| WasmError::Internal("scalar field offset missing".into())),
        (hir::Type::Tuple(elems), ScalarKey::Index(i)) => compute_tuple_layout(elems, ctx.policy)
            .elems
            .get(*i)
            .cloned()
            .ok_or_else(|| WasmError::Internal("scalar tuple offset missing".into())),
        _ => Err(WasmError::Internal("scalar field key/type mismatch".into())),
    }
}

/// If `base` is a scalarized aggregate local (`Ident`), the wasm local holding
/// the addressed leaf field; `Ok(None)` if `base` is not scalarized (the caller
/// falls back to address-based access).
fn scalar_base_local(
    base: &hir::Expr,
    key: &ScalarKey,
    ctx: &EmitCtx,
) -> Result<Option<u32>, WasmError> {
    if let hir::ExprKind::Ident(sym) = &base.kind
        && let Some(scalar) = ctx.scalarized.get(sym)
    {
        return match scalar.local_of(key) {
            Some(local) => Ok(Some(local)),
            None => Err(WasmError::Internal("scalarized field has no local".into())),
        };
    }
    Ok(None)
}

/// Bind a scalarized aggregate `let` (phase 2a): set each leaf-field wasm local
/// from the corresponding literal field. No box is allocated. The first scratch
/// slot — the box pointer `collect_scratch_types` reserved for this literal — is
/// skipped to keep the scratch pre-walk aligned with codegen.
fn emit_scalar_let(
    f: &mut Function,
    symbol: hir::SymbolId,
    value: &hir::Expr,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let set_field = |f: &mut Function, ctx: &EmitCtx, key: &ScalarKey| -> Result<(), WasmError> {
        match ctx.scalarized.get(&symbol).and_then(|s| s.local_of(key)) {
            Some(local) => {
                f.instruction(&Instruction::LocalSet(local));
                Ok(())
            }
            None => Err(WasmError::Internal("scalarized field has no local".into())),
        }
    };
    // Skip the box-pointer scratch slot `collect_scratch_types` reserved for a
    // struct/tuple literal (the scalarized value uses no box).
    let skip_box_slot = |ctx: &EmitCtx| {
        let c = ctx.scratch_counter.get();
        ctx.scratch_counter.set(c + 1);
    };
    match &value.kind {
        hir::ExprKind::StructLit { fields, .. } => {
            skip_box_slot(ctx);
            for (fsym, fval) in fields {
                emit_expr(f, fval, ctx)?;
                set_field(f, ctx, &ScalarKey::Field(*fsym))?;
            }
            Ok(())
        }
        hir::ExprKind::TupleLit(elems) => {
            skip_box_slot(ctx);
            for (i, elem) in elems.iter().enumerate() {
                emit_expr(f, elem, ctx)?;
                set_field(f, ctx, &ScalarKey::Index(i))?;
            }
            Ok(())
        }
        // A scalar-ABI-returning call leaves N values; consume them into the
        // locals (the last field is on top), then skip the N+1 materialize temps
        // that `collect_scratch_types` reserved for this call.
        hir::ExprKind::Call { func, args, .. } if ctx.scalar_ret.contains_key(func) => {
            emit_raw_call(f, func, args, &[], ctx)?;
            let scalar = ctx
                .scalarized
                .get(&symbol)
                .cloned()
                .ok_or_else(|| WasmError::Internal("scalarized let symbol missing".into()))?;
            for sf in scalar.fields.iter().rev() {
                set_field(f, ctx, &sf.key)?;
            }
            let c = ctx.scratch_counter.get();
            ctx.scratch_counter.set(c + scalar.fields.len() as u32 + 1);
            Ok(())
        }
        _ => Err(WasmError::Internal(
            "scalarized let with unsupported initializer".into(),
        )),
    }
}

/// Initialize a field at `base_local + offset` from `value`. An inline
/// aggregate field is built or copied in place (no sub-allocation); a scalar or
/// boxed-aggregate field stores its value/pointer as before. `base_local` holds
/// the containing box's pointer.
fn emit_field_init(
    f: &mut Function,
    base_local: u32,
    offset: u32,
    value: &hir::Expr,
    field_ty: &hir::Type,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    if ctx.policy.is_inline(field_ty) {
        emit_aggregate_into(f, base_local, offset, value, field_ty, ctx)
    } else {
        f.instruction(&Instruction::LocalGet(base_local));
        emit_expr(f, value, ctx)?;
        emit_field_store(f, field_ty, offset);
        Ok(())
    }
}

/// Write an inline aggregate `value` of type `ty` into `base_local + dest`. A
/// literal is built field-by-field directly into place (no allocation); any
/// other value is an address, whose `inline_size` bytes are copied in.
fn emit_aggregate_into(
    f: &mut Function,
    base_local: u32,
    dest: u32,
    value: &hir::Expr,
    ty: &hir::Type,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    match &value.kind {
        hir::ExprKind::StructLit {
            struct_id, fields, ..
        } => {
            let layout = match ctx.struct_layouts.get(struct_id) {
                Some(l) => l.clone(),
                None => {
                    bail(f, ctx, "missing struct layout");
                    return Ok(());
                }
            };
            for (fsym, fval) in fields {
                let Some((foff, fty)) = layout.fields.get(fsym).cloned() else {
                    bail(f, ctx, "field not found in struct layout");
                    continue;
                };
                emit_field_init(f, base_local, dest + foff, fval, &fty, ctx)?;
            }
            Ok(())
        }
        hir::ExprKind::TupleLit(elems) => {
            let elem_types = match &value.ty {
                hir::Type::Tuple(ts) => ts.clone(),
                _ => {
                    bail(f, ctx, "tuple literal with non-tuple type");
                    return Ok(());
                }
            };
            let layout = compute_tuple_layout(&elem_types, ctx.policy);
            for (elem, (eoff, ety)) in elems.iter().zip(layout.elems.iter()) {
                emit_field_init(f, base_local, dest + *eoff, elem, ety, ctx)?;
            }
            Ok(())
        }
        // A non-literal inline value evaluates to an address; copy its bytes
        // into the destination. Stack order for `memory.copy` is dest, src, len.
        _ => {
            f.instruction(&Instruction::LocalGet(base_local));
            if dest != 0 {
                f.instruction(&Instruction::I32Const(dest as i32));
                f.instruction(&Instruction::I32Add);
            }
            emit_expr(f, value, ctx)?;
            f.instruction(&Instruction::I32Const(ctx.policy.inline_size(ty) as i32));
            f.instruction(&Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
            Ok(())
        }
    }
}

/// Build a struct literal box, leave its pointer on the stack, and return the
/// scratch local that also holds it (so callers like `emit_scalar_arg` can
/// re-read the box without an extra temporary).
fn emit_struct_lit(
    f: &mut Function,
    struct_id: hir::StructId,
    fields: &[(hir::InternSymbol, hir::Expr)],
    ctx: &EmitCtx,
) -> Result<u32, WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let ptr_local = ctx.scratch_base + counter;

    let layout = match ctx.struct_layouts.get(&struct_id) {
        Some(l) => l.clone(),
        None => {
            bail(f, ctx, "missing struct layout");
            return Ok(ptr_local);
        }
    };

    // ptr_local = __alloc(size)
    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));

    // For each field: store at offset
    for (field_sym, value) in fields {
        let (offset, field_ty) = match layout.fields.get(field_sym) {
            Some(t) => t.clone(),
            None => {
                bail(f, ctx, "field not found in struct layout");
                continue;
            }
        };
        emit_field_init(f, ptr_local, offset, value, &field_ty, ctx)?;
    }

    // Push ptr as the struct value
    f.instruction(&Instruction::LocalGet(ptr_local));
    Ok(ptr_local)
}

/// Emit a tuple literal: heap-allocate the positional layout, store each
/// element at its offset, and leave the pointer on the stack. Mirrors
/// `emit_struct_lit` but the layout is computed from `tuple_ty`'s elements.
fn emit_tuple_lit(
    f: &mut Function,
    tuple_ty: &hir::Type,
    elems: &[hir::Expr],
    ctx: &EmitCtx,
) -> Result<u32, WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let ptr_local = ctx.scratch_base + counter;

    let elem_types = match tuple_ty {
        hir::Type::Tuple(ts) => ts,
        _ => {
            bail(f, ctx, "tuple literal with non-tuple type");
            return Ok(ptr_local);
        }
    };
    let layout = compute_tuple_layout(elem_types, ctx.policy);

    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));

    for (value, (offset, elem_ty)) in elems.iter().zip(layout.elems.iter()) {
        emit_field_init(f, ptr_local, *offset, value, elem_ty, ctx)?;
    }

    f.instruction(&Instruction::LocalGet(ptr_local));
    Ok(ptr_local)
}

fn emit_variant_lit(
    f: &mut Function,
    enum_id: hir::EnumId,
    variant_idx: u32,
    fields: &[(hir::InternSymbol, hir::Expr)],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let ptr_local = ctx.scratch_base + counter;

    let layout = match ctx.enum_layouts.get(&enum_id) {
        Some(l) => l,
        None => {
            bail(f, ctx, "missing enum layout");
            return Ok(());
        }
    };
    let variant = match layout.variants.get(variant_idx as usize) {
        Some(v) => v,
        None => {
            bail(f, ctx, "missing enum variant layout");
            return Ok(());
        }
    };

    f.instruction(&Instruction::I32Const(layout.size as i32));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));

    f.instruction(&Instruction::LocalGet(ptr_local));
    f.instruction(&Instruction::I32Const(variant_idx as i32));
    emit_field_store(f, &hir::Type::U32, 0);

    for (field_sym, value) in fields {
        let (payload_offset, field_ty) = match variant.fields.get(field_sym) {
            Some(t) => t.clone(),
            None => {
                bail(f, ctx, "variant field not found in layout");
                continue;
            }
        };
        emit_field_init(f, ptr_local, 8 + payload_offset, value, &field_ty, ctx)?;
    }

    f.instruction(&Instruction::LocalGet(ptr_local));
    Ok(())
}

/// Where the value a sub-pattern matches against lives.
#[derive(Clone, Copy)]
enum Src {
    /// Directly in a local (the scrutinee, or a stashed aggregate pointer).
    Local(u32),
    /// At `base + offset` in linear memory (a tuple element or variant field).
    Field { base: u32, offset: u32 },
    /// On top of the operand stack (a `let` initializer just evaluated). Unlike
    /// a local or field it can be read only once, so any pattern that needs the
    /// value more than once stashes it to a scratch local first (`aggregate_base`).
    Stack,
}

/// A `mut` binding made by a `match mut` arm, recorded so the arm's writes can
/// be copied back into the scrutinee's place at arm end. Only *scalar* payloads
/// are recorded: an aggregate binding aliases the scrutinee (its local holds a
/// pointer to the box/payload region), so writes already land in the box; a
/// scalar binding is a plain copy whose writes would otherwise be discarded.
struct MutWriteBack {
    src: Src,
    ty: hir::Type,
    symbol: hir::SymbolId,
}

/// Whether a value of this type is carried in a wasm scalar. A `mut` binding of
/// a scalar copies the value (and needs write-back); an aggregate aliases.
fn is_scalar_ty(ty: &hir::Type) -> bool {
    matches!(
        ty,
        hir::Type::U8
            | hir::Type::I8
            | hir::Type::U16
            | hir::Type::I16
            | hir::Type::U32
            | hir::Type::I32
            | hir::Type::U64
            | hir::Type::I64
            | hir::Type::Usize
            | hir::Type::Isize
            | hir::Type::F32
            | hir::Type::F64
            | hir::Type::Bool
    )
}

/// Copy a `mut` binding's value back into the place it was bound from, at the
/// end of a `match mut` arm. `Src::Stack` can't occur for a match binding (the
/// scrutinee is always stashed in a local first).
fn emit_mut_write_backs(f: &mut Function, wb: &[MutWriteBack], ctx: &EmitCtx) {
    for w in wb {
        let Some(&local) = ctx.locals.get(&w.symbol) else {
            continue;
        };
        match w.src {
            Src::Local(idx) => {
                f.instruction(&Instruction::LocalGet(local));
                f.instruction(&Instruction::LocalSet(idx));
            }
            Src::Field { base, offset } => {
                f.instruction(&Instruction::LocalGet(base));
                f.instruction(&Instruction::LocalGet(local));
                emit_field_store(f, &w.ty, offset);
            }
            Src::Stack => {}
        }
    }
}

/// Compile a `match` as a chain of fail-blocks wrapped in a result-carrying
/// block. Each arm tests-and-binds its pattern, short-circuiting to its
/// fail-block on the first mismatch (so loads behind a non-matching
/// constructor are never executed), then runs its body and branches to `$done`.
fn emit_match(
    f: &mut Function,
    mode: hir::PassMode,
    scrutinee: &hir::Expr,
    arms: &[hir::MatchArm],
    result_ty: &hir::Type,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 1);
    let scrutinee_local = ctx.scratch_base + counter;
    emit_expr(f, scrutinee, ctx)?;
    f.instruction(&Instruction::LocalSet(scrutinee_local));

    let produces = produces_value(result_ty);
    // `$done`: every matched arm branches here with its body's value.
    let done_level = ctx.ctrl_depth.get();
    if produces {
        f.instruction(&Instruction::Block(BlockType::Result(hir_type_to_valtype(
            result_ty,
        ))));
    } else {
        f.instruction(&Instruction::Block(BlockType::Empty));
    }
    ctx.enter_ctrl();

    // A consumed scrutinee transfers ownership into the arm; once its payload is
    // bound, the match owns the box(es) the arm did not move out and frees them.
    // Consumption is always inferred from the arms (`match own`/`match mut`
    // modes don't force it) — an `own` binding moves a payload out.
    let consuming = hir::cfg::match_consumes(&ctx.program.copy_types, arms);
    let mut mut_wb: Vec<MutWriteBack> = Vec::new();
    for arm in arms {
        // `$fail`: a mismatch in this arm's test branches to its end, falling
        // through to the next arm.
        f.instruction(&Instruction::Block(BlockType::Empty));
        ctx.enter_ctrl();
        emit_test_bind(
            f,
            Src::Local(scrutinee_local),
            &scrutinee.ty,
            &arm.pattern,
            ctx,
            &mut mut_wb,
            false,
        );
        emit_expr(f, &arm.body, ctx)?;
        // A consumed scrutinee transfers ownership into the arm; after the arm
        // body (and its bindings' drops) ran, reclaim the scrutinee's box(es)
        // for the fields the arm did not move out.
        if consuming {
            emit_consume_cleanup(f, scrutinee_local, &[], &scrutinee.ty, &arm.pattern, ctx);
        }
        // `match mut`: scalar `mut` bindings are copies, so copy them back into
        // the scrutinee's place at arm end (aggregate bindings alias and need
        // no copy-back). Skipped when the scrutinee is consumed — the boxes
        // were freed and there is no place to write back to.
        if mode == hir::PassMode::Mut && !consuming {
            emit_mut_write_backs(f, &mut_wb, ctx);
        }
        mut_wb.clear();
        let done_br = ctx.ctrl_depth.get() - 1 - done_level;
        f.instruction(&Instruction::Br(done_br));
        f.instruction(&Instruction::End);
        ctx.exit_ctrl();
    }

    // Exhaustiveness is checked at typecheck, so no arm matching is
    // unreachable at runtime; `Unreachable` also satisfies the block's result.
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    ctx.exit_ctrl();
    Ok(())
}

/// Push the value identified by `src` (of type `ty`) onto the stack.
/// Given an aggregate's base pointer already on the stack, push a field's value
/// at `offset`. An inline aggregate field's "value" is its address (`base +
/// offset`); a scalar or boxed-aggregate field is loaded.
fn emit_field_value(f: &mut Function, ty: &hir::Type, offset: u32, ctx: &EmitCtx) {
    if ctx.policy.is_inline(ty) {
        if offset != 0 {
            f.instruction(&Instruction::I32Const(offset as i32));
            f.instruction(&Instruction::I32Add);
        }
    } else {
        emit_field_load(f, ty, offset);
    }
}

/// Deep-copy the inline bytes of a `Copy` aggregate field (`src = Src::Field`)
/// into a fresh heap box and leave the box pointer on the stack. This is the
/// value-semantics counterpart to `emit_field_value`'s aliasing address: used
/// when a destructuring pattern binds a nested `Copy` field as a value. Claims
/// two scratch locals (`addr`/`dst`), reserved by the pattern walk.
fn emit_field_copy_out(f: &mut Function, src: &Src, ty: &hir::Type, ctx: &EmitCtx) {
    let Src::Field { base, offset } = src else {
        return; // only field bindings need a box-out; others are already boxes
    };
    let size = ctx.policy.inline_size(ty) as i32;
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 2);
    let addr = ctx.scratch_base + counter;
    let dst = ctx.scratch_base + counter + 1;

    // source address = base + offset
    f.instruction(&Instruction::LocalGet(*base));
    if *offset != 0 {
        f.instruction(&Instruction::I32Const(*offset as i32));
        f.instruction(&Instruction::I32Add);
    }
    f.instruction(&Instruction::LocalSet(addr));
    f.instruction(&Instruction::I32Const(size));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalTee(dst));
    f.instruction(&Instruction::LocalGet(addr));
    f.instruction(&Instruction::I32Const(size));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(dst));
}

/// Deref-read of an inline-aggregate pointee: the slot address is on the stack;
/// allocate a fresh box of the inline size, copy the slot's bytes into it, and
/// leave the box pointer as the result (an independent copy, so dropping it can
/// never alias the slot). `slot_local`/`box_local` are the two scratch locals
/// claimed by the `Deref` arm before evaluating the operand.
fn emit_inline_deref_read(
    f: &mut Function,
    ty: &hir::Type,
    slot_local: u32,
    box_local: u32,
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    let size = ctx.policy.inline_size(ty) as i32;
    // stack: [slot_addr]
    f.instruction(&Instruction::LocalSet(slot_local));
    f.instruction(&Instruction::I32Const(size));
    f.instruction(&Instruction::Call(ctx.builtins.alloc));
    f.instruction(&Instruction::LocalSet(box_local));
    // dest = box, src = slot, len = size
    f.instruction(&Instruction::LocalGet(box_local));
    f.instruction(&Instruction::LocalGet(slot_local));
    f.instruction(&Instruction::I32Const(size));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(box_local));
    Ok(())
}

/// Whether `ty` is a `Copy` *aggregate* (a struct with `impl Copy`), carried as
/// a heap-box pointer. A `Copy` value stays alive after a copy site, so copying
/// the box pointer would alias — it needs a deep copy of the box bytes instead.
/// Scalars and raw pointers are also `Copy`, but their value is already
/// self-contained (no box); tuples/enums are never `Copy` today.
pub(crate) fn is_boxed_copy_aggregate(ty: &hir::Type, policy: &InlinePolicy) -> bool {
    matches!(ty, hir::Type::Struct(..)) && policy.is_copy(ty)
}

/// Whether a move/copy site must deep-copy `value` into a fresh box instead of
/// moving its pointer: a `Copy` aggregate (the source stays alive), or an
/// inline-aggregate field/tuple-index whose bytes live inside the parent box
/// (moving them out requires an independent copy).
pub(crate) fn needs_value_copy(value: &hir::Expr, policy: &InlinePolicy) -> bool {
    is_boxed_copy_aggregate(&value.ty, policy) || is_inline_field_read(value, policy)
}

/// Whether `value` is a read of an inline-aggregate field/tuple element — an
/// address into the parent box rather than an independently-owned box.
pub(crate) fn is_inline_field_read(value: &hir::Expr, policy: &InlinePolicy) -> bool {
    policy.is_inline(&value.ty)
        && matches!(
            value.kind,
            hir::ExprKind::Field { .. } | hir::ExprKind::TupleIndex { .. }
        )
}

/// Whether copying `value` into an inline slot/field (`*p = value` or
/// `x.f = value`) orphans a whole heap box that must be freed afterwards:
/// `value` is an inline aggregate but *not* an inline field read (which is an
/// address into its parent's box, with no allocation of its own). The move
/// checker consumes `value` at both sites, so drop elaboration won't reclaim
/// the box — the store must.
pub(crate) fn store_orphans_box(value: &hir::Expr, policy: &InlinePolicy) -> bool {
    policy.is_inline(&value.ty) && !is_inline_field_read(value, policy)
}

/// Evaluate `value` and leave an *independent* copy on the stack. For a `Copy`
/// aggregate or an inline-aggregate field read, that is a fresh heap box whose
/// bytes are copied from the evaluated value's box/place; for anything else it
/// is just `emit_expr` (the value is already self-contained).
///
/// This is the codegen side of value semantics: `let b = a`, `a = b`, and a
/// moved-out field must not alias the source's box. Consumes two scratch locals
/// (`src`/`dst`), which the matching `walks` reservation provides.
fn emit_copy_value(f: &mut Function, value: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    if !needs_value_copy(value, ctx.policy) {
        return emit_expr(f, value, ctx);
    }
    let size = ctx.policy.inline_size(&value.ty) as i32;
    let counter = ctx.scratch_counter.get();
    ctx.scratch_counter.set(counter + 2);
    let src = ctx.scratch_base + counter;
    let dst = ctx.scratch_base + counter + 1;

    emit_expr(f, value, ctx)?; // [src]
    f.instruction(&Instruction::LocalSet(src)); // []
    f.instruction(&Instruction::I32Const(size)); // [size]
    f.instruction(&Instruction::Call(ctx.builtins.alloc)); // [dst]
    f.instruction(&Instruction::LocalTee(dst)); // [dst] and dst = dst
    f.instruction(&Instruction::LocalGet(src)); // [dst, src]
    f.instruction(&Instruction::I32Const(size)); // [dst, src, size]
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    }); // []
    f.instruction(&Instruction::LocalGet(dst)); // [dst]
    Ok(())
}

/// Evaluate a *move* of `value` onto the stack: a read of an inline-aggregate
/// field/tuple element must be boxed out (its bytes live in the parent, which
/// may outlive or be freed independently), while any other value is already a
/// self-contained box and moves as its pointer. Used at `return` and `own` call
/// arguments, where a `Copy` value needn't be copied (the source dies) but a
/// field address must be.
fn emit_move_value(f: &mut Function, value: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    if is_inline_field_read(value, ctx.policy) {
        emit_copy_value(f, value, ctx)
    } else {
        emit_expr(f, value, ctx)
    }
}

fn push_src(f: &mut Function, src: Src, ty: &hir::Type, ctx: &EmitCtx) {
    match src {
        Src::Local(idx) => {
            f.instruction(&Instruction::LocalGet(idx));
        }
        Src::Field { base, offset } => {
            f.instruction(&Instruction::LocalGet(base));
            emit_field_value(f, ty, offset, ctx);
        }
        // Already on the operand stack — nothing to push. Valid only for a
        // single consuming use (a leaf binding); callers needing it repeatedly
        // route through `aggregate_base`.
        Src::Stack => {}
    }
}

/// Recursively test a value (`src`, of type `ty`) against `pattern`, branching
/// to the enclosing `$fail` block (relative depth 0 — no blocks are opened
/// here) on any mismatch, and binding any sub-bindings along the matching path.
/// `owning` is true for a `let` pattern (every binding takes ownership) and
/// false for a `match` arm (bare/`read`/`mut` bindings borrow; `own` moves).
/// `mut_wb` collects the arm's `mut` bindings of scalar payloads (see
/// [`MutWriteBack`]) so `emit_match` can copy their writes back.
fn emit_test_bind(
    f: &mut Function,
    src: Src,
    ty: &hir::Type,
    pattern: &hir::Pattern,
    ctx: &EmitCtx,
    mut_wb: &mut Vec<MutWriteBack>,
    owning: bool,
) {
    match pattern {
        hir::Pattern::Wildcard { .. } => {
            // A value left on the stack (a `let _ = expr`) must be dropped to
            // keep the stack balanced; one in a local/field is just ignored.
            if matches!(src, Src::Stack) && produces_value(ty) {
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Pattern::Binding { symbol, mode, .. } => {
            if let Some(&local) = ctx.locals.get(symbol) {
                // A field binding of an inline aggregate is an *alias* of the
                // enclosing value's bytes unless the binding takes ownership
                // (a `let`, an `own` match binding, or a `Copy` value) — in
                // which case it is a value and must be deep-copied out.
                let field_copy = matches!(src, Src::Field { .. })
                    && ctx.policy.is_inline(ty)
                    && (ctx.policy.is_copy(ty) || *mode == hir::PassMode::Own || owning);
                if field_copy {
                    emit_field_copy_out(f, &src, ty, ctx);
                } else {
                    push_src(f, src, ty, ctx);
                }
                f.instruction(&Instruction::LocalSet(local));
                // A `mut` binding of a scalar or `Copy` aggregate payload is a
                // copy (a plain aggregate binding aliases); record it so the
                // writes copy back to the scrutinee.
                if *mode == hir::PassMode::Mut
                    && (is_scalar_ty(ty) || is_boxed_copy_aggregate(ty, ctx.policy))
                {
                    mut_wb.push(MutWriteBack {
                        src,
                        ty: ty.clone(),
                        symbol: *symbol,
                    });
                }
            } else if matches!(src, Src::Stack) && produces_value(ty) {
                // Unbound name fed from the stack: discard to stay balanced.
                f.instruction(&Instruction::Drop);
            }
        }
        hir::Pattern::Int { value, ty: pty, .. } => {
            push_src(f, src, ty, ctx);
            if matches!(hir_type_to_valtype(pty), ValType::I64) {
                f.instruction(&Instruction::I64Const(*value));
                f.instruction(&Instruction::I64Ne);
            } else {
                f.instruction(&Instruction::I32Const(*value as i32));
                f.instruction(&Instruction::I32Ne);
            }
            f.instruction(&Instruction::BrIf(0));
        }
        hir::Pattern::Bool { value, .. } => {
            push_src(f, src, ty, ctx);
            f.instruction(&Instruction::I32Const(*value as i32));
            f.instruction(&Instruction::I32Ne);
            f.instruction(&Instruction::BrIf(0));
        }
        hir::Pattern::Tuple { elems, .. } => {
            let base = aggregate_base(f, src, ty, ctx);
            let elem_types: Vec<hir::Type> = match ty {
                hir::Type::Tuple(ts) => ts.clone(),
                _ => Vec::new(),
            };
            let layout = compute_tuple_layout(&elem_types, ctx.policy);
            for (elem, (offset, elem_ty)) in elems.iter().zip(layout.elems.iter()) {
                emit_test_bind(
                    f,
                    Src::Field {
                        base,
                        offset: *offset,
                    },
                    elem_ty,
                    elem,
                    ctx,
                    mut_wb,
                    owning,
                );
            }
        }
        hir::Pattern::Variant {
            variant_idx,
            fields,
            ..
        } => {
            // Use the enum id from the value's (monomorphized) type, not the
            // pattern's — mono rewrites types but not the pattern's `enum_id`.
            let enum_id = match ty {
                hir::Type::Enum(id, _) => *id,
                _ => {
                    bail(f, ctx, "variant pattern on non-enum type");
                    return;
                }
            };
            let base = aggregate_base(f, src, ty, ctx);
            // Discriminant test.
            f.instruction(&Instruction::LocalGet(base));
            emit_field_load(f, &hir::Type::U32, 0);
            f.instruction(&Instruction::I32Const(*variant_idx as i32));
            f.instruction(&Instruction::I32Ne);
            f.instruction(&Instruction::BrIf(0));

            let variant = ctx
                .enum_layouts
                .get(&enum_id)
                .and_then(|layout| layout.variants.get(*variant_idx as usize))
                .cloned();
            let Some(variant) = variant else {
                bail(f, ctx, "missing enum variant layout");
                return;
            };
            for fp in fields {
                let Some((payload_offset, field_ty)) = variant.fields.get(&fp.field).cloned()
                else {
                    bail(f, ctx, "variant field not found in layout");
                    continue;
                };
                emit_test_bind(
                    f,
                    Src::Field {
                        base,
                        offset: 8 + payload_offset,
                    },
                    &field_ty,
                    &fp.pattern,
                    ctx,
                    mut_wb,
                    owning,
                );
            }
        }
        hir::Pattern::Struct { fields, .. } => {
            // Irrefutable: no discriminant test, just bind each named field from
            // its offset. The struct id comes from the value's (monomorphized)
            // type, not the pattern's.
            let sid = match ty {
                hir::Type::Struct(id, _) => *id,
                _ => {
                    bail(f, ctx, "struct pattern on non-struct type");
                    return;
                }
            };
            let base = aggregate_base(f, src, ty, ctx);
            let Some(layout) = ctx.struct_layouts.get(&sid).cloned() else {
                bail(f, ctx, "missing struct layout");
                return;
            };
            for fp in fields {
                let Some((offset, field_ty)) = layout.fields.get(&fp.field).cloned() else {
                    bail(f, ctx, "field not found in struct layout");
                    continue;
                };
                emit_test_bind(
                    f,
                    Src::Field { base, offset },
                    &field_ty,
                    &fp.pattern,
                    ctx,
                    mut_wb,
                    owning,
                );
            }
        }
    }
}

/// Produce a local holding an aggregate value's pointer for repeated access:
/// reuse the local directly when `src` already is one, else stash the loaded
/// pointer in a fresh scratch local. (Mirrored by `walks::collect_match_arm_temps`.)
fn aggregate_base(f: &mut Function, src: Src, ty: &hir::Type, ctx: &EmitCtx) -> u32 {
    match src {
        Src::Local(idx) => idx,
        // A field load or the stack-top value: stash it in a scratch local so the
        // pointer can be reread for each sub-field. (`push_src` is a no-op for
        // `Stack`, so this just `LocalSet`s the value already on the stack.)
        Src::Field { .. } | Src::Stack => {
            let counter = ctx.scratch_counter.get();
            ctx.scratch_counter.set(counter + 1);
            let tmp = ctx.scratch_base + counter;
            push_src(f, src, ty, ctx);
            f.instruction(&Instruction::LocalSet(tmp));
            tmp
        }
    }
}

/// Drop the un-moved-out fields of a consumed match value `base` (a local
/// holding the value's box pointer), reached by `path` — a chain of inline
/// field byte-offsets from `base`. Run after the arm body ran (so a binding
/// taken by the arm drops first): a destructured field recurses, a
/// wildcard/omitted needs-drop field is dropped in place, and — only at the
/// top level (`path` empty) — the scrutinee's own box is freed last. Bound
/// leaves are dropped by drop elaboration, so they are skipped here. Places
/// are re-derived from `base` each time, so no scratch locals are needed.
fn emit_consume_cleanup(
    f: &mut Function,
    base: u32,
    path: &[u32],
    ty: &hir::Type,
    pattern: &hir::Pattern,
    ctx: &EmitCtx,
) {
    // A whole-value binding moved ownership into the binding; its own drop frees
    // the box. Nothing to free here.
    if matches!(pattern, hir::Pattern::Binding { .. }) {
        return;
    }
    match (ty, pattern) {
        (
            hir::Type::Enum(eid, _),
            hir::Pattern::Variant {
                variant_idx,
                fields,
                ..
            },
        ) => {
            if let Some(variant) = ctx
                .enum_layouts
                .get(eid)
                .and_then(|l| l.variants.get(*variant_idx as usize))
            {
                for (name, (payload_offset, field_ty)) in &variant.fields {
                    let sub = fields
                        .iter()
                        .find(|fp| fp.field == *name)
                        .map(|fp| &fp.pattern);
                    cleanup_field(f, base, path, 8 + *payload_offset, field_ty, sub, ctx);
                }
            }
        }
        (hir::Type::Struct(sid, _), hir::Pattern::Struct { fields, .. }) => {
            if let Some(layout) = ctx.struct_layouts.get(sid) {
                for (name, (offset, field_ty)) in &layout.fields {
                    let sub = fields
                        .iter()
                        .find(|fp| fp.field == *name)
                        .map(|fp| &fp.pattern);
                    cleanup_field(f, base, path, *offset, field_ty, sub, ctx);
                }
            }
        }
        (hir::Type::Tuple(ts), hir::Pattern::Tuple { elems, .. }) => {
            let layout = compute_tuple_layout(ts, ctx.policy);
            for (i, (offset, elem_ty)) in layout.elems.iter().enumerate() {
                let sub = elems.get(i);
                cleanup_field(f, base, path, *offset, elem_ty, sub, ctx);
            }
        }
        _ => {}
    }
    // Free the consumed value's own box — but only the outermost one: nested
    // inline fields have no separate allocation.
    if path.is_empty() {
        f.instruction(&Instruction::LocalGet(base));
        f.instruction(&Instruction::Call(ctx.builtins.free));
    }
}

/// Handle one field of a consumed value during match cleanup: a bound leaf is
/// taken (its binding drops it), a destructured field recurses, and a
/// wildcard/omitted field is dropped in place if it needs a destructor.
fn cleanup_field(
    f: &mut Function,
    base: u32,
    path: &[u32],
    offset: u32,
    field_ty: &hir::Type,
    sub: Option<&hir::Pattern>,
    ctx: &EmitCtx,
) {
    match sub {
        // Taken whole: the binding owns it and drops it.
        Some(hir::Pattern::Binding { .. }) => {}
        // Destructured in place: recurse so its own (sub-)fields are handled.
        Some(p) if is_destructure(p) => {
            if ctx.policy.is_inline(field_ty) {
                // Inline field: its bytes live at `base + path + offset`.
                let mut child = path.to_vec();
                child.push(offset);
                emit_consume_cleanup(f, base, &child, field_ty, p, ctx);
            } else {
                // Boxed (recursive) field: the slot holds a 4-byte box pointer.
                // Load it as the new base and recurse with an empty path, so the
                // nested box's own fields are cleaned and its box freed.
                let new_base = aggregate_base(f, Src::Field { base, offset }, field_ty, ctx);
                emit_consume_cleanup(f, new_base, &[], field_ty, p, ctx);
            }
        }
        // Wildcard or omitted: drop the untaken value in place.
        _ => drop_value_at_path(f, base, path, offset, field_ty, ctx),
    }
}

/// Drop an untaken field: run its synthesized drop on the place at
/// `base` + `path` + `offset`. An inline field is dropped in place there; a
/// boxed (recursive) field's slot holds a box pointer, so it is loaded, the
/// pointed-to box dropped, the pointer reloaded, and the box freed (mirroring
/// `emit_drop_in_place`). A no-op for scalars (no destructor).
fn drop_value_at_path(
    f: &mut Function,
    base: u32,
    path: &[u32],
    offset: u32,
    ty: &hir::Type,
    ctx: &EmitCtx,
) {
    // A trait object's slot holds the fat pointer; load it and run the trait's
    // destructor (frees the boxed value and the fat pointer).
    if let hir::Type::Trait(tid) = ty {
        if let Some(&drop_fn) = ctx.drop_trait_fns.get(tid) {
            let mut child = path.to_vec();
            child.push(offset);
            push_place_path(f, base, &child);
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            f.instruction(&Instruction::Call(drop_fn));
        }
        return;
    }
    let Some(&didx) = ctx.drop_fns.get(ty) else {
        return;
    };
    let mut child = path.to_vec();
    child.push(offset);
    if ctx.policy.is_inline(ty) {
        push_place_path(f, base, &child);
        f.instruction(&Instruction::Call(didx));
    } else {
        // Boxed recursive aggregate: load the box pointer, drop its box, reload
        // the pointer, and free the box (the drop call consumed the pointer).
        push_place_path(f, base, &child);
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        f.instruction(&Instruction::Call(didx));
        push_place_path(f, base, &child);
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        f.instruction(&Instruction::Call(ctx.builtins.free));
    }
}

/// Push the address reached from local `base` by adding `path` (a chain of
/// inline field byte-offsets). An empty `path` pushes `base` itself.
fn push_place_path(f: &mut Function, base: u32, path: &[u32]) {
    f.instruction(&Instruction::LocalGet(base));
    let mut total: i32 = 0;
    for &offset in path {
        total += offset as i32;
    }
    if total != 0 {
        f.instruction(&Instruction::I32Const(total));
        f.instruction(&Instruction::I32Add);
    }
}

/// Whether a pattern destructures an aggregate in place (vs. binding/ignoring
/// the whole value) — the patterns whose own fields must be cleaned on consume.
pub(crate) fn is_destructure(pattern: &hir::Pattern) -> bool {
    matches!(
        pattern,
        hir::Pattern::Variant { .. } | hir::Pattern::Struct { .. } | hir::Pattern::Tuple { .. }
    )
}

/// Emit RAII drop of owned local `sym` of type `ty`: call the type's
/// synthesized `drop_T` (which runs the value's own `Drop::drop` and recursively
/// drops its fields in place), then free the box. A trait object is dropped by
/// its `drop_trait_Trait` instead (which frees both the boxed value and the fat
/// pointer).
fn emit_drop(f: &mut Function, sym: hir::SymbolId, ty: &hir::Type, ctx: &EmitCtx) {
    let Some(&local) = ctx.locals.get(&sym) else {
        return;
    };
    if let hir::Type::Trait(tid) = ty {
        if let Some(&drop_fn) = ctx.drop_trait_fns.get(tid) {
            f.instruction(&Instruction::LocalGet(local));
            f.instruction(&Instruction::Call(drop_fn));
        }
        return;
    }
    if let Some(&drop_fn) = ctx.drop_fns.get(ty) {
        f.instruction(&Instruction::LocalGet(local));
        f.instruction(&Instruction::Call(drop_fn));
        f.instruction(&Instruction::LocalGet(local));
        f.instruction(&Instruction::Call(ctx.builtins.free));
    }
}

/// Collect every concrete type that needs a synthesized drop function: the
/// types dropped at `Stmt::Drop` sites, transitively closed over the owned
/// fields a drop recurses into. Returned in a deterministic order so the
/// assigned wasm indices are stable.
pub(crate) fn collect_drop_types(
    program: &hir::Program,
    info: &hir::DropInfo,
    policy: &InlinePolicy,
) -> Vec<hir::Type> {
    let mut work: Vec<hir::Type> = Vec::new();
    for func in &program.functions {
        collect_drop_sites(&func.body, program, &mut work);
    }
    // A struct coerced into a trait object has its box owned by the trait
    // object, so its own drop site is elided by the move — but its drop glue
    // still needs the synthesized `drop_S`. Seed every coercible struct (every
    // struct that implements a trait) so its destructor is emitted.
    for &(_, owner) in program.impls.keys() {
        if let hir::MethodOwner::Struct(sid) = owner {
            work.push(hir::Type::Struct(sid, vec![]));
        }
    }
    work.reverse(); // process in source order despite the LIFO worklist
    let mut seen: HashSet<hir::Type> = HashSet::new();
    let mut order: Vec<hir::Type> = Vec::new();
    while let Some(ty) = work.pop() {
        // A trait object's destructor dispatches through its vtable to the
        // concrete type's drop glue (see `emit_drop_trait_fn`), not to a
        // synthesized `drop_Trait`; exclude it from the per-type drop fns.
        if !info.needs_drop(&ty) || matches!(ty, hir::Type::Trait(_)) || !seen.insert(ty.clone()) {
            continue;
        }
        order.push(ty.clone());
        // Enqueue the owned field types this drop will recurse into.
        for (_, fty) in recursable_fields(&ty, program, policy) {
            work.push(fty);
        }
        // An enum's drop branches on the discriminant and drops the active
        // variant's payload, so its payload field types need drop fns too.
        if let hir::Type::Enum(eid, _) = &ty {
            for (_, fty) in enum_payload_fields(*eid, program, policy) {
                work.push(fty);
            }
        }
    }
    order
}

/// Synthesize the body of `drop_T(ptr: i32)`: run `T`'s own `Drop::drop` (if
/// any) while its fields are still valid, then recursively drop each owned
/// field. The place is NOT freed here — the owned value's drop site (and
/// `drop_in_place` for buffer elements) frees it. An inline field is dropped
/// *in place* at `ptr + offset`; a recursive (non-inline) aggregate field is
/// stored as a 4-byte box pointer in the slot, so it is loaded, dropped, and
/// freed (mirroring `emit_drop_in_place`). For an enum, drop only the active
/// variant's payload (chosen by the inline discriminant).
pub(crate) fn emit_drop_fn(
    ty: &hir::Type,
    drop_fns: &HashMap<hir::Type, u32>,
    drop_trait_fns: &HashMap<hir::TraitId, u32>,
    func_map: &HashMap<hir::FuncId, u32>,
    program: &hir::Program,
    info: &hir::DropInfo,
    policy: &InlinePolicy,
    free_idx: u32,
) -> Function {
    let mut f = Function::new(vec![]); // param 0 is the place pointer
    if let Some(drop_method) = info.drop_method(ty)
        && let Some(&widx) = func_map.get(&drop_method)
    {
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::Call(widx));
    }
    match ty {
        // Inline tagged union: `[u32 discr][inline payload]`. Drop only the
        // active variant's needs-drop payload fields, in place.
        hir::Type::Enum(eid, _) => {
            if let Some(e) = program.enums.get(eid.0 as usize) {
                let layout = compute_enum_layout(e, policy);
                for (variant_idx, variant) in e.variants.iter().enumerate() {
                    let Some(vlayout) = layout.variants.get(variant_idx) else {
                        continue;
                    };
                    let droppable: Vec<(u32, hir::Type)> = variant
                        .fields
                        .iter()
                        .filter_map(|field| {
                            let (off, fty) = vlayout.fields.get(&field.name)?.clone();
                            drop_fns.get(&fty).map(|_| (8 + off, fty))
                        })
                        .collect();
                    if droppable.is_empty() {
                        continue;
                    }
                    f.instruction(&Instruction::Block(BlockType::Empty));
                    // `if discr != variant_idx: skip this variant's payload`.
                    f.instruction(&Instruction::LocalGet(0));
                    f.instruction(&Instruction::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));
                    f.instruction(&Instruction::I32Const(variant_idx as i32));
                    f.instruction(&Instruction::I32Ne);
                    f.instruction(&Instruction::BrIf(0));
                    for (offset, fty) in droppable {
                        emit_drop_field(
                            &mut f,
                            offset,
                            &fty,
                            drop_fns,
                            drop_trait_fns,
                            free_idx,
                            policy,
                        );
                    }
                    f.instruction(&Instruction::End);
                }
            }
        }
        // Struct/tuple/array: a static inline field list, each dropped in place.
        _ => {
            for (offset, fty) in recursable_fields(ty, program, policy) {
                emit_drop_field(
                    &mut f,
                    offset,
                    &fty,
                    drop_fns,
                    drop_trait_fns,
                    free_idx,
                    policy,
                );
            }
        }
    }
    f.instruction(&Instruction::End);
    f
}

/// Synthesize `drop_glue_S(data_ptr)`: the box destructor for a concrete
/// struct `S` that may be coerced into a trait object. It runs `S`'s own
/// `drop_S` (in place) if `S` needs dropping, then frees the box. This is the
/// uniform `(i32) -> ()` entry stored in every vtable's drop slot, so
/// `drop_trait_Trait` can tear down an erased value without knowing `S`.
pub(crate) fn emit_drop_glue_fn(
    sid: hir::StructId,
    drop_fns: &HashMap<hir::Type, u32>,
    info: &hir::DropInfo,
    free_idx: u32,
) -> Function {
    let mut f = Function::new(vec![]); // param 0 is the data pointer
    let ty = hir::Type::Struct(sid, vec![]);
    if info.needs_drop(&ty) {
        if let Some(&didx) = drop_fns.get(&ty) {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::Call(didx));
        }
    }
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(free_idx));
    f.instruction(&Instruction::End);
    f
}

/// Synthesize `drop_trait_Trait(fat_ptr)`: the destructor for a trait object.
/// It loads the concrete type's drop-glue table slot from the vtable (at a
/// fixed offset after the trait's methods), `call_indirect`s it with
/// `data_ptr`, then frees the 8-byte fat pointer itself.
pub(crate) fn emit_drop_trait_fn(
    tid: hir::TraitId,
    program: &hir::Program,
    drop_fn_type: u32,
    free_idx: u32,
) -> Function {
    let mut f = Function::new(vec![]); // param 0 is the fat pointer
    let n_methods = program
        .traits
        .get(tid.0 as usize)
        .map(|t| t.methods.len())
        .unwrap_or(0) as i32;
    // data_ptr = fat_ptr[+4] (the argument to drop_glue_S).
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    }));
    // drop-glue table slot = vtable[fat_ptr[+0] + n_methods * 4].
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(n_methods * 4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::CallIndirect {
        type_index: drop_fn_type,
        table_index: 0,
    });
    // Free the fat pointer object.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(free_idx));
    f.instruction(&Instruction::End);
    f
}

/// Drop one owned field of `drop_T`'s place (param 0) at `offset`. An inline
/// field's bytes live at `ptr + offset`, so `drop_fty` is called on that
/// address. A recursive (non-inline) aggregate is stored in the slot as a box
/// pointer: load it, run `drop_fty` on the box, reload the pointer, and free
/// the box — the same treatment `emit_drop_in_place` gives a buffer element. A
/// trait-object field's slot holds the fat pointer: load it and run the trait's
/// destructor (frees the boxed value and the fat pointer).
fn emit_drop_field(
    f: &mut Function,
    offset: u32,
    fty: &hir::Type,
    drop_fns: &HashMap<hir::Type, u32>,
    drop_trait_fns: &HashMap<hir::TraitId, u32>,
    free_idx: u32,
    policy: &InlinePolicy,
) {
    if let hir::Type::Trait(tid) = fty {
        if let Some(&drop_fn) = drop_trait_fns.get(tid) {
            f.instruction(&Instruction::LocalGet(0));
            if offset != 0 {
                f.instruction(&Instruction::I32Const(offset as i32));
                f.instruction(&Instruction::I32Add);
            }
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            f.instruction(&Instruction::Call(drop_fn));
        }
        return;
    }
    // Present only for needs-drop field types; scalars/pointers are skipped.
    let Some(&didx) = drop_fns.get(fty) else {
        return;
    };
    if policy.is_inline(fty) {
        f.instruction(&Instruction::LocalGet(0));
        if offset != 0 {
            f.instruction(&Instruction::I32Const(offset as i32));
            f.instruction(&Instruction::I32Add);
        }
        f.instruction(&Instruction::Call(didx));
    } else {
        // Boxed recursive field: load the box pointer out of the slot.
        f.instruction(&Instruction::LocalGet(0));
        if offset != 0 {
            f.instruction(&Instruction::I32Const(offset as i32));
            f.instruction(&Instruction::I32Add);
        }
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        f.instruction(&Instruction::Call(didx));
        // The call consumed the box pointer; reload it to free the box.
        f.instruction(&Instruction::LocalGet(0));
        if offset != 0 {
            f.instruction(&Instruction::I32Const(offset as i32));
            f.instruction(&Instruction::I32Add);
        }
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        f.instruction(&Instruction::Call(free_idx));
    }
}

/// The owned fields a `drop_T` recurses into, as `(byte offset, type)` in
/// declaration order. Structs and tuples have a static field list, and an
/// array recurses into each element (laid out as a homogeneous tuple). Enums
/// are handled separately — `emit_drop_fn` dispatches on the discriminant and
/// drops only the active variant's payload, and `enum_payload_fields` registers
/// the payload types — so they contribute none here.
/// The concrete length of an array length-type. After monomorphization every
/// array length is a `ConstInt`; anything else would be a compiler bug.
fn array_const_len(len: &hir::Type) -> usize {
    match len {
        hir::Type::ConstInt(v) => *v as usize,
        other => panic!("array length should be a ConstInt after monomorphization, got {other}"),
    }
}

fn recursable_fields(
    ty: &hir::Type,
    program: &hir::Program,
    policy: &InlinePolicy,
) -> Vec<(u32, hir::Type)> {
    match ty {
        hir::Type::Struct(sid, _) => match program.structs.get(sid.0 as usize) {
            Some(s) => {
                let layout = compute_struct_layout(s, policy);
                s.fields
                    .iter()
                    .filter_map(|field| {
                        layout
                            .fields
                            .get(&field.name)
                            .map(|(off, fty)| (*off, fty.clone()))
                    })
                    .collect()
            }
            None => Vec::new(),
        },
        hir::Type::Tuple(elems) => compute_tuple_layout(elems, policy).elems,
        // An array drops each element: lay it out as a homogeneous tuple.
        hir::Type::Array(elem, len) => {
            compute_tuple_layout(&vec![(**elem).clone(); array_const_len(len)], policy).elems
        }
        _ => Vec::new(),
    }
}

/// The payload fields an enum's drop must be prepared to run, flattened across
/// every variant as `(8 + payload offset, type)`. The emitted drop branches on
/// the discriminant and drops only the active variant's fields; this list just
/// ensures the payload types get synthesized drop functions.
fn enum_payload_fields(
    eid: hir::EnumId,
    program: &hir::Program,
    policy: &InlinePolicy,
) -> Vec<(u32, hir::Type)> {
    let Some(e) = program.enums.get(eid.0 as usize) else {
        return Vec::new();
    };
    let layout = compute_enum_layout(e, policy);
    let mut out = Vec::new();
    for (variant_idx, variant) in e.variants.iter().enumerate() {
        let Some(vlayout) = layout.variants.get(variant_idx) else {
            continue;
        };
        for field in &variant.fields {
            if let Some((off, fty)) = vlayout.fields.get(&field.name) {
                out.push((8 + off, fty.clone()));
            }
        }
    }
    out
}

/// Walk a body collecting the type of every `Stmt::Drop` it contains.
fn collect_drop_sites(block: &hir::Block, program: &hir::Program, out: &mut Vec<hir::Type>) {
    for stmt in &block.stmts {
        match stmt {
            hir::Stmt::Drop { ty, .. } => out.push(ty.clone()),
            hir::Stmt::Loop { body, .. } | hir::Stmt::While { body, .. } => {
                collect_drop_sites(body, program, out)
            }
            hir::Stmt::Let { value, .. }
            | hir::Stmt::Assign { value, .. }
            | hir::Stmt::Return {
                value: Some(value), ..
            }
            | hir::Stmt::Expr(value) => collect_drop_sites_expr(value, program, out),
            hir::Stmt::DerefAssign { ptr, value, .. } => {
                collect_drop_sites_expr(ptr, program, out);
                collect_drop_sites_expr(value, program, out);
            }
            hir::Stmt::FieldAssign { object, value, .. } => {
                collect_drop_sites_expr(object, program, out);
                collect_drop_sites_expr(value, program, out);
            }
            hir::Stmt::Return { value: None, .. } | hir::Stmt::Break { .. } => {}
        }
    }
}

/// Recurse into the blocks an expression can contain (`if`/`match`/block) to
/// reach nested `Stmt::Drop`s.
fn collect_drop_sites_expr(expr: &hir::Expr, program: &hir::Program, out: &mut Vec<hir::Type>) {
    match &expr.kind {
        hir::ExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            collect_drop_sites(then_branch, program, out);
            if let Some(b) = else_branch {
                collect_drop_sites(b, program, out);
            }
        }
        hir::ExprKind::Block(b) | hir::ExprKind::UnsafeBlock(b) => {
            collect_drop_sites(b, program, out)
        }
        hir::ExprKind::Match {
            scrutinee, arms, ..
        } => {
            let consuming = hir::cfg::match_consumes(&program.copy_types, arms);
            for arm in arms {
                collect_drop_sites_expr(&arm.body, program, out);
                // A consumed match's cleanup drops the un-moved-out fields in
                // place (and recurses into destructured ones); those types need
                // drop functions even though no `Stmt::Drop` reaches them.
                if consuming {
                    collect_consume_cleanup_types(&scrutinee.ty, &arm.pattern, program, out);
                }
            }
        }
        // `drop_in_place[T](p)` drops `T` in place — so `T` needs a `drop_T`
        // even though no `Stmt::Drop` or field recursion reaches it (it lives
        // behind the pointer). Register the pointee type.
        hir::ExprKind::Call { func, args, .. }
            if program.functions[func.0 as usize].runtime == Some(hir::RuntimeAbi::DropInPlace) =>
        {
            if let Some(hir::Type::Pointer { pointee, .. }) = args.first().map(|a| &a.ty) {
                out.push((**pointee).clone());
            }
        }
        _ => {}
    }
}

/// Enqueue the field types `emit_consume_cleanup` will need drop functions for
/// when cleaning up a consumed match arm. Mirrors that function's recursion:
/// a binding takes ownership (nothing here), a destructured field recurses, and
/// a wildcard/omitted field is dropped in place (so its own `drop_T` is needed).
fn collect_consume_cleanup_types(
    ty: &hir::Type,
    pattern: &hir::Pattern,
    program: &hir::Program,
    out: &mut Vec<hir::Type>,
) {
    if matches!(pattern, hir::Pattern::Binding { .. }) {
        return;
    }
    match (ty, pattern) {
        (
            hir::Type::Enum(eid, _),
            hir::Pattern::Variant {
                variant_idx,
                fields,
                ..
            },
        ) => {
            let Some(e) = program.enums.get(eid.0 as usize) else {
                return;
            };
            let Some(variant) = e.variants.get(*variant_idx as usize) else {
                return;
            };
            for field in &variant.fields {
                let sub = fields
                    .iter()
                    .find(|fp| fp.field == field.name)
                    .map(|fp| &fp.pattern);
                collect_cleanup_field_types(&field.ty, sub, program, out);
            }
        }
        (hir::Type::Struct(sid, _), hir::Pattern::Struct { fields, .. }) => {
            let Some(s) = program.structs.get(sid.0 as usize) else {
                return;
            };
            for field in &s.fields {
                let sub = fields
                    .iter()
                    .find(|fp| fp.field == field.name)
                    .map(|fp| &fp.pattern);
                collect_cleanup_field_types(&field.ty, sub, program, out);
            }
        }
        (hir::Type::Tuple(ts), hir::Pattern::Tuple { elems, .. }) => {
            for (i, ety) in ts.iter().enumerate() {
                let sub = elems.get(i);
                collect_cleanup_field_types(ety, sub, program, out);
            }
        }
        _ => {}
    }
}

fn collect_cleanup_field_types(
    field_ty: &hir::Type,
    sub: Option<&hir::Pattern>,
    program: &hir::Program,
    out: &mut Vec<hir::Type>,
) {
    match sub {
        // Taken whole: the binding owns it and drops it — no cleanup drop.
        Some(hir::Pattern::Binding { .. }) => {}
        // Destructured in place: its sub-fields are cleaned instead.
        Some(p) if is_destructure(p) => {
            collect_consume_cleanup_types(field_ty, p, program, out);
        }
        // Wildcard or omitted: dropped in place, so its `drop_T` is needed.
        _ => out.push(field_ty.clone()),
    }
}

/// `drop_in_place[T](p)`: run `T`'s destructor on the value at `p`, without
/// reclaiming the slot (it lives in a `Vec`/`Array` buffer). The element type
/// is the pointee of the `*mut T` argument. There are three cases:
///
/// - An *inline* aggregate's bytes live at `p` itself, so `drop_T` is called
///   on the slot address (and the slot itself is reclaimed with the buffer).
/// - A *self-referential* aggregate has no finite inline size, so the slot
///   holds a 4-byte box pointer: load it, run `drop_T` on the box, then free
///   the box (its destructor runs in place, it does not free itself).
/// - A scalar/pointer element has no destructor: discard the address.
fn emit_drop_in_place(f: &mut Function, ptr: &hir::Expr, ctx: &EmitCtx) -> Result<(), WasmError> {
    let elem_ty = match &ptr.ty {
        hir::Type::Pointer { pointee, .. } => (**pointee).clone(),
        _ => return Err(WasmError::Internal("drop_in_place on a non-pointer".into())),
    };
    emit_expr(f, ptr, ctx)?; // the slot address (*mut T)
    if ctx.policy.is_inline(&elem_ty) {
        // The slot address is the value: drop its contents in place.
        if let Some(&didx) = ctx.drop_fns.get(&elem_ty) {
            f.instruction(&Instruction::Call(didx));
        } else {
            f.instruction(&Instruction::Drop);
        }
    } else if let hir::Type::Trait(tid) = &elem_ty {
        // Trait object: the slot holds the fat pointer. Load it and run the
        // trait's destructor (frees the boxed value and the fat pointer).
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        if let Some(&drop_fn) = ctx.drop_trait_fns.get(tid) {
            f.instruction(&Instruction::Call(drop_fn));
        } else {
            f.instruction(&Instruction::Drop);
        }
    } else if matches!(
        &elem_ty,
        hir::Type::Struct(..) | hir::Type::Tuple(..) | hir::Type::Enum(..) | hir::Type::Array(..)
    ) {
        // Recursive aggregate: the slot holds a box pointer. Load it, drop the
        // pointed-to box, then free the box (drop_T drops in place only).
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        if let Some(&didx) = ctx.drop_fns.get(&elem_ty) {
            f.instruction(&Instruction::Call(didx));
            // The call consumed the box pointer; reload it to free the box.
            emit_expr(f, ptr, ctx)?;
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
        }
        f.instruction(&Instruction::Call(ctx.builtins.free));
    } else {
        // Scalar/pointer element: no destructor and nothing to free.
        f.instruction(&Instruction::Drop);
    }
    Ok(())
}

fn emit_runtime_call(
    f: &mut Function,
    runtime: hir::RuntimeAbi,
    args: &[hir::Expr],
    ctx: &EmitCtx,
) -> Result<(), WasmError> {
    match runtime {
        // write(fd, s: String) — ignore fd for now (always stdout); load
        // s.data + s.len from the String struct, hand to __write_bytes.
        hir::RuntimeAbi::Write => {
            emit_write(f, args, ctx)?;
        }
        // now_nanos() -> u64: clock_time_get(CLOCK_MONOTONIC=1, precision=0,
        // out=CLOCK_SCRATCH); discard errno; load the u64 timestamp.
        hir::RuntimeAbi::ClockNow => {
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I64Const(0));
            f.instruction(&Instruction::I32Const(CLOCK_SCRATCH));
            f.instruction(&Instruction::Call(ctx.builtins.clock));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(CLOCK_SCRATCH));
            f.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        }
        // read_raw(fd, ptr, cap) -> nread: build a one-element iovec at the
        // shared scratch (buf@0, buf_len@4) and call fd_read; the host writes
        // the count to offset 8, which becomes the result.
        hir::RuntimeAbi::Read => {
            let store32 = MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            };
            // iovec.buf = ptr
            f.instruction(&Instruction::I32Const(0));
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Store(store32));
            // iovec.buf_len = cap
            f.instruction(&Instruction::I32Const(4));
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::I32Store(store32));
            // fd_read(fd, iovs=0, iovs_len=1, nread=8); discard errno.
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Const(8));
            f.instruction(&Instruction::Call(ctx.builtins.fd_read));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(8));
            f.instruction(&Instruction::I32Load(store32));
        }
        // path_open(dir_fd, dirflags, path, path_len, oflags, rights_base,
        // rights_inheriting, fdflags, opened_out) -> errno. A direct WASI
        // passthrough: the 9 Prim arguments already match the import's wasm
        // types, so push them and call.
        hir::RuntimeAbi::PathOpen => {
            for arg in args {
                emit_expr(f, arg, ctx)?;
            }
            f.instruction(&Instruction::Call(ctx.builtins.path_open));
        }
        // fd_close(fd) -> errno: direct passthrough.
        hir::RuntimeAbi::Close => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.fd_close));
        }
        // poll(subs, events, nsubs) -> nevents: the scheduler has already laid
        // out the subscription structs; call poll_oneoff and return the count
        // the host writes to POLL_NEVENTS.
        hir::RuntimeAbi::Poll => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::I32Const(POLL_NEVENTS));
            f.instruction(&Instruction::Call(ctx.builtins.poll_oneoff));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(POLL_NEVENTS));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
        }
        // Cooperative yield — suspend with the scheduler's yield tag.
        // Control returns to the scheduler's `on $yield` handler in `_start`,
        // which reschedules immediately (single-task case) or picks another
        // runnable continuation (future, when a queue exists).
        hir::RuntimeAbi::Trap => {
            f.instruction(&Instruction::Unreachable);
        }
        // oom() — the allocator's allocation-failure abort. Write the message
        // to stdout and the abort sentinel to stderr from static bytes, then
        // trap. Must not allocate (the heap is the thing that failed) and must
        // write the same sentinel `panic` does, so `prim run` reports a
        // nonzero exit. Static byte offsets come from `lib.rs`.
        hir::RuntimeAbi::Oom => {
            for (fd, site) in [(1, ctx.oom_msg), (2, ctx.oom_sentinel)] {
                f.instruction(&Instruction::I32Const(fd));
                f.instruction(&Instruction::I32Const(site.ptr as i32));
                f.instruction(&Instruction::I32Const(site.len as i32));
                f.instruction(&Instruction::Call(ctx.builtins.write_bytes));
                f.instruction(&Instruction::Drop);
            }
            f.instruction(&Instruction::Unreachable);
        }
        hir::RuntimeAbi::Yield => {
            f.instruction(&Instruction::Suspend(ctx.builtins.yield_tag));
        }
        // resume(handle) -> bool: delegate to the __rt_resume helper.
        hir::RuntimeAbi::Resume => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::Call(ctx.builtins.rt_resume));
        }
        // task_count() -> usize: number of slots in the task table.
        hir::RuntimeAbi::TaskCount => {
            f.instruction(&Instruction::TableSize(ctx.builtins.cont_table));
        }
        // task_live(handle) -> bool: the slot still holds a continuation.
        hir::RuntimeAbi::TaskLive => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::TableGet(ctx.builtins.cont_table));
            f.instruction(&Instruction::RefIsNull);
            f.instruction(&Instruction::I32Eqz);
        }
        // `spawn` is rewritten to ExprKind::Spawn during lowering, so a call
        // through the runtime ABI never reaches codegen.
        hir::RuntimeAbi::Spawn => {
            unreachable!("spawn is lowered to ExprKind::Spawn in hir_builder");
        }
        hir::RuntimeAbi::DropInPlace => {
            unreachable!("drop_in_place is emitted via emit_drop_in_place in the Call arm");
        }
        // spawn_main(): seed the program's main as a task. Same shape as
        // ExprKind::Spawn but the target is `main`, known to the compiler.
        hir::RuntimeAbi::SpawnMain => {
            f.instruction(&Instruction::RefFunc(ctx.builtins.main_func));
            f.instruction(&Instruction::ContNew(ctx.builtins.cont_type));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::TableGrow(ctx.builtins.cont_table));
        }
        // ---- pointer ops on *mut u8 and *mut u32 ----
        // null_T(): push 0 as an i32 (any pointer type lowers to i32).
        // ptr_add/sub/offset_T: scale n by sizeof(T), then add/sub.
        // ptr_byte_*: skip the scaling.
        // ptr_addr_T: pointer is already an i32, no-op.
        hir::RuntimeAbi::NullMutU8
        | hir::RuntimeAbi::NullMutU32
        | hir::RuntimeAbi::NullMutUsize => {
            f.instruction(&Instruction::I32Const(0));
        }
        hir::RuntimeAbi::PtrAddMutU8 | hir::RuntimeAbi::PtrByteAddMutU8 => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrSubMutU8 | hir::RuntimeAbi::PtrByteSubMutU8 => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrOffsetMutU8 | hir::RuntimeAbi::PtrByteOffsetMutU8 => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrAddMutU32 | hir::RuntimeAbi::PtrAddMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Const(4));
            f.instruction(&Instruction::I32Mul);
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrSubMutU32 | hir::RuntimeAbi::PtrSubMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Const(4));
            f.instruction(&Instruction::I32Mul);
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrOffsetMutU32 | hir::RuntimeAbi::PtrOffsetMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Const(4));
            f.instruction(&Instruction::I32Mul);
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrByteAddMutU32 | hir::RuntimeAbi::PtrByteAddMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrByteSubMutU32 | hir::RuntimeAbi::PtrByteSubMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrByteOffsetMutU32 | hir::RuntimeAbi::PtrByteOffsetMutUsize => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrAddrMutU8
        | hir::RuntimeAbi::PtrAddrMutU32
        | hir::RuntimeAbi::PtrAddrMutUsize => {
            emit_expr(f, &args[0], ctx)?;
        }
        hir::RuntimeAbi::MemoryGrow => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::MemoryGrow(0));
        }
        hir::RuntimeAbi::MemoryCopy => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        }
        hir::RuntimeAbi::MemoryFill => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            emit_expr(f, &args[2], ctx)?;
            f.instruction(&Instruction::MemoryFill(0));
        }
        hir::RuntimeAbi::ClzU32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Clz);
        }
        hir::RuntimeAbi::CtzU32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Ctz);
        }
        hir::RuntimeAbi::PopcntU32 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Popcnt);
        }
        hir::RuntimeAbi::ClzU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64Clz);
        }
        hir::RuntimeAbi::CtzU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64Ctz);
        }
        hir::RuntimeAbi::PopcntU64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64Popcnt);
        }
        // `size_of[T]()` is folded to a constant during monomorphization, so
        // no call to it ever reaches codegen.
        hir::RuntimeAbi::SizeOf => unreachable!("size_of is folded in monomorphization"),
        // Generic `*mut T` primitives. A pointer is an i32 address, so these
        // are type-independent; element scaling happens in Prim via size_of.
        hir::RuntimeAbi::Null => {
            f.instruction(&Instruction::I32Const(0));
        }
        hir::RuntimeAbi::PtrByteAdd | hir::RuntimeAbi::PtrByteOffset | hir::RuntimeAbi::At => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Add);
        }
        hir::RuntimeAbi::PtrByteSub => {
            emit_expr(f, &args[0], ctx)?;
            emit_expr(f, &args[1], ctx)?;
            f.instruction(&Instruction::I32Sub);
        }
        hir::RuntimeAbi::PtrAddr | hir::RuntimeAbi::FromAddr | hir::RuntimeAbi::ArrayPtr => {
            emit_expr(f, &args[0], ctx)?;
        }
        // Float <-> integer conversions (std.convert). Float-to-integer
        // truncates toward zero and saturates (no trap on overflow/NaN);
        // integer-to-float rounds to nearest; f32->f64 is an exact widen.
        hir::RuntimeAbi::F64ToU64Trunc => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64TruncSatF64U);
        }
        hir::RuntimeAbi::U64ToF64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::F64ConvertI64U);
        }
        hir::RuntimeAbi::F32ToF64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::F64PromoteF32);
        }
        // Integer conversions (std.convert). A `u8`/`i32`/… all share the
        // wasm `i32` representation, so most narrowings are a mask or a
        // sign-extend and most widenings are a no-op or an i64 extend.
        hir::RuntimeAbi::ConvNoop => {
            emit_expr(f, &args[0], ctx)?;
        }
        hir::RuntimeAbi::ConvTruncU8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Const(0xFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvTruncU16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Const(0xFFFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvSext8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Extend8S);
        }
        hir::RuntimeAbi::ConvSext16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32Extend16S);
        }
        hir::RuntimeAbi::ConvExtI32S => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32S);
        }
        hir::RuntimeAbi::ConvExtI32U => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I64ExtendI32U);
        }
        hir::RuntimeAbi::ConvWrapI64 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
        }
        hir::RuntimeAbi::ConvWrapTruncU8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Const(0xFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvWrapTruncU16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Const(0xFFFF));
            f.instruction(&Instruction::I32And);
        }
        hir::RuntimeAbi::ConvWrapSext8 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Extend8S);
        }
        hir::RuntimeAbi::ConvWrapSext16 => {
            emit_expr(f, &args[0], ctx)?;
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Extend16S);
        }
    }
    Ok(())
}

fn emit_write(f: &mut Function, args: &[hir::Expr], ctx: &EmitCtx) -> Result<(), WasmError> {
    if args.len() < 3 {
        return Err(WasmError::Internal(
            "write_raw expects three arguments".into(),
        ));
    }

    // write_raw(fd, ptr, len) -> nwritten: evaluate the three args in order
    // and call __write_bytes, which leaves the written-byte count on the stack
    // (the primitive's return value). Byte-oriented — no String/Vec knowledge.
    emit_expr(f, &args[0], ctx)?; // fd:  i32
    emit_expr(f, &args[1], ctx)?; // ptr: *mut u8 (i32)
    emit_expr(f, &args[2], ctx)?; // len: usize (i32)
    f.instruction(&Instruction::Call(ctx.builtins.write_bytes));
    Ok(())
}

fn emit_binary_op(f: &mut Function, op: hir::BinaryOp, operand_ty: &hir::Type) {
    let vt = hir_type_to_valtype(operand_ty);
    let signed = is_signed_int(operand_ty);

    match op {
        hir::BinaryOp::Add => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Add),
            ValType::I64 => f.instruction(&Instruction::I64Add),
            ValType::F32 => f.instruction(&Instruction::F32Add),
            ValType::F64 => f.instruction(&Instruction::F64Add),
            _ => unreachable!("add on unsupported operand type"),
        },
        hir::BinaryOp::Subtract => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Sub),
            ValType::I64 => f.instruction(&Instruction::I64Sub),
            ValType::F32 => f.instruction(&Instruction::F32Sub),
            ValType::F64 => f.instruction(&Instruction::F64Sub),
            _ => unreachable!("subtract on unsupported operand type"),
        },
        hir::BinaryOp::Multiply => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Mul),
            ValType::I64 => f.instruction(&Instruction::I64Mul),
            ValType::F32 => f.instruction(&Instruction::F32Mul),
            ValType::F64 => f.instruction(&Instruction::F64Mul),
            _ => unreachable!("multiply on unsupported operand type"),
        },
        hir::BinaryOp::Divide => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32DivS),
            (ValType::I32, false) => f.instruction(&Instruction::I32DivU),
            (ValType::I64, true) => f.instruction(&Instruction::I64DivS),
            (ValType::I64, false) => f.instruction(&Instruction::I64DivU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Div),
            (ValType::F64, _) => f.instruction(&Instruction::F64Div),
            _ => unreachable!("divide on unsupported operand type"),
        },
        hir::BinaryOp::Modulo => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32RemS),
            (ValType::I32, false) => f.instruction(&Instruction::I32RemU),
            (ValType::I64, true) => f.instruction(&Instruction::I64RemS),
            (ValType::I64, false) => f.instruction(&Instruction::I64RemU),
            _ => unreachable!("modulo on unsupported operand type"),
        },
        hir::BinaryOp::Equals => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Eq),
            ValType::I64 => f.instruction(&Instruction::I64Eq),
            ValType::F32 => f.instruction(&Instruction::F32Eq),
            ValType::F64 => f.instruction(&Instruction::F64Eq),
            _ => unreachable!("equals on unsupported operand type"),
        },
        hir::BinaryOp::NotEquals => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Ne),
            ValType::I64 => f.instruction(&Instruction::I64Ne),
            ValType::F32 => f.instruction(&Instruction::F32Ne),
            ValType::F64 => f.instruction(&Instruction::F64Ne),
            _ => unreachable!("notequals on unsupported operand type"),
        },
        hir::BinaryOp::Greater => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32GtS),
            (ValType::I32, false) => f.instruction(&Instruction::I32GtU),
            (ValType::I64, true) => f.instruction(&Instruction::I64GtS),
            (ValType::I64, false) => f.instruction(&Instruction::I64GtU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Gt),
            (ValType::F64, _) => f.instruction(&Instruction::F64Gt),
            _ => unreachable!("greater on unsupported operand type"),
        },
        hir::BinaryOp::GreaterEquals => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32GeS),
            (ValType::I32, false) => f.instruction(&Instruction::I32GeU),
            (ValType::I64, true) => f.instruction(&Instruction::I64GeS),
            (ValType::I64, false) => f.instruction(&Instruction::I64GeU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Ge),
            (ValType::F64, _) => f.instruction(&Instruction::F64Ge),
            _ => unreachable!("greaterequals on unsupported operand type"),
        },
        hir::BinaryOp::Less => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32LtS),
            (ValType::I32, false) => f.instruction(&Instruction::I32LtU),
            (ValType::I64, true) => f.instruction(&Instruction::I64LtS),
            (ValType::I64, false) => f.instruction(&Instruction::I64LtU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Lt),
            (ValType::F64, _) => f.instruction(&Instruction::F64Lt),
            _ => unreachable!("less on unsupported operand type"),
        },
        hir::BinaryOp::LessEquals => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32LeS),
            (ValType::I32, false) => f.instruction(&Instruction::I32LeU),
            (ValType::I64, true) => f.instruction(&Instruction::I64LeS),
            (ValType::I64, false) => f.instruction(&Instruction::I64LeU),
            (ValType::F32, _) => f.instruction(&Instruction::F32Le),
            (ValType::F64, _) => f.instruction(&Instruction::F64Le),
            _ => unreachable!("lessequals on unsupported operand type"),
        },
        hir::BinaryOp::BitAnd => match vt {
            ValType::I32 => f.instruction(&Instruction::I32And),
            ValType::I64 => f.instruction(&Instruction::I64And),
            _ => unreachable!("bitand on unsupported operand type"),
        },
        hir::BinaryOp::BitOr => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Or),
            ValType::I64 => f.instruction(&Instruction::I64Or),
            _ => unreachable!("bitor on unsupported operand type"),
        },
        hir::BinaryOp::BitXor => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Xor),
            ValType::I64 => f.instruction(&Instruction::I64Xor),
            _ => unreachable!("bitxor on unsupported operand type"),
        },
        hir::BinaryOp::ShiftLeft => match vt {
            ValType::I32 => f.instruction(&Instruction::I32Shl),
            ValType::I64 => f.instruction(&Instruction::I64Shl),
            _ => unreachable!("shiftleft on unsupported operand type"),
        },
        hir::BinaryOp::ShiftRight => match (vt, signed) {
            (ValType::I32, true) => f.instruction(&Instruction::I32ShrS),
            (ValType::I32, false) => f.instruction(&Instruction::I32ShrU),
            (ValType::I64, true) => f.instruction(&Instruction::I64ShrS),
            (ValType::I64, false) => f.instruction(&Instruction::I64ShrU),
            _ => unreachable!("shiftright on unsupported operand type"),
        },
    };
}
