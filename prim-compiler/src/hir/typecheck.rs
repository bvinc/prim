use super::{
    BinaryOp, Block, Expr, ExprKind, FuncId, Function, InternSymbol, PassMode, Program, SpanId,
    Stmt, StructId, SymbolId, Type,
};
use crate::hir::cfg::{CopyCtx, is_copy};
use prim_tok::{FileId, Span};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckError {
    pub file: FileId,
    pub span: Span,
    pub kind: TypeCheckKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckKind {
    UndefinedSymbol(SymbolId),
    UnknownFunction(FuncId),
    UnknownStruct(StructId),
    UnknownField {
        struct_id: StructId,
        field: InternSymbol,
    },
    TypeMismatch {
        expected: String,
        found: String,
    },
    InvalidBinaryOperands {
        op: BinaryOp,
        left: Type,
        right: Type,
    },
    InvalidDereference(Type),
    ArityMismatch {
        func: FuncId,
        expected: usize,
        found: usize,
    },
    BreakOutsideLoop,
    UndeterminedReturn,
    MissingReturnValue,
    UndeterminedParamType,
    UndeterminedFieldType,
    AssignToImmutable(SymbolId),
    /// A `block` parameter used as a value rather than invoked (`b` instead of
    /// `b(...)`). Blocks are second-class; they have no runtime value.
    BlockValueUsed(String),
    /// A block literal passed to a `block(...)` formal with a different number
    /// of parameters.
    BlockArityMismatch {
        expected: usize,
        found: usize,
    },
    /// Assignment (`e = ..`, `e.field = ..`, `*e = ..`) through a `read` block
    /// parameter — a read block grants read-only access to the element.
    AssignToReadBlockParam(String),
    /// A block call (`b(arg)`) whose argument is not a dereference of the
    /// element slot. Blocks operate on container slots reached through a raw
    /// pointer (`*add[T](v.ptr, i)`); a non-deref place has no addressable
    /// slot to mutate and would lower a whole-assign to a bogus store.
    BlockArgNotDeref,
    /// An `inline fn` parameter that takes a non-`Copy` value by `own`. Moving
    /// a value *into* the callee has no place after the body is spliced into
    /// the caller; borrows and `Copy` values are fine.
    InlineFnOwnParam(String),
    /// An `inline fn` (directly or indirectly) calling itself: inlining cannot
    /// bottom out.
    RecursiveInlineFn(String),
    /// An `inline fn` with a return type. v1 splices the callee body directly
    /// into the caller, so a non-local `return value` would escape to the
    /// caller instead of flowing through the call.
    InlineFnReturns(String),
    /// A `spawn(f)` whose target is an `inline fn`. Inlining empties the
    /// callee's body (it is spliced into each caller), so a spawn would start
    /// a task that silently runs no body.
    SpawnInlineFn(String),
    /// A numeric literal whose type couldn't be determined from context.
    /// `float` distinguishes the suggested suffix (`f64` vs `i32`).
    AmbiguousLiteral {
        float: bool,
    },
    /// An integer literal whose value doesn't fit its determined type.
    LiteralOutOfRange {
        value: i64,
        ty: Type,
    },
    /// A `match` whose arms don't cover every value. `witness` is an example
    /// pattern that isn't matched.
    NonExhaustiveMatch {
        witness: String,
    },
    /// A `match` arm that can never be reached (a prior arm already covers
    /// everything it would).
    UnreachablePattern,
    /// A raw-pointer power (`*p` deref, pointer arithmetic, reinterpret, or
    /// allocator primitive) used outside an `unsafe` context.
    UnsafeOpInSafeContext {
        what: String,
    },
    Legacy(String),
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeCheckKind::UndefinedSymbol(sym) => write!(f, "Undefined symbol {:?}", sym),
            TypeCheckKind::UnknownFunction(func) => write!(f, "Unknown function {:?}", func),
            TypeCheckKind::UnknownStruct(id) => write!(f, "Unknown struct {:?}", id),
            TypeCheckKind::UnknownField { struct_id, field } => {
                write!(f, "Unknown field {:?} on struct {:?}", field, struct_id)
            }
            TypeCheckKind::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeCheckKind::InvalidBinaryOperands { op, left, right } => write!(
                f,
                "Invalid binary operands for '{}' (left: {}, right: {})",
                op, left, right
            ),
            TypeCheckKind::InvalidDereference(ty) => {
                write!(f, "Invalid dereference of type {}", ty)
            }
            TypeCheckKind::ArityMismatch {
                func,
                expected,
                found,
            } => write!(
                f,
                "Function {:?} called with {} args, expected {}",
                func, found, expected
            ),
            TypeCheckKind::BreakOutsideLoop => write!(f, "break used outside of loop"),
            TypeCheckKind::UndeterminedReturn => {
                write!(f, "return expression has undetermined type")
            }
            TypeCheckKind::MissingReturnValue => {
                write!(f, "missing return value for declared return type")
            }
            TypeCheckKind::UndeterminedParamType => {
                write!(f, "function parameter has undetermined type")
            }
            TypeCheckKind::UndeterminedFieldType => {
                write!(f, "struct field has undetermined type")
            }
            TypeCheckKind::AssignToImmutable(sym) => {
                write!(f, "assignment to immutable global {:?}", sym)
            }
            TypeCheckKind::BlockValueUsed(name) => write!(
                f,
                "block parameter '{name}' used as a value; invoke it as '{name}(...)'"
            ),
            TypeCheckKind::BlockArityMismatch { expected, found } => write!(
                f,
                "block literal has {found} parameter(s), but the `block(...)` type expects {expected}"
            ),
            TypeCheckKind::BlockArgNotDeref => write!(
                f,
                "block call argument must be a dereference of an element slot \
                 (e.g. `b(*add[T](v.ptr, i))`), not a bare place: blocks mutate \
                 container slots reached through raw pointers"
            ),
            TypeCheckKind::AssignToReadBlockParam(name) => write!(
                f,
                "cannot assign through read-only block parameter '{name}' (declare the block \
                 parameter `block(mut T)` to mutate the element)"
            ),
            TypeCheckKind::InlineFnOwnParam(name) => write!(
                f,
                "inline fn parameter '{name}' takes a non-Copy value by `own`, which is not \
                 supported yet: use a `read`/`mut` borrow or a Copy type"
            ),
            TypeCheckKind::RecursiveInlineFn(name) => write!(
                f,
                "inline fn '{name}' is recursive (directly or indirectly); inline fns must \
                 not recurse"
            ),
            TypeCheckKind::InlineFnReturns(name) => write!(
                f,
                "inline fn '{name}' declares a return type, which is not supported yet: inline \
                 fns must return `()`. Return from the enclosing function non-locally inside a \
                 block instead"
            ),
            TypeCheckKind::SpawnInlineFn(name) => write!(
                f,
                "cannot spawn inline fn '{name}': its body is spliced into callers at compile \
                 time and has no runtime entry point"
            ),
            TypeCheckKind::AmbiguousLiteral { float } => {
                let (kind, suffix) = if *float {
                    ("float", "f64")
                } else {
                    ("integer", "i32")
                };
                write!(
                    f,
                    "ambiguous {kind} literal: its type can't be determined from context; \
                     add a type suffix (e.g. `{suffix}`) or a type annotation"
                )
            }
            TypeCheckKind::LiteralOutOfRange { value, ty } => {
                write!(f, "literal {value} is out of range for type {ty}")
            }
            TypeCheckKind::NonExhaustiveMatch { witness } => {
                write!(f, "non-exhaustive match: pattern `{witness}` not covered")
            }
            TypeCheckKind::UnreachablePattern => {
                write!(
                    f,
                    "unreachable match arm: already covered by an earlier arm"
                )
            }
            TypeCheckKind::UnsafeOpInSafeContext { what } => write!(
                f,
                "unsafe operation outside `unsafe` context: {what} \
                 (wrap it in an `unsafe {{ ... }}` block or mark the enclosing \
                 function `unsafe fn`)"
            ),
            TypeCheckKind::Legacy(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for TypeCheckError {}

pub fn type_check(program: &mut Program) -> Result<(), TypeCheckError> {
    let mut checker = Checker::new(program);
    checker.collect_signatures()?;
    checker.check_unsafe_bodies()?;
    checker.check_functions()?;
    checker.check_inline_recursion()?;
    checker.check_inline_spawn()
}

struct Checker<'a> {
    program: &'a mut Program,
    func_sigs: HashMap<FuncId, (Vec<Type>, Option<Type>)>,
    /// Per-function parameter passing modes, parallel to the signature's
    /// params. Kept separately because `program.functions` is taken out of the
    /// program while bodies are checked, so it can't be indexed during the
    /// `MethodCall`→`Call` rewrite.
    func_modes: HashMap<FuncId, Vec<crate::hir::PassMode>>,
    /// Type parameters of each generic function, indexed by callee
    /// `FuncId`. Non-generic functions are absent — `get()` returning
    /// `None` is the signal that no inference is needed at a call site.
    func_type_params: HashMap<FuncId, Vec<crate::hir::TypeParam>>,
    /// Type parameters of each generic struct. Same convention as
    /// `func_type_params`: absent means non-generic, no inference at a
    /// `StructLit`.
    struct_type_params: HashMap<StructId, Vec<crate::hir::TypeParam>>,
    struct_fields: HashMap<StructId, HashMap<InternSymbol, Type>>,
    enum_type_params: HashMap<crate::hir::EnumId, Vec<crate::hir::TypeParam>>,
    /// `(enum, variant_idx)` → field name → declared type. Used at
    /// `VariantLit` typecheck for the same per-field unify/inference
    /// as `StructLit`, and at pattern lowering to look up binding
    /// types.
    enum_variant_fields: HashMap<(crate::hir::EnumId, u32), HashMap<InternSymbol, Type>>,
    /// Functions whose runtime ABI is `Trap` or `Oom` (they never return); a
    /// call to one diverges, satisfying a function's return obligation.
    trap_funcs: std::collections::HashSet<FuncId>,
    /// The `Copy` marker trait's id, resolved once by name. Used to give
    /// `T: Copy` bounds their special semantics (satisfied by `is_copy`,
    /// not by an impl-table entry — scalars/pointers are `Copy` with no impl).
    copy_trait: Option<crate::hir::TraitId>,
    loop_depth: usize,
    /// Return type of the function currently being checked. Used by
    /// `Stmt::Return` to validate the value type.
    current_ret: Option<Type>,
    /// Type parameters of the function currently being checked. Indexed
    /// by `TypeParamId`. Cleared between functions.
    current_type_params: Vec<crate::hir::TypeParam>,
    /// `read` block parameters currently in scope (during a block literal's
    /// body check). Used to reject assignment through a read-only element.
    read_block_params: HashSet<SymbolId>,
}

struct CheckedField {
    expected: Type,
    actual: Type,
}

#[derive(Clone, Copy)]
enum BoundCheckMode {
    ConcreteOnly,
    AllowForwardedParam,
}

impl<'a> Checker<'a> {
    fn new(program: &'a mut Program) -> Self {
        let copy_trait = program.traits.iter().find_map(|t| {
            program
                .symbols
                .get(t.name.0 as usize)
                .filter(|s| program.interner.resolve(&s.name) == "Copy")
                .map(|_| t.id)
        });
        Self {
            program,
            func_sigs: HashMap::new(),
            func_modes: HashMap::new(),
            func_type_params: HashMap::new(),
            struct_type_params: HashMap::new(),
            struct_fields: HashMap::new(),
            enum_type_params: HashMap::new(),
            enum_variant_fields: HashMap::new(),
            trap_funcs: std::collections::HashSet::new(),
            copy_trait,
            loop_depth: 0,
            current_ret: None,
            current_type_params: Vec::new(),
            read_block_params: HashSet::new(),
        }
    }

    /// Copy-vs-move policy for the function currently being checked: resolves
    /// `T: Copy` bounds through `current_type_params`, so a `Copy`-bounded type
    /// parameter is treated as `Copy` in a generic body.
    fn copy_ctx(&self) -> CopyCtx<'_> {
        CopyCtx::new(
            &self.program.copy_types,
            self.copy_trait,
            &self.current_type_params,
        )
    }

    /// If `symbol` resolves to a module-level global, return its declared
    /// type. Returns `None` otherwise (so identifier lookup falls through
    /// to the unknown-symbol path).
    fn global_type(&self, symbol: SymbolId) -> Option<Type> {
        let info = self.program.symbols.get(symbol.0 as usize)?;
        let gid = match info.kind {
            crate::hir::SymbolKind::Global(gid) => gid,
            _ => return None,
        };
        let global = self.program.globals.get(gid.0 as usize)?;
        Some(global.ty.clone())
    }

    /// If `symbol` is a global, return `(type, mutable)`. Used by
    /// assignment lowering to validate that the target is writable.
    fn global_info(&self, symbol: SymbolId) -> Option<(Type, bool)> {
        let info = self.program.symbols.get(symbol.0 as usize)?;
        let gid = match info.kind {
            crate::hir::SymbolKind::Global(gid) => gid,
            _ => return None,
        };
        let global = self.program.globals.get(gid.0 as usize)?;
        Some((global.ty.clone(), global.mutable))
    }

    /// Render a type for user-facing diagnostics. Struct and trait names
    /// resolve through the interner; primitive types fall through to
    /// `Type::Display`.
    fn type_name(&self, ty: &Type) -> String {
        match ty {
            Type::Struct(sid, _) => self
                .program
                .structs
                .get(sid.0 as usize)
                .and_then(|s| self.program.symbols.get(s.name.0 as usize))
                .map(|sym| self.program.interner.resolve(&sym.name).to_string())
                .unwrap_or_else(|| format!("{}", ty)),
            Type::Trait(tid) => self
                .program
                .traits
                .get(tid.0 as usize)
                .and_then(|t| self.program.symbols.get(t.name.0 as usize))
                .map(|sym| self.program.interner.resolve(&sym.name).to_string())
                .unwrap_or_else(|| format!("{}", ty)),
            Type::Enum(eid, _) => self
                .program
                .enums
                .get(eid.0 as usize)
                .and_then(|e| self.program.symbols.get(e.name.0 as usize))
                .map(|sym| self.program.interner.resolve(&sym.name).to_string())
                .unwrap_or_else(|| format!("{}", ty)),
            _ => format!("{}", ty),
        }
    }

    /// Resolve a `SymbolId` to its source name for a readable diagnostic,
    /// falling back to `?` when the symbol is out of range.
    fn symbol_name(&self, sym: SymbolId) -> String {
        self.program
            .symbols
            .get(sym.0 as usize)
            .map(|s| self.program.interner.resolve(&s.name).to_string())
            .unwrap_or_else(|| "?".to_string())
    }

    /// Walk `formal` and `actual` in lockstep, pinning any `Type::Param(i)`
    /// in `formal` to the corresponding sub-tree in `actual`. Returns
    /// false on a contradiction (T already pinned to a different type).
    /// Non-param positions don't trigger errors here — final consistency
    /// is checked by the surrounding unify step.
    fn infer_pins(
        formal: &Type,
        actual: &Type,
        pins: &mut HashMap<crate::hir::TypeParamId, Type>,
    ) -> bool {
        match (formal, actual) {
            (Type::Param(i), actual) => match pins.get(i) {
                Some(existing) => existing == actual,
                None => {
                    pins.insert(*i, actual.clone());
                    true
                }
            },
            (
                Type::Pointer {
                    mutable: ma,
                    pointee: pa,
                },
                Type::Pointer {
                    mutable: mb,
                    pointee: pb,
                },
            ) if ma == mb => Self::infer_pins(pa, pb, pins),
            (Type::Array(a, la), Type::Array(b, lb)) => {
                let e = Self::infer_pins(a, b, pins);
                let l = Self::infer_pins(la, lb, pins);
                e && l
            }
            (Type::Struct(sa, aa), Type::Struct(sb, ab)) if sa == sb && aa.len() == ab.len() => {
                for (a, b) in aa.iter().zip(ab.iter()) {
                    if !Self::infer_pins(a, b, pins) {
                        return false;
                    }
                }
                true
            }
            (Type::Enum(ea, aa), Type::Enum(eb, ab)) if ea == eb && aa.len() == ab.len() => {
                for (a, b) in aa.iter().zip(ab.iter()) {
                    if !Self::infer_pins(a, b, pins) {
                        return false;
                    }
                }
                true
            }
            (Type::Block(pa), Type::Block(pb)) if pa.len() == pb.len() => {
                for (a, b) in pa.iter().zip(pb.iter()) {
                    if a.mode != b.mode || !Self::infer_pins(&a.ty, &b.ty, pins) {
                        return false;
                    }
                }
                true
            }
            _ => true,
        }
    }

    /// Replace every `Type::Param(i)` in `ty` with `pins[i]`. Leaves
    /// types alone if a pin is missing (downstream unify will surface it).
    fn substitute_params(ty: &Type, pins: &HashMap<crate::hir::TypeParamId, Type>) -> Type {
        match ty {
            Type::Param(i) => pins.get(i).cloned().unwrap_or_else(|| ty.clone()),
            Type::Pointer { mutable, pointee } => Type::Pointer {
                mutable: *mutable,
                pointee: Box::new(Self::substitute_params(pointee, pins)),
            },
            Type::Array(elem, n) => Type::Array(
                Box::new(Self::substitute_params(elem, pins)),
                Box::new(Self::substitute_params(n, pins)),
            ),
            Type::Struct(sid, args) => Type::Struct(
                *sid,
                args.iter()
                    .map(|t| Self::substitute_params(t, pins))
                    .collect(),
            ),
            Type::Enum(eid, args) => Type::Enum(
                *eid,
                args.iter()
                    .map(|t| Self::substitute_params(t, pins))
                    .collect(),
            ),
            Type::Block(params) => Type::Block(
                params
                    .iter()
                    .map(|p| super::BlockParam {
                        mode: p.mode,
                        ty: Self::substitute_params(&p.ty, pins),
                    })
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    /// Position-indexed variant of `substitute_params`, used when the
    /// substitution comes from a `Type::Struct(sid, args)` carrier
    /// rather than a function call's pin map.
    fn substitute_params_with_slice(ty: &Type, args: &[Type]) -> Type {
        match ty {
            Type::Param(i) => args
                .get(i.0 as usize)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Type::Pointer { mutable, pointee } => Type::Pointer {
                mutable: *mutable,
                pointee: Box::new(Self::substitute_params_with_slice(pointee, args)),
            },
            Type::Array(elem, n) => Type::Array(
                Box::new(Self::substitute_params_with_slice(elem, args)),
                Box::new(Self::substitute_params_with_slice(n, args)),
            ),
            Type::Struct(sid, type_args) => Type::Struct(
                *sid,
                type_args
                    .iter()
                    .map(|t| Self::substitute_params_with_slice(t, args))
                    .collect(),
            ),
            Type::Enum(eid, type_args) => Type::Enum(
                *eid,
                type_args
                    .iter()
                    .map(|t| Self::substitute_params_with_slice(t, args))
                    .collect(),
            ),
            Type::Block(params) => Type::Block(
                params
                    .iter()
                    .map(|p| super::BlockParam {
                        mode: p.mode,
                        ty: Self::substitute_params_with_slice(&p.ty, args),
                    })
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    fn type_param_name(&self, param: &crate::hir::TypeParam) -> String {
        self.program
            .interner
            .resolve(&self.program.symbols[param.name.0 as usize].name)
            .to_string()
    }

    fn check_type_arg_bound(
        &self,
        ty: &Type,
        bound: crate::hir::TraitId,
        mode: BoundCheckMode,
        span: SpanId,
    ) -> Result<(), TypeCheckError> {
        // `Copy` is a marker trait with special satisfaction rules: a type is
        // `Copy` iff `is_copy` says so (scalars/pointers unconditionally, and
        // structs with an explicit `impl Copy`). It is not backed by an
        // impl-table entry (there is no `impl Copy for u8`), so it's handled
        // before the generic impl-table lookup below.
        if Some(bound) == self.copy_trait {
            // A forwarded type parameter carries the same bound: allowed.
            if let Type::Param(id) = ty {
                if matches!(mode, BoundCheckMode::AllowForwardedParam) {
                    let caller_bound = self
                        .current_type_params
                        .get(id.0 as usize)
                        .and_then(|p| p.bound);
                    if caller_bound == Some(bound) {
                        return Ok(());
                    }
                    // A param forwarded without the bound: name the missing
                    // bound rather than claiming the param isn't `Copy`.
                    return Err(self.error(
                        span,
                        TypeCheckKind::Legacy(format!(
                            "forwarded type parameter must carry bound {}",
                            self.type_name(&Type::Trait(bound))
                        )),
                    ));
                }
            }
            if is_copy(&self.program.copy_types, ty) {
                return Ok(());
            }
            return Err(self.error(
                span,
                TypeCheckKind::Legacy(format!(
                    "type {} does not implement trait {}",
                    self.type_name(ty),
                    self.type_name(&Type::Trait(bound))
                )),
            ));
        }
        match ty {
            _ if crate::hir::MethodOwner::of_type(ty)
                .is_some_and(|owner| self.program.impls.contains_key(&(bound, owner))) =>
            {
                Ok(())
            }
            Type::Param(id) if matches!(mode, BoundCheckMode::AllowForwardedParam) => {
                let caller_bound = self
                    .current_type_params
                    .get(id.0 as usize)
                    .and_then(|p| p.bound);
                if caller_bound == Some(bound) {
                    return Ok(());
                }
                let bound_name = self.type_name(&Type::Trait(bound));
                Err(self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "forwarded type parameter must carry bound {}",
                        bound_name
                    )),
                ))
            }
            other => {
                let type_name = self.type_name(other);
                let bound_name = self.type_name(&Type::Trait(bound));
                Err(self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "type {} does not implement trait {}",
                        type_name, bound_name
                    )),
                ))
            }
        }
    }

    fn resolve_type_args(
        &self,
        type_params: &[crate::hir::TypeParam],
        pins: &HashMap<crate::hir::TypeParamId, Type>,
        span: SpanId,
        mode: BoundCheckMode,
        cannot_infer_prefix: &str,
    ) -> Result<Vec<Type>, TypeCheckError> {
        let mut resolved = Vec::with_capacity(type_params.len());
        for (i, param) in type_params.iter().enumerate() {
            let id = crate::hir::TypeParamId(i as u32);
            let pinned = pins.get(&id).cloned().ok_or_else(|| {
                self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "{}'{}'",
                        cannot_infer_prefix,
                        self.type_param_name(param)
                    )),
                )
            })?;
            // A type parameter inferred solely from an untyped numeric literal
            // (e.g. `println(42)`) has no determined type. Skip the bound check
            // here — the bound would misleadingly report the placeholder type
            // as lacking the impl. `finalize` reports the underlying literal
            // with a precise span instead.
            if !matches!(pinned, Type::IntVar | Type::FloatVar)
                && let Some(bound) = param.bound
            {
                self.check_type_arg_bound(&pinned, bound, mode, span)?;
            }
            resolved.push(pinned);
        }
        Ok(resolved)
    }

    fn pin_type_args(pins: &mut HashMap<crate::hir::TypeParamId, Type>, type_args: &[Type]) {
        for (i, arg) in type_args.iter().enumerate() {
            pins.insert(crate::hir::TypeParamId(i as u32), arg.clone());
        }
    }

    /// Type-check a call to a generic callee: infer one type argument per type
    /// parameter from the value arguments (seeded by any turbofish `type_args`),
    /// write the resolved list back into `type_args`, unify each substituted
    /// formal against its actual, and return the substituted return type. Shared
    /// by free `Call`s and by `MethodCall` rewrites (where the receiver is
    /// `args[0]`, so the method's `self: Option[T]` pins `T` from the receiver).
    fn check_generic_call(
        &mut self,
        tparams: &[crate::hir::TypeParam],
        params: &[Type],
        ret: &Option<Type>,
        args: &mut [Expr],
        type_args: &mut Vec<Type>,
        span: SpanId,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<Type, TypeCheckError> {
        let mut pins: HashMap<crate::hir::TypeParamId, Type> = HashMap::new();
        Self::pin_type_args(&mut pins, type_args);
        let mut arg_types: Vec<Type> = Vec::with_capacity(args.len());
        for (arg, formal) in args.iter_mut().zip(params.iter()) {
            let expected = Self::substitute_params(formal, &pins);
            self.apply_expected(arg, &expected);
            let got = self.check_expr(arg, locals)?;
            if !Self::infer_pins(formal, &got, &mut pins) {
                return Err(self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "conflicting inferences for a type parameter (got {} for a position already pinned to a different type)",
                        self.type_name(&got)
                    )),
                ));
            }
            arg_types.push(got);
        }
        let inferred = self.resolve_type_args(
            tparams,
            &pins,
            span,
            BoundCheckMode::AllowForwardedParam,
            "cannot infer type parameter ",
        )?;
        for (i, (arg, formal)) in args.iter_mut().zip(params.iter()).enumerate() {
            let substituted = Self::substitute_params(formal, &pins);
            let got = &arg_types[i];
            if self.try_coerce_struct_to_trait(arg, got, &substituted) {
                continue;
            }
            if self.unify(&substituted, got).is_none() {
                return Err(self.error(
                    span,
                    TypeCheckKind::TypeMismatch {
                        expected: self.type_name(&substituted),
                        found: self.type_name(got),
                    },
                ));
            }
        }
        let ret_ty = ret
            .as_ref()
            .map(|r| Self::substitute_params(r, &pins))
            .unwrap_or(Type::Unit);
        *type_args = inferred;
        Ok(ret_ty)
    }

    fn infer_field_pins(
        &self,
        fields: &[CheckedField],
        pins: &mut HashMap<crate::hir::TypeParamId, Type>,
        span: SpanId,
        aggregate_name: &str,
    ) -> Result<(), TypeCheckError> {
        for field in fields {
            if !Self::infer_pins(&field.expected, &field.actual, pins) {
                let expected = self.type_name(&field.expected);
                let actual = self.type_name(&field.actual);
                return Err(self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "conflicting inferences for {} type parameter (expected {}, got {})",
                        aggregate_name, expected, actual
                    )),
                ));
            }
        }
        Ok(())
    }

    fn substitute_checked_fields(
        fields: &[CheckedField],
        pins: &HashMap<crate::hir::TypeParamId, Type>,
    ) -> Vec<Type> {
        fields
            .iter()
            .map(|field| Self::substitute_params(&field.expected, pins))
            .collect()
    }

    /// Type an irrefutable pattern (wildcard, binding, or tuple) against an
    /// expected type. Thin wrapper over [`type_pattern`] used by `let`.
    fn type_irrefutable_pattern(
        &mut self,
        pattern: &mut crate::hir::Pattern,
        expected: &Type,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<(), TypeCheckError> {
        self.type_pattern(pattern, expected, locals, false)?;
        Ok(())
    }

    /// Type a pattern against an `expected` type, recording each binding's type
    /// into `locals` and the pattern nodes, and returning the bound symbols.
    /// When `refutable` is false (a `let` binding), refutable patterns (literal,
    /// variant) are rejected.
    fn type_pattern(
        &mut self,
        pattern: &mut crate::hir::Pattern,
        expected: &Type,
        locals: &mut HashMap<SymbolId, Type>,
        refutable: bool,
    ) -> Result<Vec<SymbolId>, TypeCheckError> {
        match pattern {
            crate::hir::Pattern::Wildcard { ty, .. } => {
                *ty = expected.clone();
                Ok(Vec::new())
            }
            crate::hir::Pattern::Binding { symbol, ty, .. } => {
                *ty = expected.clone();
                locals.insert(*symbol, expected.clone());
                Ok(vec![*symbol])
            }
            crate::hir::Pattern::Int { value, ty, span } => {
                if !refutable {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "refutable pattern in irrefutable position".to_string(),
                        ),
                    ));
                }
                if !self.is_integer(expected) && !matches!(expected, Type::IntVar) {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: self.type_name(expected),
                            found: "an integer literal".to_string(),
                        },
                    ));
                }
                *ty = expected.clone();
                self.check_int_literal_fits(*value, expected, *span)?;
                Ok(Vec::new())
            }
            crate::hir::Pattern::Bool { span, .. } => {
                if !refutable {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "refutable pattern in irrefutable position".to_string(),
                        ),
                    ));
                }
                if !matches!(expected, Type::Bool) {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: self.type_name(expected),
                            found: "bool".to_string(),
                        },
                    ));
                }
                Ok(Vec::new())
            }
            crate::hir::Pattern::Tuple { elems, ty, span } => match expected {
                Type::Tuple(elem_types) if elem_types.len() == elems.len() => {
                    *ty = expected.clone();
                    let mut bindings = Vec::new();
                    let elem_types = elem_types.clone();
                    for (elem, elem_ty) in elems.iter_mut().zip(elem_types.iter()) {
                        bindings.extend(self.type_pattern(elem, elem_ty, locals, refutable)?);
                    }
                    Ok(bindings)
                }
                other => Err(self.error(
                    *span,
                    TypeCheckKind::TypeMismatch {
                        expected: self.type_name(other),
                        found: format!("a {}-tuple pattern", elems.len()),
                    },
                )),
            },
            crate::hir::Pattern::Variant {
                enum_id: pattern_enum,
                variant_idx,
                fields,
                span,
            } => {
                if !refutable {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "refutable pattern in irrefutable position".to_string(),
                        ),
                    ));
                }
                let (enum_id, enum_args) = match expected {
                    Type::Enum(id, args) => (*id, args.clone()),
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected: self.type_name(other),
                                found: "an enum variant pattern".to_string(),
                            },
                        ));
                    }
                };
                if *pattern_enum != enum_id {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "pattern's enum does not match scrutinee's enum".to_string(),
                        ),
                    ));
                }
                let field_map = self
                    .enum_variant_fields
                    .get(&(enum_id, *variant_idx))
                    .cloned()
                    .ok_or_else(|| {
                        self.error(
                            *span,
                            TypeCheckKind::Legacy("variant fields not found".to_string()),
                        )
                    })?;
                // A tuple-variant pattern is positional, so it must bind every
                // element (named struct-variant patterns may bind a subset).
                let variant_def =
                    &self.program.enums[enum_id.0 as usize].variants[*variant_idx as usize];
                if variant_def.is_tuple && fields.len() != variant_def.fields.len() {
                    let expected = variant_def.fields.len();
                    let found = fields.len();
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(format!(
                            "tuple variant pattern expects {expected} element(s), but {found} were given"
                        )),
                    ));
                }
                let span = *span;
                let mut arm_bindings = Vec::new();
                for fp in fields.iter_mut() {
                    let declared = field_map.get(&fp.field).cloned().ok_or_else(|| {
                        self.error(
                            span,
                            TypeCheckKind::Legacy(format!(
                                "no field '{}' on variant",
                                self.program.interner.resolve(&fp.field)
                            )),
                        )
                    })?;
                    let resolved = Self::substitute_params_with_slice(&declared, &enum_args);
                    fp.ty = resolved.clone();
                    arm_bindings.extend(self.type_pattern(
                        &mut fp.pattern,
                        &resolved,
                        locals,
                        refutable,
                    )?);
                }
                Ok(arm_bindings)
            }
            crate::hir::Pattern::Struct {
                struct_id: pattern_struct,
                fields,
                span,
            } => {
                let (struct_id, struct_args) = match expected {
                    Type::Struct(id, args) => (*id, args.clone()),
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected: self.type_name(other),
                                found: "a struct pattern".to_string(),
                            },
                        ));
                    }
                };
                if *pattern_struct != struct_id {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "pattern's struct does not match the value's struct".to_string(),
                        ),
                    ));
                }
                let field_map = self.struct_fields.get(&struct_id).cloned().ok_or_else(|| {
                    self.error(
                        *span,
                        TypeCheckKind::Legacy("struct fields not found".to_string()),
                    )
                })?;
                let span = *span;
                let mut bindings = Vec::new();
                for fp in fields.iter_mut() {
                    let declared = field_map.get(&fp.field).cloned().ok_or_else(|| {
                        self.error(
                            span,
                            TypeCheckKind::Legacy(format!(
                                "no field '{}' on struct",
                                self.program.interner.resolve(&fp.field)
                            )),
                        )
                    })?;
                    let resolved = Self::substitute_params_with_slice(&declared, &struct_args);
                    fp.ty = resolved.clone();
                    bindings.extend(self.type_pattern(
                        &mut fp.pattern,
                        &resolved,
                        locals,
                        refutable,
                    )?);
                }
                Ok(bindings)
            }
        }
    }

    /// If `got` is a struct that implements the trait `expected`, wrap
    /// `arg` in a `Coerce` node and return true. Otherwise return false
    /// without touching `arg`; callers fall through to the regular
    /// unification path.
    fn try_coerce_struct_to_trait(&self, arg: &mut Expr, got: &Type, expected: &Type) -> bool {
        let (Type::Trait(tid), Type::Struct(sid, _)) = (expected, got) else {
            return false;
        };
        if !self
            .program
            .impls
            .contains_key(&(*tid, crate::hir::MethodOwner::Struct(*sid)))
        {
            return false;
        }
        let arg_span = arg.span;
        let old = std::mem::replace(
            arg,
            Expr {
                kind: ExprKind::Error,
                ty: Type::Undetermined,
                span: arg_span,
            },
        );
        *arg = Expr {
            kind: ExprKind::Coerce {
                value: Box::new(old),
                source_struct: *sid,
                target_trait: *tid,
            },
            ty: Type::Trait(*tid),
            span: arg_span,
        };
        true
    }

    fn check_record_literal_fields(
        &mut self,
        aggregate_name: &str,
        declared_fields: &HashMap<InternSymbol, Type>,
        fields: &mut [(InternSymbol, Expr)],
        span: SpanId,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<Vec<CheckedField>, TypeCheckError> {
        let mut seen = HashSet::with_capacity(fields.len());
        let mut checked = Vec::with_capacity(fields.len());

        for (field_sym, value) in fields.iter_mut() {
            if !seen.insert(*field_sym) {
                let field_name = self.program.interner.resolve(field_sym);
                return Err(self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "duplicate field '{}' in {} literal",
                        field_name, aggregate_name
                    )),
                ));
            }

            let expected = declared_fields.get(field_sym).cloned().ok_or_else(|| {
                let field_name = self.program.interner.resolve(field_sym);
                self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "unknown field '{}' in {} literal",
                        field_name, aggregate_name
                    )),
                )
            })?;

            self.apply_expected(value, &expected);
            let actual = self.check_expr(value, locals)?;
            checked.push(CheckedField { expected, actual });
        }

        for field_sym in declared_fields.keys() {
            if !seen.contains(field_sym) {
                let field_name = self.program.interner.resolve(field_sym);
                return Err(self.error(
                    span,
                    TypeCheckKind::Legacy(format!(
                        "missing field '{}' in {} literal",
                        field_name, aggregate_name
                    )),
                ));
            }
        }

        Ok(checked)
    }

    fn collect_signatures(&mut self) -> Result<(), TypeCheckError> {
        for s in &self.program.structs {
            let mut fields = HashMap::new();
            for f in &s.fields {
                if matches!(f.ty, Type::Undetermined) {
                    return Err(self.error(f.span, TypeCheckKind::UndeterminedFieldType));
                }
                fields.insert(f.name, f.ty.clone());
            }
            self.struct_fields.insert(s.id, fields);
            if !s.type_params.is_empty() {
                self.struct_type_params.insert(s.id, s.type_params.clone());
            }
        }

        for e in &self.program.enums {
            if !e.type_params.is_empty() {
                self.enum_type_params.insert(e.id, e.type_params.clone());
            }
            for (idx, variant) in e.variants.iter().enumerate() {
                let mut fields = HashMap::with_capacity(variant.fields.len());
                for f in &variant.fields {
                    if matches!(f.ty, Type::Undetermined) {
                        return Err(self.error(f.span, TypeCheckKind::UndeterminedFieldType));
                    }
                    fields.insert(f.name, f.ty.clone());
                }
                self.enum_variant_fields.insert((e.id, idx as u32), fields);
            }
        }

        for f in &self.program.functions {
            for param in &f.params {
                if matches!(param.ty, Type::Undetermined) {
                    return Err(self.error(param.span, TypeCheckKind::UndeterminedParamType));
                }
            }
            let params = f.params.iter().map(|p| p.ty.clone()).collect();
            self.func_sigs.insert(f.id, (params, f.ret.clone()));
            self.func_modes
                .insert(f.id, f.params.iter().map(|p| p.mode).collect());
            if !f.type_params.is_empty() {
                self.func_type_params.insert(f.id, f.type_params.clone());
            }
            if matches!(
                f.runtime,
                Some(crate::hir::RuntimeAbi::Trap | crate::hir::RuntimeAbi::Oom)
            ) {
                self.trap_funcs.insert(f.id);
            }
        }
        Ok(())
    }

    /// Reject recursion among `inline fn`s: the inlining pass cannot bottom
    /// out if an inline function can reach itself, directly or transitively.
    fn check_inline_recursion(&self) -> Result<(), TypeCheckError> {
        let n = self.program.functions.len();
        let mut edges: Vec<Vec<FuncId>> = vec![Vec::new(); n];
        for f in &self.program.functions {
            if !f.is_inline {
                continue;
            }
            let mut seen = std::collections::HashSet::new();
            collect_callees(&f.body, &mut seen);
            for callee in seen {
                let fid = callee.0 as usize;
                if fid < n && self.program.functions[fid].is_inline {
                    edges[f.id.0 as usize].push(callee);
                }
            }
        }

        // Three-color DFS: a back edge to a gray node is a cycle.
        #[derive(Clone, Copy, PartialEq)]
        enum Color {
            White,
            Gray,
            Black,
        }
        let mut color = vec![Color::White; n];
        let mut cycle: Option<(SpanId, SymbolId)> = None;
        fn visit(
            node: usize,
            edges: &[Vec<FuncId>],
            color: &mut [Color],
            functions: &[Function],
            cycle: &mut Option<(SpanId, SymbolId)>,
        ) {
            color[node] = Color::Gray;
            for next in &edges[node] {
                let m = next.0 as usize;
                match color[m] {
                    Color::Gray => {
                        if cycle.is_none() {
                            let f = &functions[m];
                            *cycle = Some((f.span, f.name));
                        }
                    }
                    Color::White => visit(m, edges, color, functions, cycle),
                    Color::Black => {}
                }
            }
            color[node] = Color::Black;
        }
        for fid in 0..n {
            if self.program.functions[fid].is_inline && color[fid] == Color::White {
                visit(fid, &edges, &mut color, &self.program.functions, &mut cycle);
            }
        }
        match cycle {
            Some((span, sym)) => Err(self.error(
                span,
                TypeCheckKind::RecursiveInlineFn(self.symbol_name(sym)),
            )),
            None => Ok(()),
        }
    }

    /// Reject `spawn(f)` where `f` is an `inline fn`. The inlining pass empties
    /// an inline fn's body (it is spliced into each caller), so a spawn
    /// reference would otherwise survive as a task that silently runs no body.
    /// Runs after `check_functions` so `program.functions` is back in place.
    fn check_inline_spawn(&self) -> Result<(), TypeCheckError> {
        for func in &self.program.functions {
            if let Some((span, target)) = first_inline_spawn(&func.body, &self.program.functions) {
                let name = self.symbol_name(self.program.functions[target.0 as usize].name);
                return Err(self.error(span, TypeCheckKind::SpawnInlineFn(name)));
            }
        }
        Ok(())
    }

    /// Enforce the safe/unsafe boundary: raw-pointer powers (`*p` deref,
    /// pointer arithmetic, reinterpret, allocator, and calls to `unsafe fn`)
    /// are only allowed inside an `unsafe` context — the body of an
    /// `unsafe fn` or an `unsafe { ... }` block. Runs before bodies are
    /// checked so the violation points at the same source the rest of
    /// typecheck would otherwise process.
    fn check_unsafe_bodies(&self) -> Result<(), TypeCheckError> {
        for func in &self.program.functions {
            let in_unsafe = func.unsafe_fn;
            if let Some((span, what)) = unsafe_violation(self.program, &func.body, in_unsafe) {
                return Err(self.error(span, TypeCheckKind::UnsafeOpInSafeContext { what }));
            }
        }
        Ok(())
    }

    fn check_functions(&mut self) -> Result<(), TypeCheckError> {
        let mut funcs = std::mem::take(&mut self.program.functions);
        for func in funcs.iter_mut() {
            self.check_function(func)?;
        }
        self.program.functions = funcs;
        Ok(())
    }

    fn check_function(&mut self, func: &mut Function) -> Result<(), TypeCheckError> {
        let mut locals = HashMap::new();
        self.loop_depth = 0;
        self.current_ret = func.ret.clone();
        self.current_type_params = func.type_params.clone();

        // `inline fn` parameters: `own` non-Copy values have no place after the
        // body is spliced into the caller (and would need move/drop bookkeeping
        // across the splice), so reject them. Borrows and `Copy` values are the
        // supported v1 shapes. Inline fns must also return `()` (v1).
        if func.is_inline {
            if func.ret.is_some() {
                return Err(self.error(
                    func.span,
                    TypeCheckKind::InlineFnReturns(self.symbol_name(func.name)),
                ));
            }
            let copy_ctx = self.copy_ctx();
            for p in &func.params {
                if p.mode == PassMode::Own && !copy_ctx.is_copy(&p.ty) {
                    return Err(self.error(
                        p.span,
                        TypeCheckKind::InlineFnOwnParam(self.symbol_name(p.name)),
                    ));
                }
            }
        }

        for p in &func.params {
            locals.insert(p.name, p.ty.clone());
        }

        for stmt in &mut func.body.stmts {
            self.check_stmt(stmt, &mut locals)?;
        }

        if let Some(ret_ty) = &func.ret {
            if let Some(expr) = &mut func.body.expr {
                self.apply_expected(expr, ret_ty);
                let expr_ty = self.check_expr(expr, &mut locals)?;
                if self.try_coerce_struct_to_trait(expr, &expr_ty, ret_ty) {
                    // Coerced struct → trait; nothing more to do.
                } else {
                    match self.unify(&expr_ty, ret_ty) {
                        Some(unified) => {
                            self.apply_expected(expr, &unified);
                        }
                        None => {
                            let expected_name = self.type_name(ret_ty);
                            let found_name = self.type_name(&expr_ty);
                            return Err(self.error(
                                expr.span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                }
            } else if func.runtime.is_none() && !self.block_always_returns(&func.body) {
                return Err(self.error(func.span, TypeCheckKind::MissingReturnValue));
            }
        } else if let Some(expr) = &mut func.body.expr {
            self.check_expr(expr, &mut locals)?;
        }

        self.finalize_block(&mut func.body)?;
        Ok(())
    }

    /// Whether every path through `block` ends in a `return` (or otherwise
    /// diverges), so a declared return value is guaranteed. A trailing value
    /// expression also satisfies the obligation. Used so functions can be
    /// written in statement form with explicit `return`.
    fn block_always_returns(&self, block: &Block) -> bool {
        if block.expr.is_some() {
            return true;
        }
        // A statement that always returns makes the rest of the block dead,
        // so the block diverges if any statement does.
        block.stmts.iter().any(|s| self.stmt_always_returns(s))
    }

    fn stmt_always_returns(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return { .. } => true,
            Stmt::Expr(e) => self.expr_always_returns(e),
            _ => false,
        }
    }

    fn expr_always_returns(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::If {
                then_branch,
                else_branch: Some(else_branch),
                ..
            } => self.block_always_returns(then_branch) && self.block_always_returns(else_branch),
            ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
                self.block_always_returns(block)
            }
            ExprKind::Match { arms, .. } => {
                !arms.is_empty() && arms.iter().all(|a| self.expr_always_returns(&a.body))
            }
            // A call to a diverging runtime primitive (`trap`, `oom`) never
            // returns, so it
            // diverges (satisfies any return obligation). This lets a generic
            // `never`-style helper be written as `{ ...; trap() }`.
            ExprKind::Call { func, .. } => self.trap_funcs.contains(func),
            _ => false,
        }
    }

    fn check_stmt(
        &mut self,
        stmt: &mut Stmt,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<(), TypeCheckError> {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                span,
            } => {
                if !matches!(ty, Type::Undetermined) {
                    self.apply_expected(value, ty);
                }
                let val_ty = self.check_expr(value, locals)?;
                if matches!(ty, Type::Undetermined) {
                    *ty = val_ty.clone();
                } else if self.try_coerce_struct_to_trait(value, &val_ty, ty) {
                    // Coerced; `value`'s type is now the trait type.
                } else {
                    match self.unify(ty, &val_ty) {
                        Some(unified) => {
                            *ty = unified.clone();
                            self.apply_expected(value, &unified);
                        }
                        None => {
                            let expected_name = self.type_name(ty);
                            let found_name = self.type_name(&val_ty);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                }
                let bound_ty = ty.clone();
                self.type_irrefutable_pattern(pattern, &bound_ty, locals)?;
                Ok(())
            }
            Stmt::Assign {
                target,
                value,
                span,
            } => {
                if self.read_block_params.contains(target) {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::AssignToReadBlockParam(self.symbol_name(*target)),
                    ));
                }
                let target_ty = if let Some(t) = locals.get(target).cloned() {
                    t
                } else if let Some((t, mutable)) = self.global_info(*target) {
                    if !mutable {
                        return Err(self.error(*span, TypeCheckKind::AssignToImmutable(*target)));
                    }
                    t
                } else {
                    return Err(self.error(*span, TypeCheckKind::UndefinedSymbol(*target)));
                };
                self.apply_expected(value, &target_ty);
                let val_ty = self.check_expr(value, locals)?;
                if self.unify(&target_ty, &val_ty).is_none() {
                    let expected_name = self.type_name(&target_ty);
                    let found_name = self.type_name(&val_ty);
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: expected_name,
                            found: found_name,
                        },
                    ));
                }
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr, locals)?;
                Ok(())
            }
            Stmt::Loop { body, .. } => {
                self.loop_depth += 1;
                self.check_block(body, locals)?;
                self.loop_depth -= 1;
                Ok(())
            }
            Stmt::While {
                condition,
                body,
                span,
            } => {
                let cond_ty = self.check_expr(condition, locals)?;
                if !matches!(cond_ty, Type::Bool) {
                    let found_name = self.type_name(&cond_ty);
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: "bool".to_string(),
                            found: found_name,
                        },
                    ));
                }
                self.loop_depth += 1;
                self.check_block(body, locals)?;
                self.loop_depth -= 1;
                Ok(())
            }
            Stmt::Break { span } => {
                if self.loop_depth == 0 {
                    Err(self.error(*span, TypeCheckKind::BreakOutsideLoop))
                } else {
                    Ok(())
                }
            }
            Stmt::Return { value, span } => {
                let ret_ty = self.current_ret.clone();
                match (value.as_mut(), ret_ty) {
                    (Some(expr), Some(ret_ty)) => {
                        self.apply_expected(expr, &ret_ty);
                        let expr_ty = self.check_expr(expr, locals)?;
                        if self.try_coerce_struct_to_trait(expr, &expr_ty, &ret_ty) {
                            // Coerced struct → trait at return position.
                        } else if self.unify(&ret_ty, &expr_ty).is_none() {
                            let expected_name = self.type_name(&ret_ty);
                            let found_name = self.type_name(&expr_ty);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                    (Some(_), None) => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(
                                "function has no return type but `return` was given a value"
                                    .to_string(),
                            ),
                        ));
                    }
                    (None, Some(_)) => {
                        return Err(self.error(*span, TypeCheckKind::MissingReturnValue));
                    }
                    (None, None) => {}
                }
                Ok(())
            }
            // Inserted only by drop elaboration, which runs after typecheck.
            Stmt::Drop { .. } => Ok(()),
            Stmt::DerefAssign {
                ptr, value, span, ..
            } => {
                // No `read_block_params` gate here: writing through a raw
                // pointer mutates the *pointee*, not the shared-borrowed
                // pointer value. This mirrors the ordinary-parameter rule —
                // `read p: *mut T` permits `*p = v` — so a `read` block
                // element of type `*mut T` must too. For a non-pointer block
                // element, the deref is already rejected below by the
                // "left of `*... =` must be a pointer" check.
                let ptr_ty = self.check_expr(ptr, locals)?;
                let pointee = match &ptr_ty {
                    Type::Pointer { mutable, pointee } => {
                        if !*mutable {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::Legacy(
                                    "cannot write through a *const pointer".to_string(),
                                ),
                            ));
                        }
                        (**pointee).clone()
                    }
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "left of `*... =` must be a pointer, found {}",
                                other
                            )),
                        ));
                    }
                };
                self.apply_expected(value, &pointee);
                let val_ty = self.check_expr(value, locals)?;
                if self.unify(&pointee, &val_ty).is_none() {
                    let expected_name = self.type_name(&pointee);
                    let found_name = self.type_name(&val_ty);
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: expected_name,
                            found: found_name,
                        },
                    ));
                }
                Ok(())
            }
            Stmt::FieldAssign {
                object,
                field,
                value,
                span,
            } => {
                if let Some(root) = field_root_symbol(object)
                    && self.read_block_params.contains(&root)
                {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::AssignToReadBlockParam(self.symbol_name(root)),
                    ));
                }
                let object_ty = self.check_expr(object, locals)?;
                let (struct_id, type_args) = match object_ty {
                    Type::Struct(id, args) => (id, args),
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "left of `.{} =` must be a struct, found {}",
                                self.program.interner.resolve(field),
                                self.type_name(&other)
                            )),
                        ));
                    }
                };
                let field_ty = self
                    .struct_fields
                    .get(&struct_id)
                    .and_then(|fields| fields.get(field))
                    .ok_or_else(|| {
                        self.error(
                            *span,
                            TypeCheckKind::UnknownField {
                                struct_id,
                                field: *field,
                            },
                        )
                    })?;
                let expected = Self::substitute_params_with_slice(field_ty, &type_args);
                self.apply_expected(value, &expected);
                let val_ty = self.check_expr(value, locals)?;
                if self.unify(&expected, &val_ty).is_none() {
                    let expected_name = self.type_name(&expected);
                    let found_name = self.type_name(&val_ty);
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: expected_name,
                            found: found_name,
                        },
                    ));
                }
                Ok(())
            }
        }
    }

    fn check_block(
        &mut self,
        block: &mut Block,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<Option<Type>, TypeCheckError> {
        for stmt in &mut block.stmts {
            self.check_stmt(stmt, locals)?;
        }
        if let Some(expr) = &mut block.expr {
            let ty = self.check_expr(expr, locals)?;
            Ok(Some(ty))
        } else {
            Ok(None)
        }
    }

    fn check_expr(
        &mut self,
        expr: &mut Expr,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<Type, TypeCheckError> {
        let Expr { kind, ty, span } = expr;
        match kind {
            ExprKind::Int(value) => {
                // Once a literal has a determined integer type (from a suffix
                // or pushed-in context), its value must fit that type. An
                // undetermined literal is left for `finalize` to reject.
                self.check_int_literal_fits(*value, ty, *span)?;
                Ok(ty.clone())
            }
            ExprKind::Float(_) => Ok(ty.clone()),
            // A const generic parameter used as a value is a `usize`.
            ExprKind::ConstParam(_) => {
                *ty = Type::Usize;
                Ok(Type::Usize)
            }
            // `spawn(f)` yields the new task's handle.
            ExprKind::Spawn { .. } => {
                *ty = Type::Usize;
                Ok(Type::Usize)
            }
            ExprKind::Bool(_) => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::Bool;
                }
                Ok(ty.clone())
            }
            ExprKind::Str(_) => Ok(ty.clone()),
            ExprKind::Ident(symbol) => {
                if let Some(t) = locals.get(symbol) {
                    if matches!(t, Type::Block(_)) {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::BlockValueUsed(self.symbol_name(*symbol)),
                        ));
                    }
                    *ty = t.clone();
                    Ok(t.clone())
                } else if let Some(global_ty) = self.global_type(*symbol) {
                    *ty = global_ty.clone();
                    Ok(global_ty)
                } else {
                    // Resolved to a non-value symbol (or one with no type) used
                    // in value position — error here rather than leak
                    // `Undetermined` into codegen.
                    Err(self.error(*span, TypeCheckKind::UndefinedSymbol(*symbol)))
                }
            }
            ExprKind::Binary {
                op, left, right, ..
            } => {
                let l = self.check_expr(left, locals)?;
                let r = self.check_expr(right, locals)?;

                match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo => match self.unify_numeric(&l, &r) {
                        Some(unified) => {
                            self.apply_expected(left, &unified);
                            self.apply_expected(right, &unified);
                            *ty = unified.clone();
                            Ok(unified)
                        }
                        None => Err(self.error(
                            *span,
                            TypeCheckKind::InvalidBinaryOperands {
                                op: *op,
                                left: l,
                                right: r,
                            },
                        )),
                    },
                    BinaryOp::Equals | BinaryOp::NotEquals => match self.unify(&l, &r) {
                        Some(unified) if self.is_equality_compatible(&unified) => {
                            self.apply_expected(left, &unified);
                            self.apply_expected(right, &unified);
                            *ty = Type::Bool;
                            Ok(Type::Bool)
                        }
                        _ => Err(self.error(
                            *span,
                            TypeCheckKind::InvalidBinaryOperands {
                                op: *op,
                                left: l,
                                right: r,
                            },
                        )),
                    },
                    BinaryOp::Greater
                    | BinaryOp::GreaterEquals
                    | BinaryOp::Less
                    | BinaryOp::LessEquals => match self.unify_numeric(&l, &r) {
                        Some(unified) => {
                            self.apply_expected(left, &unified);
                            self.apply_expected(right, &unified);
                            *ty = Type::Bool;
                            Ok(Type::Bool)
                        }
                        None => Err(self.error(
                            *span,
                            TypeCheckKind::InvalidBinaryOperands {
                                op: *op,
                                left: l,
                                right: r,
                            },
                        )),
                    },
                    BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::ShiftLeft
                    | BinaryOp::ShiftRight => match self.unify_integer(&l, &r) {
                        Some(unified) => {
                            self.apply_expected(left, &unified);
                            self.apply_expected(right, &unified);
                            *ty = unified.clone();
                            Ok(unified)
                        }
                        None => Err(self.error(
                            *span,
                            TypeCheckKind::InvalidBinaryOperands {
                                op: *op,
                                left: l,
                                right: r,
                            },
                        )),
                    },
                }
            }
            ExprKind::MethodCall {
                receiver,
                method,
                args,
                arg_modes,
            } => {
                // Step 1: typecheck the receiver. Dispatch differs based on
                // whether it's a concrete struct (direct call) or a trait
                // value (dynamic dispatch via vtable).
                let recv_ty = self.check_expr(receiver, locals)?;
                let owner = match &recv_ty {
                    Type::Trait(tid) => {
                        // Dynamic dispatch path. Find the method in the
                        // trait's declaration order to determine the vtable
                        // slot, then rewrite to DynCall.
                        let method_name = *method;
                        let trait_def = self.program.traits.get(tid.0 as usize).cloned();
                        let trait_def = trait_def.ok_or_else(|| {
                            self.error(
                                *span,
                                TypeCheckKind::Legacy(format!("unknown trait {:?}", tid)),
                            )
                        })?;
                        let method_idx = trait_def
                            .method_idx
                            .get(&method_name)
                            .copied()
                            .ok_or_else(|| {
                                let name = self.program.interner.resolve(&method_name).to_string();
                                self.error(
                                    *span,
                                    TypeCheckKind::Legacy(format!(
                                        "no method '{}' on trait {:?}",
                                        name, tid
                                    )),
                                )
                            })?;
                        let ret_ty = trait_def.methods[method_idx as usize]
                            .ret
                            .clone()
                            .unwrap_or(Type::Unit);
                        // Take args/receiver out and rewrite.
                        let receiver_owned = std::mem::replace(
                            receiver.as_mut(),
                            Expr {
                                kind: ExprKind::Error,
                                ty: Type::Undetermined,
                                span: *span,
                            },
                        );
                        let args_owned = std::mem::take(args);
                        let arg_modes_owned = std::mem::take(arg_modes);
                        let trait_id = *tid;
                        *kind = ExprKind::DynCall {
                            receiver: Box::new(receiver_owned),
                            trait_id,
                            method_idx,
                            args: args_owned,
                            arg_modes: arg_modes_owned,
                        };
                        // Typecheck args inside the DynCall against the
                        // trait method signature (skipping the receiver
                        // position).
                        let ExprKind::DynCall { args, .. } = kind else {
                            unreachable!()
                        };
                        let trait_params = &trait_def.methods[method_idx as usize].params;
                        // Trait method sigs include the receiver as the
                        // first parameter; skip it when matching call args.
                        let expected_args = if trait_params.is_empty() {
                            &[][..]
                        } else {
                            &trait_params[1..]
                        };
                        if expected_args.len() != args.len() {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::Legacy(format!(
                                    "method '{}' expects {} args, got {}",
                                    self.program.interner.resolve(&method_name),
                                    expected_args.len(),
                                    args.len()
                                )),
                            ));
                        }
                        for (arg, expected) in args.iter_mut().zip(expected_args.iter()) {
                            self.apply_expected(arg, expected);
                            let got = self.check_expr(arg, locals)?;
                            if self.unify(expected, &got).is_none() {
                                let expected_name = self.type_name(expected);
                                let found_name = self.type_name(&got);
                                return Err(self.error(
                                    *span,
                                    TypeCheckKind::TypeMismatch {
                                        expected: expected_name,
                                        found: found_name,
                                    },
                                ));
                            }
                        }
                        *ty = ret_ty.clone();
                        return Ok(ret_ty);
                    }
                    Type::Param(tp_id) => {
                        // The receiver is a generic type parameter. The
                        // bound (if any) tells us which trait's methods are
                        // callable on it. Resolve the method through that
                        // trait, then emit a `TraitBoundCall` — kept opaque
                        // until monomorphization picks a concrete `T`.
                        let method_name = *method;
                        let tp = self.current_type_params.get(tp_id.0 as usize).cloned();
                        let bound_tid = match tp.and_then(|p| p.bound) {
                            Some(b) => b,
                            None => {
                                let name = self.program.interner.resolve(&method_name).to_string();
                                return Err(self.error(
                                    *span,
                                    TypeCheckKind::Legacy(format!(
                                        "cannot call method '{}' on unbounded type parameter",
                                        name
                                    )),
                                ));
                            }
                        };
                        let trait_def = self
                            .program
                            .traits
                            .get(bound_tid.0 as usize)
                            .cloned()
                            .expect("bound trait id out of range");
                        let method_idx = trait_def
                            .method_idx
                            .get(&method_name)
                            .copied()
                            .ok_or_else(|| {
                                let name = self.program.interner.resolve(&method_name).to_string();
                                self.error(
                                    *span,
                                    TypeCheckKind::Legacy(format!(
                                        "no method '{}' on trait {:?}",
                                        name, bound_tid
                                    )),
                                )
                            })? as usize;

                        // Substitute `Self` references (params/return
                        // typed as the bound trait itself) with the type
                        // parameter so the body typechecks against `T`.
                        let subst = |t: &Type| -> Type {
                            if matches!(t, Type::Trait(b) if *b == bound_tid) {
                                Type::Param(*tp_id)
                            } else {
                                t.clone()
                            }
                        };
                        let sig = &trait_def.methods[method_idx];
                        let expected_args: Vec<Type> = if sig.params.is_empty() {
                            Vec::new()
                        } else {
                            sig.params[1..].iter().map(subst).collect()
                        };
                        let ret_ty = sig.ret.as_ref().map(subst).unwrap_or(Type::Unit);

                        // Rewrite in place to `TraitBoundCall` so the
                        // monomorphization pass can find it.
                        let receiver_owned = std::mem::replace(
                            receiver.as_mut(),
                            Expr {
                                kind: ExprKind::Error,
                                ty: Type::Undetermined,
                                span: *span,
                            },
                        );
                        let args_owned = std::mem::take(args);
                        let arg_modes_owned = std::mem::take(arg_modes);
                        let tp_id = *tp_id;
                        *kind = ExprKind::TraitBoundCall {
                            receiver: Box::new(receiver_owned),
                            type_param: tp_id,
                            bound: bound_tid,
                            method: method_name,
                            args: args_owned,
                            arg_modes: arg_modes_owned,
                        };
                        let ExprKind::TraitBoundCall { args, .. } = kind else {
                            unreachable!()
                        };
                        if expected_args.len() != args.len() {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::Legacy(format!(
                                    "method '{}' expects {} args, got {}",
                                    self.program.interner.resolve(&method_name),
                                    expected_args.len(),
                                    args.len()
                                )),
                            ));
                        }
                        for (arg, expected) in args.iter_mut().zip(expected_args.iter()) {
                            self.apply_expected(arg, expected);
                            let got = self.check_expr(arg, locals)?;
                            if self.unify(expected, &got).is_none() {
                                let expected_name = self.type_name(expected);
                                let found_name = self.type_name(&got);
                                return Err(self.error(
                                    *span,
                                    TypeCheckKind::TypeMismatch {
                                        expected: expected_name,
                                        found: found_name,
                                    },
                                ));
                            }
                        }
                        *ty = ret_ty.clone();
                        return Ok(ret_ty);
                    }
                    // Static dispatch on a struct, enum, or primitive.
                    other => match crate::hir::MethodOwner::of_type(other) {
                        Some(owner) => owner,
                        None => {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::Legacy(format!(
                                    "method call receiver must be a struct, enum, primitive, or trait type, got {}",
                                    recv_ty
                                )),
                            ));
                        }
                    },
                };
                let method_name = *method;
                // A method name provided by more than one trait can't be
                // resolved from a concrete receiver — there is no bound to pick
                // the trait. Require disambiguation.
                if self
                    .program
                    .ambiguous_methods
                    .contains(&(owner, method_name))
                {
                    let name = self.program.interner.resolve(&method_name).to_string();
                    let type_name = self.type_name(&recv_ty);
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(format!(
                            "method '{name}' on {type_name} is ambiguous: more than one trait \
                             provides it"
                        )),
                    ));
                }
                // Step 2: look up the impl method. It must take `self` — an
                // associated function can't be called on a value.
                let func = match self
                    .program
                    .impl_methods
                    .get(&(owner, method_name))
                    .copied()
                {
                    Some(impl_fn) if impl_fn.is_method => impl_fn.func,
                    Some(_) => {
                        let name = self.program.interner.resolve(&method_name).to_string();
                        let type_name = self.type_name(&recv_ty);
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "'{}' is an associated function of {}, not a method; call it as {}.{}(...)",
                                name, type_name, type_name, name
                            )),
                        ));
                    }
                    None => {
                        let name = self.program.interner.resolve(&method_name).to_string();
                        let type_name = self.type_name(&recv_ty);
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!("no method '{}' on {}", name, type_name)),
                        ));
                    }
                };
                // Step 3: build the call argument list (receiver + original args).
                let receiver_owned = std::mem::replace(
                    receiver.as_mut(),
                    Expr {
                        kind: ExprKind::Error,
                        ty: Type::Undetermined,
                        span: *span,
                    },
                );
                let mut new_args = Vec::with_capacity(args.len() + 1);
                new_args.push(receiver_owned);
                new_args.append(args);
                // The receiver's passing mode comes from the callee's `self`
                // parameter; prepend it so `arg_modes` stays parallel to the
                // rewritten `args` (index 0 = receiver) for the ownership pass.
                let self_mode = self
                    .func_modes
                    .get(&func)
                    .and_then(|m| m.first())
                    .copied()
                    .unwrap_or_default();
                let mut new_arg_modes = Vec::with_capacity(arg_modes.len() + 1);
                new_arg_modes.push(self_mode);
                new_arg_modes.append(arg_modes);
                // Step 4: rewrite in place to a Call. For a method on a generic
                // owner the callee is generic; its type args are inferred below
                // (from the receiver onward) and written into `type_args`.
                *kind = ExprKind::Call {
                    func,
                    type_args: Vec::new(),
                    args: new_args,
                    arg_modes: new_arg_modes,
                };
                // Step 5: typecheck the rewritten call. The receiver's type
                // is already known so check_expr on it is a no-op.
                let ExprKind::Call {
                    func,
                    type_args,
                    args,
                    ..
                } = kind
                else {
                    unreachable!()
                };
                let (params, ret) = self
                    .func_sigs
                    .get(func)
                    .cloned()
                    .ok_or_else(|| self.error(*span, TypeCheckKind::UnknownFunction(*func)))?;
                if params.len() != args.len() {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::ArityMismatch {
                            func: *func,
                            expected: params.len(),
                            found: args.len(),
                        },
                    ));
                }
                // A method on a generic owner is a generic function: infer its
                // type args from the receiver/args (`self: Option[T]` pins `T`).
                if let Some(tparams) = self.func_type_params.get(func).cloned() {
                    let ret_ty = self.check_generic_call(
                        &tparams, &params, &ret, args, type_args, *span, locals,
                    )?;
                    *ty = ret_ty.clone();
                    return Ok(ret_ty);
                }
                for (arg, expected) in args.iter_mut().zip(params.iter()) {
                    self.apply_expected(arg, expected);
                    let got = self.check_expr(arg, locals)?;
                    if self.try_coerce_struct_to_trait(arg, &got, expected) {
                        continue;
                    }
                    match self.unify(expected, &got) {
                        Some(unified) => self.apply_expected(arg, &unified),
                        None => {
                            let expected_name = self.type_name(expected);
                            let found_name = self.type_name(&got);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                }
                let ret_ty = ret.unwrap_or(Type::Unit);
                *ty = ret_ty.clone();
                Ok(ret_ty)
            }
            ExprKind::Call {
                func,
                type_args,
                args,
                arg_modes: _,
            } => {
                let (params, ret) = self
                    .func_sigs
                    .get(func)
                    .cloned()
                    .ok_or_else(|| self.error(*span, TypeCheckKind::UnknownFunction(*func)))?;
                if params.len() != args.len() {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::ArityMismatch {
                            func: *func,
                            expected: params.len(),
                            found: args.len(),
                        },
                    ));
                }
                let callee_type_params = self.func_type_params.get(func).cloned();
                if let Some(tparams) = callee_type_params {
                    let ret_ty = self.check_generic_call(
                        &tparams, &params, &ret, args, type_args, *span, locals,
                    )?;
                    *ty = ret_ty.clone();
                    return Ok(ret_ty);
                }
                // Non-generic callee — original path.
                for (arg, expected) in args.iter_mut().zip(params.iter()) {
                    self.apply_expected(arg, expected);
                    let got = self.check_expr(arg, locals)?;
                    if self.try_coerce_struct_to_trait(arg, &got, expected) {
                        continue;
                    }
                    match self.unify(expected, &got) {
                        Some(unified) => {
                            self.apply_expected(arg, &unified);
                        }
                        None => {
                            let expected_name = self.type_name(expected);
                            let found_name = self.type_name(&got);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                }
                let ret_ty = ret.clone().unwrap_or(Type::Unit);
                *ty = ret_ty.clone();
                Ok(ret_ty)
            }
            ExprKind::StructLit {
                struct_id,
                type_args,
                fields,
            } => {
                let sid = *struct_id;
                let generic = self.struct_type_params.get(&sid).cloned();
                let declared_fields = self
                    .struct_fields
                    .get(&sid)
                    .cloned()
                    .ok_or_else(|| self.error(*span, TypeCheckKind::UnknownStruct(sid)))?;
                let checked_fields = self.check_record_literal_fields(
                    "struct",
                    &declared_fields,
                    fields,
                    *span,
                    locals,
                )?;

                let (resolved_type_args, expected_after_subst): (Vec<Type>, Vec<Type>) =
                    if let Some(tparams) = generic.as_ref() {
                        // Generic struct: infer T_i from each field's
                        // (formal, actual) pair, build the type-arg list,
                        // and substitute formals before unify.
                        let mut pins: HashMap<crate::hir::TypeParamId, Type> = HashMap::new();
                        self.infer_field_pins(&checked_fields, &mut pins, *span, "struct")?;
                        let inferred = self.resolve_type_args(
                            tparams,
                            &pins,
                            *span,
                            BoundCheckMode::ConcreteOnly,
                            "cannot infer type parameter ",
                        )?;
                        let substituted = Self::substitute_checked_fields(&checked_fields, &pins);
                        (inferred, substituted)
                    } else {
                        (
                            Vec::new(),
                            checked_fields
                                .iter()
                                .map(|field| field.expected.clone())
                                .collect(),
                        )
                    };

                // Second pass: unify the (possibly substituted) expected
                // type against the actual, with the trait-coercion
                // fallback that already lives here.
                for ((_, val), (expected, checked)) in fields
                    .iter_mut()
                    .zip(expected_after_subst.iter().zip(checked_fields.iter()))
                {
                    if self.try_coerce_struct_to_trait(val, &checked.actual, expected) {
                        continue;
                    }
                    match self.unify(expected, &checked.actual) {
                        Some(unified) => self.apply_expected(val, &unified),
                        None => {
                            let expected_name = self.type_name(expected);
                            let found_name = self.type_name(&checked.actual);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                }

                *type_args = resolved_type_args.clone();
                let struct_ty = Type::Struct(sid, resolved_type_args);
                *ty = struct_ty.clone();
                Ok(struct_ty)
            }
            ExprKind::Field { base, field } => {
                let base_ty = self.check_expr(base, locals)?;
                let (struct_id, base_type_args) = match base_ty {
                    Type::Struct(id, args) => (id, args),
                    // A field access on a non-struct value is a type error. It
                    // used to leak `Undetermined` to codegen (a silent trap);
                    // report it here instead.
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "field access on `{}`, which is not a struct",
                                self.type_name(&other)
                            )),
                        ));
                    }
                };
                let fields = self
                    .struct_fields
                    .get(&struct_id)
                    .ok_or_else(|| self.error(*span, TypeCheckKind::UnknownStruct(struct_id)))?;
                let field_ty = fields.get(field).ok_or_else(|| {
                    self.error(
                        *span,
                        TypeCheckKind::UnknownField {
                            struct_id,
                            field: *field,
                        },
                    )
                })?;
                // If the base struct was a generic instantiation, the
                // declared field type may contain `Type::Param`s that
                // get resolved by the carrier's type_args.
                let resolved = Self::substitute_params_with_slice(field_ty, &base_type_args);
                *ty = resolved.clone();
                Ok(resolved)
            }
            ExprKind::Deref(base) => {
                let base_ty = self.check_expr(base, locals)?;
                if let Type::Pointer { pointee, .. } = base_ty {
                    *ty = *pointee.clone();
                    Ok((*pointee).clone())
                } else {
                    Err(self.error(*span, TypeCheckKind::InvalidDereference(base_ty)))
                }
            }
            ExprKind::BitNot(operand) => {
                let operand_ty = self.check_expr(operand, locals)?;
                if matches!(operand_ty, Type::Bool) {
                    // `!` is logical NOT on a bool.
                    *ty = Type::Bool;
                    Ok(Type::Bool)
                } else if self.is_integer(&operand_ty) || matches!(operand_ty, Type::IntVar) {
                    *ty = operand_ty.clone();
                    Ok(operand_ty)
                } else {
                    Err(self.error(
                        *span,
                        TypeCheckKind::InvalidBinaryOperands {
                            op: BinaryOp::BitXor, // re-use error variant; signals integer op
                            left: operand_ty.clone(),
                            right: operand_ty,
                        },
                    ))
                }
            }
            ExprKind::Neg(operand) => {
                // For a literal operand, range-check the *negated* value so a
                // type's most-negative literal (e.g. `-128` for i8) is accepted
                // even though the bare magnitude wouldn't fit. Other operands
                // are checked normally.
                let operand_ty = if let ExprKind::Int(n) = &operand.kind {
                    let t = operand.ty.clone();
                    self.check_int_literal_fits(n.wrapping_neg(), &t, operand.span)?;
                    t
                } else {
                    self.check_expr(operand, locals)?
                };
                if self.is_numeric_or_var(&operand_ty) {
                    *ty = operand_ty.clone();
                    Ok(operand_ty)
                } else {
                    Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(format!(
                            "cannot negate a value of type {}",
                            self.type_name(&operand_ty)
                        )),
                    ))
                }
            }
            ExprKind::ArrayLit(elements) => {
                let mut elem_ty: Option<Type> = None;
                for elem in elements.iter_mut() {
                    if let Some(ref expected) = elem_ty {
                        self.apply_expected(elem, expected);
                    }
                    let t = self.check_expr(elem, locals)?;
                    match &elem_ty {
                        None => elem_ty = Some(t),
                        Some(existing) => match self.unify(existing, &t) {
                            Some(unified) => elem_ty = Some(unified),
                            None => {
                                let expected_name = self.type_name(existing);
                                let found_name = self.type_name(&t);
                                return Err(self.error(
                                    *span,
                                    TypeCheckKind::TypeMismatch {
                                        expected: expected_name,
                                        found: found_name,
                                    },
                                ));
                            }
                        },
                    }
                }
                let arr = Type::Array(
                    Box::new(elem_ty.unwrap_or(Type::Undetermined)),
                    Box::new(Type::ConstInt(elements.len() as u64)),
                );
                *ty = arr.clone();
                Ok(arr)
            }
            ExprKind::TupleLit(elements) => {
                let mut elem_types = Vec::with_capacity(elements.len());
                for elem in elements.iter_mut() {
                    elem_types.push(self.check_expr(elem, locals)?);
                }
                let tuple = Type::Tuple(elem_types);
                *ty = tuple.clone();
                Ok(tuple)
            }
            ExprKind::TupleIndex { base, index } => {
                let base_ty = self.check_expr(base, locals)?;
                match base_ty {
                    Type::Tuple(elems) => {
                        let idx = *index as usize;
                        if idx >= elems.len() {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::Legacy(format!(
                                    "tuple index {} out of range for a {}-element tuple",
                                    index,
                                    elems.len()
                                )),
                            ));
                        }
                        let elem_ty = elems[idx].clone();
                        *ty = elem_ty.clone();
                        Ok(elem_ty)
                    }
                    other => Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(format!(
                            "tuple index `.{}` on a non-tuple type {}",
                            index,
                            self.type_name(&other)
                        )),
                    )),
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(condition, locals)?;
                if !matches!(cond_ty, Type::Bool) {
                    let found_name = self.type_name(&cond_ty);
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: "bool".to_string(),
                            found: found_name,
                        },
                    ));
                }
                let then_ty = self.check_block(then_branch, locals)?;
                let else_ty = if let Some(else_block) = else_branch {
                    self.check_block(else_block, locals)?
                } else {
                    None
                };
                let result_ty = match (then_ty, else_ty) {
                    (Some(t), Some(e)) => match self.unify(&t, &e) {
                        Some(unified) => unified,
                        None => {
                            let expected_name = self.type_name(&t);
                            let found_name = self.type_name(&e);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    },
                    (Some(t), None) => t,
                    (None, Some(e)) => e,
                    (None, None) => Type::Unit,
                };
                *ty = result_ty.clone();
                Ok(result_ty)
            }
            ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
                let block_ty = self.check_block(block, locals)?;
                let result_ty = block_ty.unwrap_or(Type::Unit);
                *ty = result_ty.clone();
                Ok(result_ty)
            }
            ExprKind::BlockLit { params, body } => {
                let expected = match &*ty {
                    Type::Block(block_params) => block_params.clone(),
                    Type::Undetermined => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(
                                "block literal used where no `block(...)` type is expected"
                                    .to_string(),
                            ),
                        ));
                    }
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected: "block(...)".to_string(),
                                found: self.type_name(other),
                            },
                        ));
                    }
                };
                if params.len() != expected.len() {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::BlockArityMismatch {
                            expected: expected.len(),
                            found: params.len(),
                        },
                    ));
                }
                // Bind each parameter to its element type in the block body's
                // scope; record read-only parameters so assignments through them
                // are rejected. `return`/`break` inside the body are checked
                // against the *caller's* ret/loop (textual capture) because we
                // stay in the caller's `check_expr` context.
                for (p, bp) in params.iter().zip(expected.iter()) {
                    locals.insert(p.name, bp.ty.clone());
                    if bp.mode != PassMode::Mut {
                        self.read_block_params.insert(p.name);
                    }
                }
                let body_result = self.check_block(body, locals);
                for p in params.iter() {
                    locals.remove(&p.name);
                    self.read_block_params.remove(&p.name);
                }
                body_result?;
                *ty = Type::Block(expected.clone());
                Ok(Type::Block(expected))
            }
            ExprKind::BlockCall {
                param,
                args,
                arg_modes,
            } => {
                let block_ty = locals
                    .get(param)
                    .cloned()
                    .ok_or_else(|| self.error(*span, TypeCheckKind::UndefinedSymbol(*param)))?;
                let Type::Block(block_params) = block_ty else {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "only a `block` parameter can be invoked as `name(...)`".to_string(),
                        ),
                    ));
                };
                if args.len() != block_params.len() {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::BlockArityMismatch {
                            expected: block_params.len(),
                            found: args.len(),
                        },
                    ));
                }
                if arg_modes.len() != args.len() {
                    arg_modes.resize(args.len(), PassMode::Read);
                }
                for ((arg, mode), bp) in args
                    .iter_mut()
                    .zip(arg_modes.iter())
                    .zip(block_params.iter())
                {
                    self.apply_expected(arg, &bp.ty);
                    let got = self.check_expr(arg, locals)?;
                    if self.unify(&bp.ty, &got).is_none() {
                        let expected_name = self.type_name(&bp.ty);
                        let found_name = self.type_name(&got);
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected: expected_name,
                                found: found_name,
                            },
                        ));
                    }
                    // A block parameter is a borrow; its argument must match the
                    // declared element mode. Ownership re-checks this precisely.
                    if *mode == PassMode::Own {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(
                                "a block element is passed by borrow, not `own`".to_string(),
                            ),
                        ));
                    }
                    // Blocks capture a *slot* reached through a raw pointer
                    // (`*add[T](v.ptr, i)` in Vec.with/with_mut). For a `mut`
                    // element a non-deref argument has no addressable slot, so
                    // `e = v` would lower to a deref-assign through a value
                    // (garbage address for a scalar, or the pointee for a
                    // pointer element). Reject it up front rather than emit
                    // that silently. A `read` element is fine as a bare place:
                    // it is only read by value, never whole-assigned.
                    if bp.mode == PassMode::Mut && !matches!(arg.kind, ExprKind::Deref(_)) {
                        return Err(self.error(*span, TypeCheckKind::BlockArgNotDeref));
                    }
                }
                *ty = Type::Unit;
                Ok(Type::Unit)
            }
            ExprKind::Error => Ok(Type::Undetermined),
            ExprKind::DynCall { .. } => {
                // DynCall nodes are produced by typecheck itself; the type
                // has already been set when they were created. Returning
                // the cached type is correct here.
                Ok(ty.clone())
            }
            ExprKind::Coerce { .. } => Ok(ty.clone()),
            ExprKind::TraitBoundCall { .. } => Ok(ty.clone()),
            ExprKind::VariantLit {
                enum_id,
                variant_idx,
                type_args,
                fields,
            } => {
                let eid = *enum_id;
                let vidx = *variant_idx;
                let generic = self.enum_type_params.get(&eid).cloned();
                let contextual_type_args = match &ty {
                    Type::Enum(id, args) if *id == eid => args.clone(),
                    _ => Vec::new(),
                };
                let declared_fields = self
                    .enum_variant_fields
                    .get(&(eid, vidx))
                    .cloned()
                    .ok_or_else(|| {
                        self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "unknown enum variant {:?}::{}",
                                eid, vidx
                            )),
                        )
                    })?;
                let checked_fields = self.check_record_literal_fields(
                    "enum variant",
                    &declared_fields,
                    fields,
                    *span,
                    locals,
                )?;
                // Inference + bound checks (same shape as the struct
                // literal path).
                let (resolved_type_args, expected_after_subst): (Vec<Type>, Vec<Type>) =
                    if let Some(tparams) = generic.as_ref() {
                        let mut pins: HashMap<crate::hir::TypeParamId, Type> = HashMap::new();
                        Self::pin_type_args(&mut pins, &contextual_type_args);
                        Self::pin_type_args(&mut pins, type_args);
                        self.infer_field_pins(&checked_fields, &mut pins, *span, "enum")?;
                        let inferred = self.resolve_type_args(
                            tparams,
                            &pins,
                            *span,
                            BoundCheckMode::ConcreteOnly,
                            "cannot infer enum type parameter ",
                        )?;
                        let substituted = Self::substitute_checked_fields(&checked_fields, &pins);
                        (inferred, substituted)
                    } else {
                        (
                            Vec::new(),
                            checked_fields
                                .iter()
                                .map(|field| field.expected.clone())
                                .collect(),
                        )
                    };
                for ((_, val), (expected, checked)) in fields
                    .iter_mut()
                    .zip(expected_after_subst.iter().zip(checked_fields.iter()))
                {
                    if self.try_coerce_struct_to_trait(val, &checked.actual, expected) {
                        continue;
                    }
                    if self.unify(expected, &checked.actual).is_none() {
                        let expected_name = self.type_name(expected);
                        let found_name = self.type_name(&checked.actual);
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected: expected_name,
                                found: found_name,
                            },
                        ));
                    }
                }
                *type_args = resolved_type_args.clone();
                let enum_ty = Type::Enum(eid, resolved_type_args);
                *ty = enum_ty.clone();
                Ok(enum_ty)
            }
            ExprKind::Match {
                mode,
                scrutinee,
                arms,
            } => {
                let scrut_ty = self.check_expr(scrutinee, locals)?;
                // The scrutinee must be a type we can match on structurally.
                if !self.is_matchable(&scrut_ty) {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(format!(
                            "cannot match on a value of type {}",
                            self.type_name(&scrut_ty)
                        )),
                    ));
                }
                // A `match mut` scrutinee must be a place (a borrow needs a
                // place to borrow), and the arms' `mut` bindings write back to
                // it. Checked against the actual scrutinee type: `mut` requires
                // a non-`Copy` value the caller owns.
                if *mode == PassMode::Mut && self.copy_ctx().is_copy(&scrut_ty) {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(format!(
                            "`match mut` requires a non-Copy scrutinee (got `{}`); \
                                 borrows are second-class, so there is nothing to \
                                 mutate in a Copy value",
                            self.type_name(&scrut_ty)
                        )),
                    ));
                }
                let mut result_ty: Option<Type> = None;
                for arm in arms.iter_mut() {
                    // A `mut` arm binding is an exclusive borrow of the
                    // scrutinee: its writes copy back into the scrutinee's
                    // place, so it is only legal under an explicit `match mut`
                    // (mutation must stay explicit — a bare/`read` match would
                    // silently write through a read-only access). `own`
                    // bindings are unaffected: they move, and consumption is
                    // inferred per arm.
                    if *mode != PassMode::Mut && pattern_has_mut_binding(&arm.pattern) {
                        return Err(self.error(
                            arm.pattern.span(),
                            TypeCheckKind::Legacy(
                                "a `mut` arm binding writes back into the scrutinee; \
                                 use `match mut` to make the exclusive match explicit"
                                    .to_string(),
                            ),
                        ));
                    }
                    let arm_bindings =
                        self.type_pattern(&mut arm.pattern, &scrut_ty, locals, true)?;
                    let body_ty = self.check_expr(&mut arm.body, locals)?;
                    for binding in arm_bindings {
                        locals.remove(&binding);
                    }
                    result_ty = match result_ty {
                        None => Some(body_ty),
                        Some(prev) => match self.unify(&prev, &body_ty) {
                            Some(u) => Some(u),
                            None => {
                                let pname = self.type_name(&prev);
                                let bname = self.type_name(&body_ty);
                                return Err(self.error(
                                    *span,
                                    TypeCheckKind::TypeMismatch {
                                        expected: pname,
                                        found: bname,
                                    },
                                ));
                            }
                        },
                    };
                }
                // Consumption is always inferred from the arms: an `own`
                // binding of a non-`Copy` payload moves it out, and only then
                // is the scrutinee consumed. `match own e` is optional
                // documentation of that consume — it must not claim a consume
                // the arms don't make (the move checker, drop-elaboration and
                // codegen would otherwise disagree about the same bindings).
                let consumes = self.copy_ctx().match_consumes(arms);
                if *mode == PassMode::Own && !consumes {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "`match own e` requires an arm that moves a payload out \
                             (write `own v` in the pattern); consumption is inferred \
                             from the arms, not forced by the keyword"
                                .to_string(),
                        ),
                    ));
                }
                // A consuming match's scrutinee is owned and freed by the
                // match, so no arm may *borrow* a payload out of it (the
                // borrow would dangle / be dropped as owned). Move with `own`.
                if consumes {
                    for arm in arms.iter() {
                        if let Some(binding) =
                            pattern_first_borrow_binding(self.copy_ctx(), &arm.pattern)
                        {
                            return Err(self.error(
                                binding.1,
                                TypeCheckKind::Legacy(
                                    "cannot borrow a payload out of a consumed scrutinee; \
                                     write `own` to move it out instead"
                                        .to_string(),
                                ),
                            ));
                        }
                    }
                }
                // Reachability + exhaustiveness via the Maranget usefulness
                // algorithm.
                self.check_match_usefulness(arms, &scrut_ty, *span)?;
                let final_ty = result_ty.unwrap_or(Type::Unit);
                *ty = final_ty.clone();
                Ok(final_ty)
            }
        }
    }

    /// Types a `match` can scrutinize structurally: enums, booleans, integers,
    /// and tuples. (Floats are excluded — equality matching on floats is a
    /// footgun.)
    fn is_matchable(&self, t: &Type) -> bool {
        matches!(
            t,
            Type::Enum(..) | Type::Bool | Type::Tuple(_) | Type::Struct(..)
        ) || self.is_integer(t)
    }

    /// Run reachability + exhaustiveness analysis over a `match`'s arms.
    fn check_match_usefulness(
        &self,
        arms: &[crate::hir::MatchArm],
        scrut_ty: &Type,
        span: SpanId,
    ) -> Result<(), TypeCheckError> {
        let patterns: Vec<crate::hir::Pattern> = arms.iter().map(|a| a.pattern.clone()).collect();
        let analysis = crate::hir::usefulness::analyze(&patterns, scrut_ty, self.program);
        if let Some(&idx) = analysis.unreachable.first() {
            return Err(self.error(arms[idx].pattern.span(), TypeCheckKind::UnreachablePattern));
        }
        if let Some(witness) = analysis.missing {
            return Err(self.error(span, TypeCheckKind::NonExhaustiveMatch { witness }));
        }
        Ok(())
    }

    // === Type Inference Helpers ===

    fn unify(&self, a: &Type, b: &Type) -> Option<Type> {
        match (a, b) {
            (Type::Undetermined, other) | (other, Type::Undetermined) => Some(other.clone()),
            (Type::IntVar, Type::IntVar) => Some(Type::IntVar),
            (Type::IntVar, t) | (t, Type::IntVar) if self.is_integer(t) => Some(t.clone()),
            (Type::FloatVar, Type::FloatVar) => Some(Type::FloatVar),
            (Type::FloatVar, t) | (t, Type::FloatVar) if self.is_float(t) => Some(t.clone()),
            (Type::IntVar, Type::FloatVar) | (Type::FloatVar, Type::IntVar) => None,
            (Type::Struct(x, ax), Type::Struct(y, ay)) if x == y && ax == ay => Some(a.clone()),
            (Type::Enum(x, ax), Type::Enum(y, ay)) if x == y && ax == ay => Some(a.clone()),
            (
                Type::Pointer {
                    mutable: ma,
                    pointee: pa,
                },
                Type::Pointer {
                    mutable: mb,
                    pointee: pb,
                },
                // `a` is the expected type. A `*mut T` actual may be used
                // where a `*const T` is expected (dropping mutability is
                // sound and a runtime no-op); the reverse is rejected.
            ) if !ma || *mb => self.unify(pa, pb).map(|p| Type::Pointer {
                mutable: *ma,
                pointee: Box::new(p),
            }),
            (Type::Array(a_elem, na), Type::Array(b_elem, nb)) => {
                match (self.unify(a_elem, b_elem), self.unify(na, nb)) {
                    (Some(e), Some(n)) => Some(Type::Array(Box::new(e), Box::new(n))),
                    _ => None,
                }
            }
            (Type::Tuple(ax), Type::Tuple(bx)) if ax.len() == bx.len() => {
                let mut elems = Vec::with_capacity(ax.len());
                for (ea, eb) in ax.iter().zip(bx.iter()) {
                    elems.push(self.unify(ea, eb)?);
                }
                Some(Type::Tuple(elems))
            }
            (a, b) if a == b => Some(a.clone()),
            _ => None,
        }
    }

    fn unify_numeric(&self, a: &Type, b: &Type) -> Option<Type> {
        let unified = self.unify(a, b)?;
        if self.is_numeric_or_var(&unified) {
            Some(unified)
        } else {
            None
        }
    }

    fn unify_integer(&self, a: &Type, b: &Type) -> Option<Type> {
        let unified = self.unify(a, b)?;
        if self.is_integer(&unified) || matches!(unified, Type::IntVar) {
            Some(unified)
        } else {
            None
        }
    }

    /// Validate that an integer literal's value fits its determined type.
    /// `usize`/`isize` are 32-bit on wasm32, so they share `u32`/`i32`'s
    /// range. Non-integer or still-undetermined types are not checked here.
    fn check_int_literal_fits(
        &self,
        value: i64,
        ty: &Type,
        span: SpanId,
    ) -> Result<(), TypeCheckError> {
        let (min, max): (i128, i128) = match ty {
            Type::I8 => (i8::MIN as i128, i8::MAX as i128),
            Type::U8 => (0, u8::MAX as i128),
            Type::I16 => (i16::MIN as i128, i16::MAX as i128),
            Type::U16 => (0, u16::MAX as i128),
            Type::I32 | Type::Isize => (i32::MIN as i128, i32::MAX as i128),
            Type::U32 | Type::Usize => (0, u32::MAX as i128),
            Type::I64 => (i64::MIN as i128, i64::MAX as i128),
            Type::U64 => (0, u64::MAX as i128),
            _ => return Ok(()),
        };
        let v = value as i128;
        if v < min || v > max {
            return Err(self.error(
                span,
                TypeCheckKind::LiteralOutOfRange {
                    value,
                    ty: ty.clone(),
                },
            ));
        }
        Ok(())
    }

    fn apply_expected(&self, expr: &mut Expr, expected: &Type) {
        if matches!(expected, Type::Undetermined) {
            return;
        }
        let Expr { kind, ty, .. } = expr;
        match kind {
            ExprKind::Int(_) => {
                if matches!(ty, Type::IntVar | Type::Undetermined) && self.is_integer(expected) {
                    *ty = expected.clone();
                }
            }
            ExprKind::Float(_) => {
                if matches!(ty, Type::FloatVar | Type::Undetermined) && self.is_float(expected) {
                    *ty = expected.clone();
                }
            }
            ExprKind::Bool(_) => {
                if matches!(ty, Type::Undetermined) && matches!(expected, Type::Bool) {
                    *ty = Type::Bool;
                }
            }
            ExprKind::Binary { left, right, .. } => {
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    self.apply_expected(left, expected);
                    self.apply_expected(right, expected);
                    if self.is_numeric(expected) {
                        *ty = expected.clone();
                    }
                }
            }
            ExprKind::Neg(operand) => {
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    self.apply_expected(operand, expected);
                    if self.is_numeric(expected) {
                        *ty = expected.clone();
                    }
                }
            }
            ExprKind::VariantLit {
                enum_id, type_args, ..
            } => {
                if let Type::Enum(expected_enum, expected_args) = expected
                    && enum_id == expected_enum
                {
                    *type_args = expected_args.clone();
                    *ty = expected.clone();
                }
            }
            ExprKind::Match { arms, .. } => {
                for arm in arms {
                    self.apply_expected(&mut arm.body, expected);
                }
            }
            ExprKind::TupleLit(elems) => {
                if let Type::Tuple(expected_elems) = expected
                    && expected_elems.len() == elems.len()
                {
                    for (e, et) in elems.iter_mut().zip(expected_elems.iter()) {
                        self.apply_expected(e, et);
                    }
                    *ty = expected.clone();
                }
            }
            // A block literal's type is the `block(...)` formal it is passed
            // to; record it so `check_expr` can bind the parameters.
            ExprKind::BlockLit { .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = expected.clone();
                }
            }
            _ => {}
        }
    }

    /// Resolve any lingering inference variables in a pattern's stored types.
    fn finalize_pattern(pattern: &mut crate::hir::Pattern) {
        match pattern {
            crate::hir::Pattern::Wildcard { ty, .. } => *ty = Self::finalize_type(ty),
            crate::hir::Pattern::Binding { ty, .. } => *ty = Self::finalize_type(ty),
            crate::hir::Pattern::Int { ty, .. } => *ty = Self::finalize_type(ty),
            crate::hir::Pattern::Bool { .. } => {}
            crate::hir::Pattern::Tuple { elems, ty, .. } => {
                *ty = Self::finalize_type(ty);
                for elem in elems {
                    Self::finalize_pattern(elem);
                }
            }
            crate::hir::Pattern::Variant { fields, .. }
            | crate::hir::Pattern::Struct { fields, .. } => {
                for fp in fields {
                    fp.ty = Self::finalize_type(&fp.ty);
                    Self::finalize_pattern(&mut fp.pattern);
                }
            }
        }
    }

    fn finalize_block(&self, block: &mut Block) -> Result<(), TypeCheckError> {
        for stmt in &mut block.stmts {
            self.finalize_stmt(stmt)?;
        }
        if let Some(expr) = &mut block.expr {
            self.finalize_expr(expr)?;
        }
        Ok(())
    }

    fn finalize_stmt(&self, stmt: &mut Stmt) -> Result<(), TypeCheckError> {
        match stmt {
            Stmt::Let {
                pattern, ty, value, ..
            } => {
                self.finalize_expr(value)?;
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    *ty = Self::finalize_type(&value.ty);
                }
                Self::finalize_pattern(pattern);
            }
            Stmt::Assign { value, .. } => {
                self.finalize_expr(value)?;
            }
            Stmt::Expr(e) => self.finalize_expr(e)?,
            Stmt::Loop { body, .. } => {
                self.finalize_block(body)?;
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.finalize_expr(condition)?;
                self.finalize_block(body)?;
            }
            Stmt::Break { .. } => {}
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.finalize_expr(v)?;
                }
            }
            Stmt::DerefAssign { ptr, value, .. } => {
                self.finalize_expr(ptr)?;
                self.finalize_expr(value)?;
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.finalize_expr(object)?;
                self.finalize_expr(value)?;
            }
            // Inserted only by drop elaboration, which runs after typecheck.
            Stmt::Drop { .. } => {}
        }
        Ok(())
    }

    fn finalize_expr(&self, expr: &mut Expr) -> Result<(), TypeCheckError> {
        let Expr { kind, ty, span } = expr;
        match kind {
            // A literal still carrying a placeholder type never received one
            // from context — under explicit-literal-typing that's an error,
            // not a silent default.
            ExprKind::Int(_) => {
                if matches!(ty, Type::IntVar) {
                    return Err(self.error(*span, TypeCheckKind::AmbiguousLiteral { float: false }));
                }
            }
            ExprKind::Float(_) => {
                if matches!(ty, Type::FloatVar) {
                    return Err(self.error(*span, TypeCheckKind::AmbiguousLiteral { float: true }));
                }
            }
            ExprKind::Binary { left, right, .. } => {
                self.finalize_expr(left)?;
                self.finalize_expr(right)?;
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    *ty = Self::finalize_type(&left.ty);
                }
            }
            ExprKind::Call { args, .. } => {
                for arg in args {
                    self.finalize_expr(arg)?;
                }
            }
            ExprKind::BlockLit { body, .. } => {
                self.finalize_block(body)?;
                *ty = Self::finalize_type(ty);
            }
            ExprKind::BlockCall { args, .. } => {
                for arg in args {
                    self.finalize_expr(arg)?;
                }
                *ty = Self::finalize_type(ty);
            }
            // After typecheck, MethodCall is rewritten to Call or DynCall so
            // this arm is only reached if check_expr returned early with an
            // error before the rewrite. Recurse into the children to keep
            // finalize total.
            ExprKind::MethodCall { receiver, args, .. } => {
                self.finalize_expr(receiver)?;
                for arg in args {
                    self.finalize_expr(arg)?;
                }
            }
            ExprKind::DynCall { receiver, args, .. } => {
                self.finalize_expr(receiver)?;
                for arg in args {
                    self.finalize_expr(arg)?;
                }
            }
            ExprKind::TraitBoundCall { receiver, args, .. } => {
                self.finalize_expr(receiver)?;
                for arg in args {
                    self.finalize_expr(arg)?;
                }
            }
            ExprKind::Coerce { value, .. } => {
                self.finalize_expr(value)?;
            }
            ExprKind::TupleLit(elems) => {
                for e in elems {
                    self.finalize_expr(e)?;
                }
            }
            ExprKind::TupleIndex { base, .. } => {
                self.finalize_expr(base)?;
            }
            ExprKind::StructLit { fields, .. } => {
                for (_, val) in fields {
                    self.finalize_expr(val)?;
                }
            }
            ExprKind::VariantLit { fields, .. } => {
                for (_, val) in fields {
                    self.finalize_expr(val)?;
                }
            }
            ExprKind::Match {
                scrutinee, arms, ..
            } => {
                self.finalize_expr(scrutinee)?;
                for arm in arms {
                    Self::finalize_pattern(&mut arm.pattern);
                    self.finalize_expr(&mut arm.body)?;
                }
            }
            ExprKind::Field { base, .. } => {
                self.finalize_expr(base)?;
            }
            ExprKind::Deref(base) => {
                self.finalize_expr(base)?;
            }
            ExprKind::BitNot(operand) => {
                self.finalize_expr(operand)?;
                if matches!(ty, Type::IntVar | Type::Undetermined) {
                    *ty = Self::finalize_type(&operand.ty);
                }
            }
            ExprKind::Neg(operand) => {
                self.finalize_expr(operand)?;
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    *ty = Self::finalize_type(&operand.ty);
                }
            }
            ExprKind::ArrayLit(elements) => {
                for elem in elements.iter_mut() {
                    self.finalize_expr(elem)?;
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.finalize_expr(condition)?;
                self.finalize_block(then_branch)?;
                if let Some(else_block) = else_branch {
                    self.finalize_block(else_block)?;
                }
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined)
                    && let Some(then_expr) = &then_branch.expr
                {
                    *ty = Self::finalize_type(&then_expr.ty);
                }
            }
            ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
                self.finalize_block(block)?;
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined)
                    && let Some(expr) = &block.expr
                {
                    *ty = Self::finalize_type(&expr.ty);
                }
            }
            _ => {}
        }
        // Every expression must now carry a concrete type (`Unit` for no value).
        // A surviving `Undetermined` is a type the checker failed to determine —
        // reject it here rather than let it reach codegen as a silent trap. (An
        // `Error` node is already a reported lowering failure.)
        if matches!(ty, Type::Undetermined) && !matches!(kind, ExprKind::Error) {
            return Err(self.error(
                *span,
                TypeCheckKind::Legacy(
                    "could not determine the type of this expression".to_string(),
                ),
            ));
        }
        Ok(())
    }

    fn finalize_type(ty: &Type) -> Type {
        match ty {
            Type::IntVar => Type::I32,
            Type::FloatVar => Type::F64,
            Type::Block(params) => Type::Block(
                params
                    .iter()
                    .map(|p| super::BlockParam {
                        mode: p.mode,
                        ty: Self::finalize_type(&p.ty),
                    })
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    // === Type Classification Helpers ===

    fn is_integer(&self, t: &Type) -> bool {
        matches!(
            t,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::Isize
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Usize
        )
    }

    fn is_float(&self, t: &Type) -> bool {
        matches!(t, Type::F32 | Type::F64)
    }

    fn is_numeric(&self, t: &Type) -> bool {
        self.is_integer(t) || self.is_float(t)
    }

    fn is_numeric_or_var(&self, t: &Type) -> bool {
        self.is_numeric(t) || matches!(t, Type::IntVar | Type::FloatVar)
    }

    fn is_equality_compatible(&self, t: &Type) -> bool {
        matches!(
            t,
            Type::Bool
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::Isize
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Usize
                | Type::F32
                | Type::F64
                | Type::IntVar
                | Type::FloatVar
                | Type::Pointer { .. }
                | Type::Struct(_, _)
        )
    }

    fn error(&self, span_id: SpanId, kind: TypeCheckKind) -> TypeCheckError {
        let (file, span) = self
            .program
            .spans
            .get(span_id.0 as usize)
            .copied()
            .expect("missing span");
        TypeCheckError { file, span, kind }
    }
}

/// Whether a runtime intrinsic is a raw-pointer power: raw byte I/O (which
/// reads/writes linear memory through a pointer host-side), integer↔pointer
/// or byte-region↔typed-pointer *reinterpretation*, memory copy/fill/grow, or
/// the allocator. Calling these requires an `unsafe` context.
///
/// Notably *not* here: `null`, `addr`, and the (wrapping) pointer arithmetic
/// intrinsics. Computing or inspecting an address — without claiming a
/// pointee lives there or touching memory — is safe, matching Rust's
/// `ptr::null` / `addr` / `wrapping_add` (all safe) vs. `read`/`write`/
/// `from_exposed_addr`/`std::alloc` (all `unsafe`).
fn is_raw_power_runtime(abi: &crate::hir::RuntimeAbi) -> bool {
    use crate::hir::RuntimeAbi as R;
    matches!(
        abi,
        R::Write
            | R::Read
            | R::PathOpen
            | R::Close
            | R::Poll
            | R::At
            | R::FromAddr
            | R::ArrayPtr
            | R::DropInPlace
            | R::MemoryGrow
            | R::MemoryCopy
            | R::MemoryFill
    )
}

/// First raw-pointer-power violation in a block, as `(span, description)`.
/// `in_unsafe` is true while scanning the body of an `unsafe fn` or an
/// `unsafe { ... }` block; raw powers are only reported when it is false.
fn unsafe_violation(program: &Program, block: &Block, in_unsafe: bool) -> Option<(SpanId, String)> {
    for stmt in &block.stmts {
        if let Some(v) = unsafe_violation_stmt(program, stmt, in_unsafe) {
            return Some(v);
        }
    }
    block
        .expr
        .as_deref()
        .and_then(|e| unsafe_violation_expr(program, e, in_unsafe))
}

fn unsafe_violation_stmt(
    program: &Program,
    stmt: &Stmt,
    in_unsafe: bool,
) -> Option<(SpanId, String)> {
    match stmt {
        Stmt::DerefAssign { span, .. } => {
            if in_unsafe {
                None
            } else {
                Some((*span, "*p = v (raw pointer store)".to_string()))
            }
        }
        Stmt::Let { value, .. } => unsafe_violation_expr(program, value, in_unsafe),
        Stmt::Assign { value, .. } => unsafe_violation_expr(program, value, in_unsafe),
        Stmt::FieldAssign { object, value, .. } => {
            unsafe_violation_expr(program, object, in_unsafe)
                .or_else(|| unsafe_violation_expr(program, value, in_unsafe))
        }
        Stmt::Expr(e) => unsafe_violation_expr(program, e, in_unsafe),
        Stmt::Loop { body, .. } => unsafe_violation(program, body, in_unsafe),
        Stmt::While {
            condition, body, ..
        } => unsafe_violation_expr(program, condition, in_unsafe)
            .or_else(|| unsafe_violation(program, body, in_unsafe)),
        Stmt::Return { value, .. } => value
            .as_ref()
            .and_then(|e| unsafe_violation_expr(program, e, in_unsafe)),
        Stmt::Break { .. } | Stmt::Drop { .. } => None,
    }
}

fn unsafe_violation_expr(
    program: &Program,
    expr: &Expr,
    in_unsafe: bool,
) -> Option<(SpanId, String)> {
    match &expr.kind {
        ExprKind::Deref(_) => {
            if in_unsafe {
                None
            } else {
                Some((expr.span, "*p (raw pointer load)".to_string()))
            }
        }
        ExprKind::UnsafeBlock(b) => unsafe_violation(program, b, true),
        ExprKind::Call { func, args, .. } => {
            let callee = program.functions.get(func.0 as usize);
            if !in_unsafe {
                let raw = callee
                    .and_then(|f| f.runtime.as_ref())
                    .is_some_and(is_raw_power_runtime);
                let unsafe_fn = callee.is_some_and(|f| f.unsafe_fn);
                if raw || unsafe_fn {
                    let name = callee
                        .and_then(|f| program.symbols.get(f.name.0 as usize))
                        .map(|s| program.interner.resolve(&s.name).to_string())
                        .unwrap_or_else(|| "?".to_string());
                    return Some((expr.span, format!("call to unsafe function `{name}`")));
                }
            }
            args.iter()
                .find_map(|a| unsafe_violation_expr(program, a, in_unsafe))
        }
        ExprKind::Binary { left, right, .. } => unsafe_violation_expr(program, left, in_unsafe)
            .or_else(|| unsafe_violation_expr(program, right, in_unsafe)),
        ExprKind::Field { base, .. } | ExprKind::TupleIndex { base, .. } => {
            unsafe_violation_expr(program, base, in_unsafe)
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => unsafe_violation_expr(program, scrutinee, in_unsafe).or_else(|| {
            arms.iter()
                .find_map(|a| unsafe_violation_expr(program, &a.body, in_unsafe))
        }),
        ExprKind::MethodCall { receiver, args, .. }
        | ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            unsafe_violation_expr(program, receiver, in_unsafe).or_else(|| {
                args.iter()
                    .find_map(|a| unsafe_violation_expr(program, a, in_unsafe))
            })
        }
        ExprKind::Coerce { value, .. } => unsafe_violation_expr(program, value, in_unsafe),
        ExprKind::StructLit { fields, .. } => fields
            .iter()
            .find_map(|(_, e)| unsafe_violation_expr(program, e, in_unsafe)),
        ExprKind::VariantLit { fields, .. } => fields
            .iter()
            .find_map(|(_, e)| unsafe_violation_expr(program, e, in_unsafe)),
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => elems
            .iter()
            .find_map(|e| unsafe_violation_expr(program, e, in_unsafe)),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => unsafe_violation_expr(program, condition, in_unsafe)
            .or_else(|| unsafe_violation(program, then_branch, in_unsafe))
            .or_else(|| {
                else_branch
                    .as_ref()
                    .and_then(|b| unsafe_violation(program, b, in_unsafe))
            }),
        ExprKind::Block(b) => unsafe_violation(program, b, in_unsafe),
        ExprKind::BitNot(e) | ExprKind::Neg(e) => unsafe_violation_expr(program, e, in_unsafe),
        // `spawn(f)` indirectly invokes `f` in a new task, so it must observe
        // the same unsafe boundary as a direct call: safe code may not spawn
        // an `unsafe fn`.
        ExprKind::Spawn { func } => {
            let callee = program.functions.get(func.0 as usize);
            if !in_unsafe && callee.is_some_and(|f| f.unsafe_fn) {
                let name = callee
                    .and_then(|f| program.symbols.get(f.name.0 as usize))
                    .map(|s| program.interner.resolve(&s.name).to_string())
                    .unwrap_or_else(|| "?".to_string());
                return Some((expr.span, format!("spawn of unsafe function `{name}`")));
            }
            None
        }
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::ConstParam(_)
        | ExprKind::Error => None,
        // A block literal is written at its *caller*, in the caller's unsafe
        // context, so its body must observe the same safe/unsafe boundary as
        // the surrounding code: a safe caller may not pass `|e| { *e = ... }`
        // for a `block(*mut T)` element. The parameters carry names only, so
        // the body is the only expression-bearing part.
        ExprKind::BlockLit { body, .. } => unsafe_violation(program, body, in_unsafe),
        // A block call's arguments are expressions evaluated at the call site
        // (inside the inline fn body), so `b(*p)` must be checked here; the
        // callee's block body is checked separately when its literal is
        // written.
        ExprKind::BlockCall { args, .. } => args
            .iter()
            .find_map(|a| unsafe_violation_expr(program, a, in_unsafe)),
    }
}

/// Whether a match-arm pattern binds any `mut` binding. A `mut` arm binding is
/// an exclusive borrow of the scrutinee (its writes copy back into the
/// scrutinee's place), so it is only legal under an explicit `match mut`.
/// `read`/bare bindings borrow, `own` bindings move — neither writes back.
fn pattern_has_mut_binding(pattern: &crate::hir::Pattern) -> bool {
    match pattern {
        crate::hir::Pattern::Binding { mode, .. } => *mode == PassMode::Mut,
        crate::hir::Pattern::Tuple { elems, .. } => elems.iter().any(pattern_has_mut_binding),
        crate::hir::Pattern::Variant { fields, .. }
        | crate::hir::Pattern::Struct { fields, .. } => {
            fields.iter().any(|f| pattern_has_mut_binding(&f.pattern))
        }
        crate::hir::Pattern::Wildcard { .. }
        | crate::hir::Pattern::Int { .. }
        | crate::hir::Pattern::Bool { .. } => false,
    }
}

/// The first `read`/`mut`/bare binding of a non-`Copy` payload in an arm
/// pattern — a second-class *borrow* (as opposed to an `own` move). Returns
/// `(symbol, span)`; used to reject borrows in consuming matches. Copy
/// bindings are not borrows.
fn pattern_first_borrow_binding(
    copy_ctx: CopyCtx<'_>,
    pattern: &crate::hir::Pattern,
) -> Option<(crate::hir::SymbolId, SpanId)> {
    match pattern {
        crate::hir::Pattern::Binding {
            symbol,
            ty,
            mode,
            span,
        } => {
            if copy_ctx.effect(*mode, ty) == crate::hir::cfg::Effect::Borrow {
                Some((*symbol, *span))
            } else {
                None
            }
        }
        crate::hir::Pattern::Tuple { elems, .. } => elems
            .iter()
            .find_map(|p| pattern_first_borrow_binding(copy_ctx, p)),
        crate::hir::Pattern::Variant { fields, .. }
        | crate::hir::Pattern::Struct { fields, .. } => fields
            .iter()
            .find_map(|f| pattern_first_borrow_binding(copy_ctx, &f.pattern)),
        crate::hir::Pattern::Wildcard { .. }
        | crate::hir::Pattern::Int { .. }
        | crate::hir::Pattern::Bool { .. } => None,
    }
}

/// Collect every statically-dispatched callee in a block (used to find inline
/// recursion). `out` is a set so shared edges are deduplicated.
fn collect_callees(
    block: &crate::hir::Block,
    out: &mut std::collections::HashSet<crate::hir::FuncId>,
) {
    fn expr(e: &crate::hir::Expr, out: &mut std::collections::HashSet<crate::hir::FuncId>) {
        use crate::hir::ExprKind;
        match &e.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Ident(_)
            | ExprKind::ConstParam(_)
            | ExprKind::Error => {}
            ExprKind::Binary { left, right, .. } => {
                expr(left, out);
                expr(right, out);
            }
            ExprKind::Call { func, args, .. } => {
                out.insert(*func);
                for a in args {
                    expr(a, out);
                }
            }
            ExprKind::Spawn { func } => {
                out.insert(*func);
            }
            ExprKind::StructLit { fields, .. } => {
                for (_, f) in fields {
                    expr(f, out);
                }
            }
            ExprKind::VariantLit { fields, .. } => {
                for (_, f) in fields {
                    expr(f, out);
                }
            }
            ExprKind::Match {
                scrutinee, arms, ..
            } => {
                expr(scrutinee, out);
                for arm in arms {
                    expr(&arm.body, out);
                }
            }
            ExprKind::Field { base, .. } => expr(base, out),
            ExprKind::Deref(base) => expr(base, out),
            ExprKind::MethodCall { receiver, args, .. } => {
                expr(receiver, out);
                for a in args {
                    expr(a, out);
                }
            }
            ExprKind::DynCall { receiver, args, .. }
            | ExprKind::TraitBoundCall { receiver, args, .. } => {
                expr(receiver, out);
                for a in args {
                    expr(a, out);
                }
            }
            ExprKind::Coerce { value, .. } => expr(value, out),
            ExprKind::BitNot(inner) | ExprKind::Neg(inner) => expr(inner, out),
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                for el in elems {
                    expr(el, out);
                }
            }
            ExprKind::TupleIndex { base, .. } => expr(base, out),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                expr(condition, out);
                walk_block(then_branch, out);
                if let Some(b) = else_branch {
                    walk_block(b, out);
                }
            }
            ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => walk_block(b, out),
            ExprKind::BlockLit { body, .. } => walk_block(body, out),
            ExprKind::BlockCall { args, .. } => {
                for a in args {
                    expr(a, out);
                }
            }
        }
    }
    fn stmt(s: &crate::hir::Stmt, out: &mut std::collections::HashSet<crate::hir::FuncId>) {
        match s {
            crate::hir::Stmt::Expr(e) => expr(e, out),
            crate::hir::Stmt::Let { value, .. } => expr(value, out),
            crate::hir::Stmt::Assign { value, .. } => expr(value, out),
            crate::hir::Stmt::DerefAssign { ptr, value, .. } => {
                expr(ptr, out);
                expr(value, out);
            }
            crate::hir::Stmt::FieldAssign { object, value, .. } => {
                expr(object, out);
                expr(value, out);
            }
            crate::hir::Stmt::Loop { body, .. } => walk_block(body, out),
            crate::hir::Stmt::While {
                condition, body, ..
            } => {
                expr(condition, out);
                walk_block(body, out);
            }
            crate::hir::Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    expr(v, out);
                }
            }
            crate::hir::Stmt::Break { .. } | crate::hir::Stmt::Drop { .. } => {}
        }
    }
    fn walk_block(b: &crate::hir::Block, out: &mut std::collections::HashSet<crate::hir::FuncId>) {
        for s in &b.stmts {
            stmt(s, out);
        }
        if let Some(e) = &b.expr {
            expr(e, out);
        }
    }
    walk_block(block, out);
}

/// Return the first `spawn(f)` in `block` whose target `f` is an `inline fn`,
/// as `(spawn span, target FuncId)`. Used to reject a spawn that inlining would
/// otherwise empty into a no-op task.
fn first_inline_spawn(
    block: &crate::hir::Block,
    functions: &[crate::hir::Function],
) -> Option<(SpanId, crate::hir::FuncId)> {
    use crate::hir::ExprKind;

    fn expr(
        e: &crate::hir::Expr,
        functions: &[crate::hir::Function],
    ) -> Option<(SpanId, crate::hir::FuncId)> {
        match &e.kind {
            ExprKind::Spawn { func } => {
                let f = functions.get(func.0 as usize)?;
                if f.is_inline {
                    Some((e.span, *func))
                } else {
                    None
                }
            }
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Ident(_)
            | ExprKind::ConstParam(_)
            | ExprKind::Error => None,
            ExprKind::Binary { left, right, .. } => {
                expr(left, functions).or_else(|| expr(right, functions))
            }
            ExprKind::Call { args, .. } => args.iter().find_map(|a| expr(a, functions)),
            ExprKind::StructLit { fields, .. } | ExprKind::VariantLit { fields, .. } => {
                fields.iter().find_map(|(_, f)| expr(f, functions))
            }
            ExprKind::Match {
                scrutinee, arms, ..
            } => expr(scrutinee, functions)
                .or_else(|| arms.iter().find_map(|a| expr(&a.body, functions))),
            ExprKind::Field { base, .. } => expr(base, functions),
            ExprKind::Deref(base) => expr(base, functions),
            ExprKind::MethodCall { receiver, args, .. } => {
                expr(receiver, functions).or_else(|| args.iter().find_map(|a| expr(a, functions)))
            }
            ExprKind::DynCall { receiver, args, .. }
            | ExprKind::TraitBoundCall { receiver, args, .. } => {
                expr(receiver, functions).or_else(|| args.iter().find_map(|a| expr(a, functions)))
            }
            ExprKind::Coerce { value, .. } => expr(value, functions),
            ExprKind::BitNot(inner) | ExprKind::Neg(inner) => expr(inner, functions),
            ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
                elems.iter().find_map(|el| expr(el, functions))
            }
            ExprKind::TupleIndex { base, .. } => expr(base, functions),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => expr(condition, functions)
                .or_else(|| walk_block(then_branch, functions))
                .or_else(|| else_branch.as_ref().and_then(|b| walk_block(b, functions))),
            ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => walk_block(b, functions),
            ExprKind::BlockLit { body, .. } => walk_block(body, functions),
            ExprKind::BlockCall { args, .. } => args.iter().find_map(|a| expr(a, functions)),
        }
    }

    fn stmt(
        s: &crate::hir::Stmt,
        functions: &[crate::hir::Function],
    ) -> Option<(SpanId, crate::hir::FuncId)> {
        match s {
            crate::hir::Stmt::Expr(e) => expr(e, functions),
            crate::hir::Stmt::Let { value, .. } => expr(value, functions),
            crate::hir::Stmt::Assign { value, .. } => expr(value, functions),
            crate::hir::Stmt::DerefAssign { ptr, value, .. } => {
                expr(ptr, functions).or_else(|| expr(value, functions))
            }
            crate::hir::Stmt::FieldAssign { object, value, .. } => {
                expr(object, functions).or_else(|| expr(value, functions))
            }
            crate::hir::Stmt::Loop { body, .. } => walk_block(body, functions),
            crate::hir::Stmt::While {
                condition, body, ..
            } => expr(condition, functions).or_else(|| walk_block(body, functions)),
            crate::hir::Stmt::Return { value, .. } => {
                value.as_ref().and_then(|v| expr(v, functions))
            }
            crate::hir::Stmt::Break { .. } | crate::hir::Stmt::Drop { .. } => None,
        }
    }

    fn walk_block(
        b: &crate::hir::Block,
        functions: &[crate::hir::Function],
    ) -> Option<(SpanId, crate::hir::FuncId)> {
        for s in &b.stmts {
            if let Some(found) = stmt(s, functions) {
                return Some(found);
            }
        }
        b.expr.as_ref().and_then(|e| expr(e, functions))
    }

    walk_block(block, functions)
}

/// Root symbol of a *field path* (`e.field`, `e.0`, nested), but NOT crossing
/// a `Deref`. Used by the `FieldAssign` read-block-param gate: writing through
/// a raw pointer (`(*e).field = v`, where `e` is a `read` block element of
/// pointer type) mutates the *pointee*, not the shared-borrowed pointer value,
/// so it must not count as a write to the element itself. Only a field path
/// rooted directly at the element (`e.field`, `e.0`, `e.a.b`) is a write to the
/// element.
fn field_root_symbol(expr: &crate::hir::Expr) -> Option<SymbolId> {
    match &expr.kind {
        crate::hir::ExprKind::Ident(sym) => Some(*sym),
        crate::hir::ExprKind::Field { base, .. }
        | crate::hir::ExprKind::TupleIndex { base, .. } => field_root_symbol(base),
        // Stop the walk at a deref: the write is to the pointee, not the place.
        crate::hir::ExprKind::Deref(_) => None,
        _ => None,
    }
}

#[cfg(test)]
mod field_root_tests {
    use super::{InternSymbol, SpanId, SymbolId, field_root_symbol};
    use crate::hir::{Expr, ExprKind, Type};

    fn sym(n: u32) -> SymbolId {
        SymbolId(n)
    }

    fn span() -> SpanId {
        SpanId(0)
    }

    fn ident(s: u32) -> Expr {
        Expr {
            kind: ExprKind::Ident(sym(s)),
            ty: Type::Usize,
            span: span(),
        }
    }

    fn deref(inner: Expr) -> Expr {
        Expr {
            kind: ExprKind::Deref(Box::new(inner)),
            ty: Type::Usize,
            span: span(),
        }
    }

    fn field(base: Expr) -> Expr {
        Expr {
            kind: ExprKind::Field {
                base: Box::new(base),
                field: InternSymbol::default(),
            },
            ty: Type::Usize,
            span: span(),
        }
    }

    fn tuple_index(base: Expr) -> Expr {
        Expr {
            kind: ExprKind::TupleIndex {
                base: Box::new(base),
                index: 0,
            },
            ty: Type::Usize,
            span: span(),
        }
    }

    #[test]
    fn stops_at_deref() {
        // `(*e).field = v` — a pointer write; not a write to the element.
        assert_eq!(field_root_symbol(&field(deref(ident(7)))), None);
        assert_eq!(field_root_symbol(&deref(ident(7))), None);
        assert_eq!(field_root_symbol(&tuple_index(deref(ident(7)))), None);
    }

    #[test]
    fn follows_field_paths_without_deref() {
        // `e.field = v` and `e.a.b = v` are writes to the element itself.
        assert_eq!(field_root_symbol(&field(ident(7))), Some(sym(7)));
        assert_eq!(field_root_symbol(&tuple_index(ident(7))), Some(sym(7)));
        assert_eq!(field_root_symbol(&field(field(ident(7)))), Some(sym(7)));
    }
}
