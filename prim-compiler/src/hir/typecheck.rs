use super::{
    BinaryOp, Block, Expr, ExprKind, FuncId, Function, InternSymbol, Program, SpanId, Stmt,
    StructId, SymbolId, Type,
};
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
    DbgUnsupportedType(Type),
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
            TypeCheckKind::DbgUnsupportedType(ty) => {
                write!(f, "@dbg does not support values of type {}", ty)
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
            TypeCheckKind::Legacy(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for TypeCheckError {}

pub fn type_check(program: &mut Program) -> Result<(), TypeCheckError> {
    let mut checker = Checker::new(program);
    checker.collect_signatures()?;
    checker.check_functions()
}

struct Checker<'a> {
    program: &'a mut Program,
    func_sigs: HashMap<FuncId, (Vec<Type>, Option<Type>)>,
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
    loop_depth: usize,
    /// Return type of the function currently being checked. Used by
    /// `Stmt::Return` to validate the value type.
    current_ret: Option<Type>,
    /// Type parameters of the function currently being checked. Indexed
    /// by `TypeParamId`. Cleared between functions.
    current_type_params: Vec<crate::hir::TypeParam>,
}

struct CheckedField {
    expected: Type,
    actual: Type,
}

struct MatchCoverage {
    covered: Vec<bool>,
    covered_count: u32,
    has_wildcard: bool,
}

impl MatchCoverage {
    fn new(variant_count: u32) -> Self {
        Self {
            covered: vec![false; variant_count as usize],
            covered_count: 0,
            has_wildcard: false,
        }
    }

    fn cover_wildcard(&mut self) {
        self.has_wildcard = true;
    }

    fn cover_variant(&mut self, variant_idx: u32) {
        let Some(slot) = self.covered.get_mut(variant_idx as usize) else {
            return;
        };
        if !*slot {
            *slot = true;
            self.covered_count += 1;
        }
    }

    fn is_exhaustive(&self) -> bool {
        self.has_wildcard || self.covered_count == self.covered.len() as u32
    }
}

#[derive(Clone, Copy)]
enum BoundCheckMode {
    ConcreteOnly,
    AllowForwardedParam,
}

impl<'a> Checker<'a> {
    fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            func_sigs: HashMap::new(),
            func_type_params: HashMap::new(),
            struct_type_params: HashMap::new(),
            struct_fields: HashMap::new(),
            enum_type_params: HashMap::new(),
            enum_variant_fields: HashMap::new(),
            loop_depth: 0,
            current_ret: None,
            current_type_params: Vec::new(),
        }
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
            _ => format!("{}", ty),
        }
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
            (Type::Array(a), Type::Array(b)) => Self::infer_pins(a, b, pins),
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
            Type::Array(elem) => Type::Array(Box::new(Self::substitute_params(elem, pins))),
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
            Type::Array(elem) => {
                Type::Array(Box::new(Self::substitute_params_with_slice(elem, args)))
            }
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
        match ty {
            Type::Struct(sid, _) if self.program.impls.contains_key(&(bound, *sid)) => Ok(()),
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
            if let Some(bound) = param.bound {
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

    fn check_match_pattern(
        &mut self,
        pattern: &mut crate::hir::Pattern,
        enum_id: crate::hir::EnumId,
        enum_args: &[Type],
        variant_count: u32,
        coverage: &mut MatchCoverage,
        span: SpanId,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<Vec<SymbolId>, TypeCheckError> {
        match pattern {
            crate::hir::Pattern::Wildcard { .. } => {
                coverage.cover_wildcard();
                Ok(Vec::new())
            }
            crate::hir::Pattern::Variant {
                enum_id: pattern_enum,
                variant_idx,
                bindings,
                ..
            } => {
                if *pattern_enum != enum_id {
                    return Err(self.error(
                        span,
                        TypeCheckKind::Legacy(
                            "pattern's enum does not match scrutinee's enum".to_string(),
                        ),
                    ));
                }
                if *variant_idx >= variant_count {
                    return Err(self.error(
                        span,
                        TypeCheckKind::Legacy("variant index out of range".to_string()),
                    ));
                }

                coverage.cover_variant(*variant_idx);
                let field_map = self
                    .enum_variant_fields
                    .get(&(enum_id, *variant_idx))
                    .cloned()
                    .ok_or_else(|| {
                        self.error(
                            span,
                            TypeCheckKind::Legacy("variant fields not found".to_string()),
                        )
                    })?;
                let mut arm_bindings = Vec::with_capacity(bindings.len());
                for (field_sym, binding_sym, binding_ty) in bindings.iter_mut() {
                    let declared = field_map.get(field_sym).cloned().ok_or_else(|| {
                        self.error(
                            span,
                            TypeCheckKind::Legacy(format!(
                                "no field '{}' on variant",
                                self.program.interner.resolve(field_sym)
                            )),
                        )
                    })?;
                    let resolved = Self::substitute_params_with_slice(&declared, enum_args);
                    *binding_ty = resolved.clone();
                    locals.insert(*binding_sym, resolved);
                    arm_bindings.push(*binding_sym);
                }
                Ok(arm_bindings)
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
        if !self.program.impls.contains_key(&(*tid, *sid)) {
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
            if !f.type_params.is_empty() {
                self.func_type_params.insert(f.id, f.type_params.clone());
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
        for p in &func.params {
            locals.insert(p.name, p.ty.clone());
        }
        self.loop_depth = 0;
        self.current_ret = func.ret.clone();
        self.current_type_params = func.type_params.clone();

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
            } else if func.runtime.is_none() {
                return Err(self.error(func.span, TypeCheckKind::MissingReturnValue));
            }
        } else if let Some(expr) = &mut func.body.expr {
            self.check_expr(expr, &mut locals)?;
        }

        self.finalize_block(&mut func.body);
        Ok(())
    }

    fn check_stmt(
        &mut self,
        stmt: &mut Stmt,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<(), TypeCheckError> {
        match stmt {
            Stmt::Let {
                name,
                ty,
                value,
                span,
                ..
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
                locals.insert(*name, ty.clone());
                Ok(())
            }
            Stmt::Assign {
                target,
                value,
                span,
            } => {
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
            Stmt::DerefAssign { ptr, value, span } => {
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
            ExprKind::Int(_) => Ok(ty.clone()),
            ExprKind::Float(_) => Ok(ty.clone()),
            ExprKind::Bool(_) => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::Bool;
                }
                Ok(ty.clone())
            }
            ExprKind::Str(_) => Ok(ty.clone()),
            ExprKind::Ident(symbol) => {
                if let Some(t) = locals.get(symbol) {
                    *ty = t.clone();
                    Ok(t.clone())
                } else if let Some(global_ty) = self.global_type(*symbol) {
                    *ty = global_ty.clone();
                    Ok(global_ty)
                } else {
                    *ty = Type::Undetermined;
                    Ok(Type::Undetermined)
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
            } => {
                // Step 1: typecheck the receiver. Dispatch differs based on
                // whether it's a concrete struct (direct call) or a trait
                // value (dynamic dispatch via vtable).
                let recv_ty = self.check_expr(receiver, locals)?;
                let sid = match &recv_ty {
                    Type::Struct(id, _) => *id,
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
                            .unwrap_or(Type::Undetermined);
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
                        let trait_id = *tid;
                        *kind = ExprKind::DynCall {
                            receiver: Box::new(receiver_owned),
                            trait_id,
                            method_idx,
                            args: args_owned,
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
                        let ret_ty = sig.ret.as_ref().map(subst).unwrap_or(Type::Undetermined);

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
                        let tp_id = *tp_id;
                        *kind = ExprKind::TraitBoundCall {
                            receiver: Box::new(receiver_owned),
                            type_param: tp_id,
                            bound: bound_tid,
                            method: method_name,
                            args: args_owned,
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
                    _ => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "method call receiver must be a struct or trait type, got {}",
                                recv_ty
                            )),
                        ));
                    }
                };
                let method_name = *method;
                // Step 2: look up the impl method.
                let func = match self.program.impl_methods.get(&(sid, method_name)).copied() {
                    Some(f) => f,
                    None => {
                        let name = self.program.interner.resolve(&method_name).to_string();
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "no method '{}' on struct {:?}",
                                name, sid
                            )),
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
                // Step 4: rewrite in place to a Call. Impl methods are
                // not yet generic, so type_args stays empty.
                *kind = ExprKind::Call {
                    func,
                    type_args: Vec::new(),
                    args: new_args,
                };
                // Step 5: typecheck the rewritten call. The receiver's type
                // is already known so check_expr on it is a no-op.
                let ExprKind::Call { func, args, .. } = kind else {
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
                let ret_ty = ret.unwrap_or(Type::Undetermined);
                *ty = ret_ty.clone();
                Ok(ret_ty)
            }
            ExprKind::Call {
                func,
                type_args,
                args,
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
                    // Generic callee: infer one type-arg per type param by
                    // matching the formal/actual structure, then substitute
                    // before unifying.
                    let mut pins: HashMap<crate::hir::TypeParamId, Type> = HashMap::new();
                    let mut arg_types: Vec<Type> = Vec::with_capacity(args.len());
                    for (arg, formal) in args.iter_mut().zip(params.iter()) {
                        self.apply_expected(arg, formal);
                        let got = self.check_expr(arg, locals)?;
                        if !Self::infer_pins(formal, &got, &mut pins) {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::Legacy(format!(
                                    "conflicting inferences for a type parameter (got {} for a position already pinned to a different type)",
                                    self.type_name(&got)
                                )),
                            ));
                        }
                        arg_types.push(got);
                    }
                    let inferred = self.resolve_type_args(
                        &tparams,
                        &pins,
                        *span,
                        BoundCheckMode::AllowForwardedParam,
                        "cannot infer type parameter ",
                    )?;
                    // Substitute pins into formals and unify against the
                    // already-checked actual types.
                    for (i, (arg, formal)) in args.iter_mut().zip(params.iter()).enumerate() {
                        let substituted = Self::substitute_params(formal, &pins);
                        let got = &arg_types[i];
                        if self.try_coerce_struct_to_trait(arg, got, &substituted) {
                            continue;
                        }
                        if self.unify(&substituted, got).is_none() {
                            let expected_name = self.type_name(&substituted);
                            let found_name = self.type_name(got);
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected_name,
                                    found: found_name,
                                },
                            ));
                        }
                    }
                    let ret_ty = ret
                        .as_ref()
                        .map(|r| Self::substitute_params(r, &pins))
                        .unwrap_or(Type::Undetermined);
                    *type_args = inferred;
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
                let ret_ty = ret.clone().unwrap_or(Type::Undetermined);
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
                    _ => {
                        *ty = Type::Undetermined;
                        return Ok(Type::Undetermined);
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
                if self.is_integer(&operand_ty) || matches!(operand_ty, Type::IntVar) {
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
                let arr = Type::Array(Box::new(elem_ty.unwrap_or(Type::Undetermined)));
                *ty = arr.clone();
                Ok(arr)
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
                    (None, None) => Type::Undetermined,
                };
                *ty = result_ty.clone();
                Ok(result_ty)
            }
            ExprKind::Block(block) => {
                let block_ty = self.check_block(block, locals)?;
                let result_ty = block_ty.unwrap_or(Type::Undetermined);
                *ty = result_ty.clone();
                Ok(result_ty)
            }
            ExprKind::Dbg { inner, .. } => {
                let inner_ty = self.check_expr(inner, locals)?;
                let resolved = self.finalize_type(&inner_ty);
                if !self.is_dbg_supported(&resolved) {
                    return Err(self.error(*span, TypeCheckKind::DbgUnsupportedType(resolved)));
                }
                *ty = inner_ty.clone();
                Ok(inner_ty)
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
            ExprKind::Match { scrutinee, arms } => {
                let scrut_ty = self.check_expr(scrutinee, locals)?;
                let (eid, eargs) = match &scrut_ty {
                    Type::Enum(id, args) => (*id, args.clone()),
                    other => {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::Legacy(format!(
                                "match scrutinee must be an enum type, got {}",
                                self.type_name(other)
                            )),
                        ));
                    }
                };
                let variant_count = self
                    .program
                    .enums
                    .get(eid.0 as usize)
                    .map(|e| e.variants.len() as u32)
                    .unwrap_or(0);
                let mut coverage = MatchCoverage::new(variant_count);
                let mut result_ty: Option<Type> = None;
                for arm in arms.iter_mut() {
                    let arm_bindings = self.check_match_pattern(
                        &mut arm.pattern,
                        eid,
                        &eargs,
                        variant_count,
                        &mut coverage,
                        *span,
                        locals,
                    )?;
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
                // Exhaustiveness: every variant must be covered, or a
                // wildcard must be present.
                if !coverage.is_exhaustive() {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::Legacy(
                            "non-exhaustive match: not all variants are covered".to_string(),
                        ),
                    ));
                }
                let final_ty = result_ty.unwrap_or(Type::Undetermined);
                *ty = final_ty.clone();
                Ok(final_ty)
            }
        }
    }

    fn is_dbg_supported(&self, t: &Type) -> bool {
        self.is_numeric(t) || matches!(t, Type::Bool)
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
            ) if ma == mb => self.unify(pa, pb).map(|p| Type::Pointer {
                mutable: *ma,
                pointee: Box::new(p),
            }),
            (Type::Array(a_elem), Type::Array(b_elem)) => {
                self.unify(a_elem, b_elem).map(|e| Type::Array(Box::new(e)))
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
            ExprKind::Dbg { inner, .. } => {
                self.apply_expected(inner, expected);
                *ty = inner.ty.clone();
            }
            ExprKind::VariantLit {
                enum_id, type_args, ..
            } => {
                if let Type::Enum(expected_enum, expected_args) = expected {
                    if enum_id == expected_enum {
                        *type_args = expected_args.clone();
                        *ty = expected.clone();
                    }
                }
            }
            ExprKind::Match { arms, .. } => {
                for arm in arms {
                    self.apply_expected(&mut arm.body, expected);
                }
            }
            _ => {}
        }
    }

    fn finalize_block(&self, block: &mut Block) {
        for stmt in &mut block.stmts {
            self.finalize_stmt(stmt);
        }
        if let Some(expr) = &mut block.expr {
            self.finalize_expr(expr);
        }
    }

    fn finalize_stmt(&self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Let { ty, value, .. } => {
                self.finalize_expr(value);
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    *ty = self.finalize_type(&value.ty);
                }
            }
            Stmt::Assign { value, .. } => {
                self.finalize_expr(value);
            }
            Stmt::Expr(e) => self.finalize_expr(e),
            Stmt::Loop { body, .. } => {
                self.finalize_block(body);
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.finalize_expr(condition);
                self.finalize_block(body);
            }
            Stmt::Break { .. } => {}
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.finalize_expr(v);
                }
            }
            Stmt::DerefAssign { ptr, value, .. } => {
                self.finalize_expr(ptr);
                self.finalize_expr(value);
            }
        }
    }

    fn finalize_expr(&self, expr: &mut Expr) {
        let Expr { kind, ty, .. } = expr;
        match kind {
            ExprKind::Int(_) => {
                if matches!(ty, Type::IntVar) {
                    *ty = Type::I32;
                }
            }
            ExprKind::Float(_) => {
                if matches!(ty, Type::FloatVar) {
                    *ty = Type::F64;
                }
            }
            ExprKind::Binary { left, right, .. } => {
                self.finalize_expr(left);
                self.finalize_expr(right);
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    *ty = self.finalize_type(&left.ty);
                }
            }
            ExprKind::Call { args, .. } => {
                for arg in args {
                    self.finalize_expr(arg);
                }
            }
            // After typecheck, MethodCall is rewritten to Call or DynCall so
            // this arm is only reached if check_expr returned early with an
            // error before the rewrite. Recurse into the children to keep
            // finalize total.
            ExprKind::MethodCall { receiver, args, .. } => {
                self.finalize_expr(receiver);
                for arg in args {
                    self.finalize_expr(arg);
                }
            }
            ExprKind::DynCall { receiver, args, .. } => {
                self.finalize_expr(receiver);
                for arg in args {
                    self.finalize_expr(arg);
                }
            }
            ExprKind::TraitBoundCall { receiver, args, .. } => {
                self.finalize_expr(receiver);
                for arg in args {
                    self.finalize_expr(arg);
                }
            }
            ExprKind::Coerce { value, .. } => {
                self.finalize_expr(value);
            }
            ExprKind::StructLit { fields, .. } => {
                for (_, val) in fields {
                    self.finalize_expr(val);
                }
            }
            ExprKind::VariantLit { fields, .. } => {
                for (_, val) in fields {
                    self.finalize_expr(val);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.finalize_expr(scrutinee);
                for arm in arms {
                    self.finalize_expr(&mut arm.body);
                }
            }
            ExprKind::Field { base, .. } => {
                self.finalize_expr(base);
            }
            ExprKind::Deref(base) => {
                self.finalize_expr(base);
            }
            ExprKind::BitNot(operand) => {
                self.finalize_expr(operand);
                if matches!(ty, Type::IntVar | Type::Undetermined) {
                    *ty = self.finalize_type(&operand.ty);
                }
            }
            ExprKind::ArrayLit(elements) => {
                for elem in elements.iter_mut() {
                    self.finalize_expr(elem);
                }
                if let Type::Array(elem_ty) = ty {
                    if matches!(elem_ty.as_ref(), Type::IntVar | Type::FloatVar) {
                        *ty = Type::Array(Box::new(self.finalize_type(elem_ty)));
                    }
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.finalize_expr(condition);
                self.finalize_block(then_branch);
                if let Some(else_block) = else_branch {
                    self.finalize_block(else_block);
                }
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    if let Some(then_expr) = &then_branch.expr {
                        *ty = self.finalize_type(&then_expr.ty);
                    }
                }
            }
            ExprKind::Block(block) => {
                self.finalize_block(block);
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    if let Some(expr) = &block.expr {
                        *ty = self.finalize_type(&expr.ty);
                    }
                }
            }
            ExprKind::Dbg { inner, .. } => {
                self.finalize_expr(inner);
                if matches!(ty, Type::IntVar | Type::FloatVar | Type::Undetermined) {
                    *ty = self.finalize_type(&inner.ty);
                }
            }
            _ => {}
        }
    }

    fn finalize_type(&self, ty: &Type) -> Type {
        match ty {
            Type::IntVar => Type::I32,
            Type::FloatVar => Type::F64,
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
