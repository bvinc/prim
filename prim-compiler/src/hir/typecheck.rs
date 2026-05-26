use super::{
    BinaryOp, Block, Expr, ExprKind, FuncId, Function, InternSymbol, Program, SpanId, Stmt,
    StructId, SymbolId, Type,
};
use prim_tok::{FileId, Span};
use std::collections::HashMap;

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
        expected: Type,
        found: Type,
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
    struct_fields: HashMap<StructId, HashMap<InternSymbol, Type>>,
    loop_depth: usize,
}

impl<'a> Checker<'a> {
    fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            func_sigs: HashMap::new(),
            struct_fields: HashMap::new(),
            loop_depth: 0,
        }
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
        }

        for f in &self.program.functions {
            for param in &f.params {
                if matches!(param.ty, Type::Undetermined) {
                    return Err(self.error(param.span, TypeCheckKind::UndeterminedParamType));
                }
            }
            let params = f.params.iter().map(|p| p.ty.clone()).collect();
            self.func_sigs.insert(f.id, (params, f.ret.clone()));
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

        for stmt in &mut func.body.stmts {
            self.check_stmt(stmt, &mut locals)?;
        }

        if let Some(ret_ty) = &func.ret {
            if let Some(expr) = &mut func.body.expr {
                self.apply_expected(expr, ret_ty);
                let expr_ty = self.check_expr(expr, &mut locals)?;
                match self.unify(&expr_ty, ret_ty) {
                    Some(unified) => {
                        self.apply_expected(expr, &unified);
                    }
                    None => {
                        return Err(self.error(
                            expr.span,
                            TypeCheckKind::TypeMismatch {
                                expected: ret_ty.clone(),
                                found: expr_ty,
                            },
                        ));
                    }
                }
            } else {
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
                } else {
                    match self.unify(ty, &val_ty) {
                        Some(unified) => {
                            *ty = unified.clone();
                            self.apply_expected(value, &unified);
                        }
                        None => {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: ty.clone(),
                                    found: val_ty,
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
                let target_ty = locals
                    .get(target)
                    .cloned()
                    .ok_or_else(|| self.error(*span, TypeCheckKind::UndefinedSymbol(*target)))?;
                self.apply_expected(value, &target_ty);
                let val_ty = self.check_expr(value, locals)?;
                if self.unify(&target_ty, &val_ty).is_none() {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: target_ty,
                            found: val_ty,
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
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: Type::Bool,
                            found: cond_ty,
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
                }
            }
            ExprKind::Call { func, args } => {
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
                    match self.unify(expected, &got) {
                        Some(unified) => {
                            self.apply_expected(arg, &unified);
                        }
                        None => {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: expected.clone(),
                                    found: got,
                                },
                            ));
                        }
                    }
                }
                let ret_ty = ret.clone().unwrap_or(Type::Undetermined);
                *ty = ret_ty.clone();
                Ok(ret_ty)
            }
            ExprKind::StructLit { struct_id, fields } => {
                for (field_sym, val) in fields {
                    let expected = {
                        let map = self.struct_fields.get(struct_id).ok_or_else(|| {
                            self.error(*span, TypeCheckKind::UnknownStruct(*struct_id))
                        })?;
                        map.get(field_sym).cloned().ok_or_else(|| {
                            self.error(
                                *span,
                                TypeCheckKind::UnknownField {
                                    struct_id: *struct_id,
                                    field: *field_sym,
                                },
                            )
                        })?
                    };
                    self.apply_expected(val, &expected);
                    let got = self.check_expr(val, locals)?;
                    match self.unify(&expected, &got) {
                        Some(unified) => {
                            self.apply_expected(val, &unified);
                        }
                        None => {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected,
                                    found: got,
                                },
                            ));
                        }
                    }
                }
                let struct_ty = Type::Struct(*struct_id);
                *ty = struct_ty.clone();
                Ok(struct_ty)
            }
            ExprKind::Field { base, field } => {
                let base_ty = self.check_expr(base, locals)?;
                let struct_id = match base_ty {
                    Type::Struct(id) => id,
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
                *ty = field_ty.clone();
                Ok(field_ty.clone())
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
                                return Err(self.error(
                                    *span,
                                    TypeCheckKind::TypeMismatch {
                                        expected: existing.clone(),
                                        found: t,
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
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: Type::Bool,
                            found: cond_ty,
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
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: t,
                                    found: e,
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
            ExprKind::Error => Ok(Type::Undetermined),
        }
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
            (Type::Struct(x), Type::Struct(y)) if x == y => Some(a.clone()),
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
            ExprKind::StructLit { fields, .. } => {
                for (_, val) in fields {
                    self.finalize_expr(val);
                }
            }
            ExprKind::Field { base, .. } => {
                self.finalize_expr(base);
            }
            ExprKind::Deref(base) => {
                self.finalize_expr(base);
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
                | Type::Struct(_)
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
