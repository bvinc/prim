use crate::{
    BinaryOp, FileId, FuncId, HirExpr, HirFunction, HirProgram, HirStmt, SpanId, StructId,
    SymbolId, Type,
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckError {
    pub file: FileId,
    pub span: SpanId,
    pub kind: TypeCheckKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckKind {
    UndefinedSymbol(SymbolId),
    UnknownFunction(FuncId),
    UnknownStruct(StructId),
    UnknownField {
        struct_id: StructId,
        field: SymbolId,
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
    Legacy(String),
}

impl TypeCheckError {
    pub fn error_code(&self) -> &'static str {
        match self.kind {
            TypeCheckKind::UndefinedSymbol(_) => "TYP001",
            TypeCheckKind::UnknownFunction(_) => "TYP002",
            TypeCheckKind::TypeMismatch { .. } => "TYP003",
            TypeCheckKind::InvalidBinaryOperands { .. } => "TYP004",
            TypeCheckKind::InvalidDereference(_) => "TYP005",
            TypeCheckKind::UnknownStruct(_) => "TYP006",
            TypeCheckKind::UnknownField { .. } => "TYP007",
            TypeCheckKind::ArityMismatch { .. } => "TYP008",
            TypeCheckKind::BreakOutsideLoop => "TYP010",
            TypeCheckKind::Legacy(_) => "TYP999",
        }
    }

    pub fn category(&self) -> &'static str {
        "type checking"
    }

    pub fn position(&self) -> Option<usize> {
        Some(self.span.0 as usize)
    }

    pub fn context(&self) -> Option<&str> {
        None
    }
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
                write!(
                    f,
                    "Type mismatch: expected {:?}, found {:?}",
                    expected, found
                )
            }
            TypeCheckKind::InvalidBinaryOperands { op, left, right } => write!(
                f,
                "Invalid operands for {:?}: left {:?}, right {:?}",
                op, left, right
            ),
            TypeCheckKind::InvalidDereference(ty) => {
                write!(f, "Invalid dereference of type {:?}", ty)
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
            TypeCheckKind::Legacy(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for TypeCheckError {}

pub fn type_check(program: &mut HirProgram) -> Result<(), TypeCheckError> {
    let mut checker = Checker::new(program);
    checker.collect_signatures();
    checker.check_functions()
}

struct Checker<'a> {
    program: &'a mut HirProgram,
    func_sigs: HashMap<FuncId, (Vec<Type>, Option<Type>)>,
    struct_fields: HashMap<StructId, HashMap<SymbolId, Type>>,
    loop_depth: usize,
}

impl<'a> Checker<'a> {
    fn new(program: &'a mut HirProgram) -> Self {
        Self {
            program,
            func_sigs: HashMap::new(),
            struct_fields: HashMap::new(),
            loop_depth: 0,
        }
    }

    fn collect_signatures(&mut self) {
        for s in &self.program.items.structs {
            let mut fields = HashMap::new();
            for f in &s.fields {
                fields.insert(f.name, f.ty.clone());
            }
            self.struct_fields.insert(s.id, fields);
        }

        for f in &self.program.items.functions {
            let params = f.params.iter().map(|p| p.ty.clone()).collect();
            self.func_sigs.insert(f.id, (params, f.ret.clone()));
        }
    }

    fn check_functions(&mut self) -> Result<(), TypeCheckError> {
        let mut funcs = std::mem::take(&mut self.program.items.functions);
        for func in funcs.iter_mut() {
            self.check_function(func)?;
        }
        self.program.items.functions = funcs;
        Ok(())
    }

    fn check_function(&mut self, func: &mut HirFunction) -> Result<(), TypeCheckError> {
        let mut locals = HashMap::new();
        for p in &func.params {
            locals.insert(p.name, p.ty.clone());
        }
        self.loop_depth = 0;
        for stmt in &mut func.body.stmts {
            self.check_stmt(stmt, &mut locals)?;
        }
        Ok(())
    }

    fn check_stmt(
        &mut self,
        stmt: &mut HirStmt,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<(), TypeCheckError> {
        match stmt {
            HirStmt::Let {
                name,
                ty,
                value,
                span,
            } => {
                let val_ty = self.check_expr(value, locals)?;
                if matches!(ty, Type::Undetermined)
                    || matches!(ty, Type::Struct(id) if id.0 == u32::MAX)
                {
                    *ty = val_ty.clone();
                } else if !self.types_equal(ty, &val_ty) {
                    return Err(self.error(
                        *span,
                        TypeCheckKind::TypeMismatch {
                            expected: ty.clone(),
                            found: val_ty,
                        },
                    ));
                }
                locals.insert(*name, ty.clone());
                Ok(())
            }
            HirStmt::Expr(expr) => {
                self.check_expr(expr, locals)?;
                Ok(())
            }
            HirStmt::Loop { body, .. } => {
                self.loop_depth += 1;
                for stmt in &mut body.stmts {
                    self.check_stmt(stmt, locals)?;
                }
                self.loop_depth -= 1;
                Ok(())
            }
            HirStmt::Break { span } => {
                if self.loop_depth == 0 {
                    Err(self.error(*span, TypeCheckKind::BreakOutsideLoop))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn check_expr(
        &mut self,
        expr: &mut HirExpr,
        locals: &mut HashMap<SymbolId, Type>,
    ) -> Result<Type, TypeCheckError> {
        match expr {
            HirExpr::Int { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::I64;
                }
                Ok(ty.clone())
            }
            HirExpr::Float { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::F64;
                }
                Ok(ty.clone())
            }
            HirExpr::Bool { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::Bool;
                }
                Ok(ty.clone())
            }
            HirExpr::Str { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::StrSlice;
                }
                Ok(ty.clone())
            }
            HirExpr::Ident {
                symbol,
                ty,
                span: _,
            } => {
                if let Some(t) = locals.get(symbol) {
                    *ty = t.clone();
                    Ok(t.clone())
                } else {
                    *ty = Type::Undetermined;
                    Ok(Type::Undetermined)
                }
            }
            HirExpr::Binary {
                op,
                left,
                right,
                ty,
                span,
            } => {
                let l = self.check_expr(left, locals)?;
                let r = self.check_expr(right, locals)?;
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if !self.is_numeric(&l) || !self.is_numeric(&r) || !self.types_equal(&l, &r)
                        {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::InvalidBinaryOperands {
                                    op: *op,
                                    left: l,
                                    right: r,
                                },
                            ));
                        }
                        *ty = l.clone();
                        Ok(l)
                    }
                    BinaryOp::Equals => {
                        if !self.types_equal(&l, &r) || !self.is_equality_compatible(&l) {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::InvalidBinaryOperands {
                                    op: *op,
                                    left: l,
                                    right: r,
                                },
                            ));
                        }
                        *ty = Type::Bool;
                        Ok(Type::Bool)
                    }
                }
            }
            HirExpr::Call {
                func,
                args,
                ty,
                span,
            } => {
                if func.0 == u32::MAX {
                    *ty = Type::Undetermined;
                    return Ok(Type::Undetermined);
                }
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
                    let got = self.check_expr(arg, locals)?;
                    if matches!(expected, Type::Undetermined)
                        || matches!(expected, Type::Struct(id) if id.0 == u32::MAX)
                    {
                        // accept unknown/undetermined parameter types
                    } else if !self.types_equal(expected, &got) {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected: expected.clone(),
                                found: got,
                            },
                        ));
                    }
                }
                let ret_ty = ret.clone().unwrap_or(Type::Undetermined);
                *ty = ret_ty.clone();
                Ok(ret_ty)
            }
            HirExpr::StructLit {
                struct_id,
                fields,
                ty,
                span,
            } => {
                for (field_sym, val) in fields {
                    if field_sym.0 == u32::MAX {
                        let _ = self.check_expr(val, locals)?;
                        continue;
                    }
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
                    let got = self.check_expr(val, locals)?;
                    if matches!(expected, Type::Undetermined)
                        || matches!(expected, Type::Struct(id) if id.0 == u32::MAX)
                    {
                        // Accept unknown/undetermined struct field types for now.
                    } else if !self.types_equal(&expected, &got) {
                        return Err(self.error(
                            *span,
                            TypeCheckKind::TypeMismatch {
                                expected,
                                found: got,
                            },
                        ));
                    }
                }
                let struct_ty = Type::Struct(*struct_id);
                *ty = struct_ty.clone();
                Ok(struct_ty)
            }
            HirExpr::Field {
                base,
                field,
                ty,
                span,
            } => {
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
                if field.0 == u32::MAX {
                    *ty = Type::Undetermined;
                    return Ok(Type::Undetermined);
                }
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
            HirExpr::Deref { base, ty, span } => {
                let base_ty = self.check_expr(base, locals)?;
                if let Type::Pointer { pointee, .. } = base_ty {
                    *ty = *pointee.clone();
                    Ok((*pointee).clone())
                } else {
                    Err(self.error(*span, TypeCheckKind::InvalidDereference(base_ty)))
                }
            }
            HirExpr::ArrayLit { elements, ty, span } => {
                let mut elem_ty: Option<Type> = None;
                for elem in elements {
                    let t = self.check_expr(elem, locals)?;
                    match &elem_ty {
                        None => elem_ty = Some(t),
                        Some(existing) if self.types_equal(existing, &t) => {}
                        Some(existing) => {
                            return Err(self.error(
                                *span,
                                TypeCheckKind::TypeMismatch {
                                    expected: existing.clone(),
                                    found: t,
                                },
                            ));
                        }
                    }
                }
                let arr = Type::Array(Box::new(elem_ty.unwrap_or(Type::Undetermined)));
                *ty = arr.clone();
                Ok(arr)
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn types_equal(&self, a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::Undetermined, _) | (_, Type::Undetermined) => true,
            (Type::Struct(x), Type::Struct(y)) => x == y || x.0 == u32::MAX || y.0 == u32::MAX,
            (
                Type::Pointer {
                    mutable: ma,
                    pointee: pa,
                },
                Type::Pointer {
                    mutable: mb,
                    pointee: pb,
                },
            ) => ma == mb && self.types_equal(pa, pb),
            (Type::Array(a), Type::Array(b)) => self.types_equal(a, b),
            _ => a == b,
        }
    }

    fn is_numeric(&self, t: &Type) -> bool {
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
                | Type::F32
                | Type::F64
        )
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
                | Type::Pointer { .. }
                | Type::Struct(_)
        )
    }

    fn error(&self, span: SpanId, kind: TypeCheckKind) -> TypeCheckError {
        let file = self
            .program
            .span_files
            .get(span.0 as usize)
            .copied()
            .unwrap_or(FileId(u32::MAX));
        TypeCheckError { file, span, kind }
    }
}
