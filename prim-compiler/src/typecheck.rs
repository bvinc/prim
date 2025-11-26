use crate::program::Program;
use prim_parse::{BinaryOp, Expr, Function, Program as AstProgram, Stmt, Type, TypeCheckError};
use std::collections::{HashMap, HashSet};

/// Multi-module type checker that mirrors the existing single-source checker but
/// operates over the structured `Program` (modules/files) without concatenating
/// source text.
pub fn type_check_program(program: &mut Program) -> Result<(), TypeCheckError> {
    let mut checker = TypeChecker::new();

    // First pass: collect trait signatures from all modules.
    for module in &program.modules {
        for file in &module.files {
            checker.set_source(&file.source);
            checker.collect_traits(&file.ast)?;
        }
    }

    // Validate impls across modules.
    for module in &program.modules {
        for file in &module.files {
            checker.set_source(&file.source);
            checker.validate_impls(&file.ast)?;
        }
    }

    // Collect function signatures (all modules).
    for module in &program.modules {
        for file in &module.files {
            checker.set_source(&file.source);
            checker.collect_function_signatures(&file.ast);
        }
    }

    // Type check bodies, mutating ASTs in place.
    for module in &mut program.modules {
        for file in &mut module.files {
            checker.set_source(&file.source);
            checker.check_functions(&mut file.ast)?;
        }
    }

    Ok(())
}

#[derive(Clone, Debug)]
struct FunctionRecord {
    params: Option<Vec<Type>>,
    return_type: Option<Type>,
}

type TraitMethods = HashMap<String, (Vec<Type>, Option<Type>)>;

struct TypeChecker {
    variables: HashMap<String, Type>,
    functions: HashMap<String, FunctionRecord>,
    traits: HashMap<String, TraitMethods>,
    seen_impls: HashSet<(String, String)>,
    loop_depth: usize,
    source: String,
    struct_names: HashMap<(usize, usize), String>,
}

impl TypeChecker {
    fn new() -> Self {
        let mut checker = Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            traits: HashMap::new(),
            seen_impls: HashSet::new(),
            loop_depth: 0,
            source: String::new(),
            struct_names: HashMap::new(),
        };

        // Built-ins
        checker.functions.insert(
            "println".to_string(),
            FunctionRecord {
                params: None,
                return_type: None,
            },
        );
        checker.functions.insert(
            "std__mem__copy".to_string(),
            FunctionRecord {
                params: None,
                return_type: Some(Type::Usize),
            },
        );
        checker.functions.insert(
            "std__mem__move".to_string(),
            FunctionRecord {
                params: None,
                return_type: Some(Type::Usize),
            },
        );
        checker.functions.insert(
            "std__mem__set".to_string(),
            FunctionRecord {
                params: None,
                return_type: Some(Type::Usize),
            },
        );
        checker.functions.insert(
            "std__mem__len".to_string(),
            FunctionRecord {
                params: None,
                return_type: Some(Type::Usize),
            },
        );

        checker
    }

    fn set_source(&mut self, source: &str) {
        self.source.clear();
        self.source.push_str(source);
    }

    fn register_structs(&mut self, program: &AstProgram) {
        for s in &program.structs {
            self.struct_names.insert(
                (s.name.start(), s.name.end()),
                s.name.text(&self.source).to_string(),
            );
            for field in &s.fields {
                self.record_type(&field.field_type);
            }
        }
        for func in &program.functions {
            for p in &func.parameters {
                self.record_type(&p.type_annotation);
            }
            if let Some(ret) = &func.return_type {
                self.record_type(ret);
            }
            for stmt in &func.body {
                if let Stmt::Let {
                    type_annotation: Some(ty),
                    ..
                } = stmt
                {
                    self.record_type(ty);
                }
            }
        }
        for tr in &program.traits {
            for m in &tr.methods {
                for p in &m.parameters {
                    self.record_type(&p.type_annotation);
                }
                if let Some(ret) = &m.return_type {
                    self.record_type(ret);
                }
            }
        }
        for im in &program.impls {
            self.record_type(&Type::Struct(im.struct_name));
            for m in &im.methods {
                for p in &m.parameters {
                    self.record_type(&p.type_annotation);
                }
                if let Some(ret) = &m.return_type {
                    self.record_type(ret);
                }
            }
        }
    }

    fn record_type(&mut self, ty: &Type) {
        match ty {
            Type::Struct(span) => {
                self.struct_names
                    .entry((span.start(), span.end()))
                    .or_insert_with(|| {
                        span.checked_text(&self.source)
                            .unwrap_or_default()
                            .to_string()
                    });
            }
            Type::Array(inner) => self.record_type(inner),
            Type::Pointer { pointee, .. } => self.record_type(pointee),
            _ => {}
        }
    }

    fn struct_name(&self, span: prim_parse::Span) -> String {
        if let Some(name) = self.struct_names.get(&(span.start(), span.end())) {
            return name.clone();
        }
        span.checked_text(&self.source)
            .unwrap_or_default()
            .to_string()
    }

    fn collect_traits(&mut self, program: &AstProgram) -> Result<(), TypeCheckError> {
        self.register_structs(program);
        for tr in &program.traits {
            let tname = tr.name.text(&self.source).to_string();
            let mut ms = HashMap::new();
            for m in &tr.methods {
                let mname = m.name.text(&self.source).to_string();
                let params = m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.clone())
                    .collect::<Vec<_>>();
                ms.insert(mname, (params, m.return_type.clone()));
            }
            self.traits.insert(tname, ms);
        }
        Ok(())
    }

    fn validate_impls(&mut self, program: &AstProgram) -> Result<(), TypeCheckError> {
        self.register_structs(program);
        for im in &program.impls {
            let tname = im.trait_name.text(&self.source).to_string();
            let sname = im.struct_name.text(&self.source).to_string();
            if !self.traits.contains_key(&tname) {
                return Err(TypeCheckError::UnknownTrait(tname));
            }
            let key = (tname.clone(), sname.clone());
            if !self.seen_impls.insert(key) {
                return Err(TypeCheckError::DuplicateImpl {
                    trait_name: tname,
                    struct_name: sname,
                });
            }
            let trait_methods = &self.traits[&tname];
            let mut impl_methods: HashMap<String, (Vec<Type>, Option<Type>)> = HashMap::new();
            for m in &im.methods {
                let mname = m.name.text(&self.source).to_string();
                let params = m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.clone())
                    .collect::<Vec<_>>();
                impl_methods.insert(mname, (params, m.return_type.clone()));
            }
            for (mname, (tparams, tret)) in trait_methods.iter() {
                match impl_methods.get(mname.as_str()) {
                    None => {
                        return Err(TypeCheckError::MissingImplMethod {
                            trait_name: tname.clone(),
                            struct_name: sname.clone(),
                            method: mname.clone(),
                        });
                    }
                    Some((iparams, iret)) => {
                        let params_match = iparams.len() == tparams.len()
                            && iparams
                                .iter()
                                .zip(tparams.iter())
                                .all(|(a, b)| self.types_equal(a, b));
                        let returns_match = match (iret, tret) {
                            (None, None) => true,
                            (Some(a), Some(b)) => self.types_equal(a, b),
                            _ => false,
                        };
                        if !params_match || !returns_match {
                            return Err(TypeCheckError::MethodSignatureMismatch {
                                trait_name: tname.clone(),
                                struct_name: sname.clone(),
                                method: mname.clone(),
                            });
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn collect_function_signatures(&mut self, program: &AstProgram) {
        self.register_structs(program);
        for func in &program.functions {
            let func_name = func.name.text(&self.source);
            let params = func
                .parameters
                .iter()
                .map(|p| p.type_annotation.clone())
                .collect::<Vec<_>>();
            self.functions.insert(
                func_name.to_string(),
                FunctionRecord {
                    params: Some(params),
                    return_type: func.return_type.clone(),
                },
            );
        }
    }

    fn check_functions(&mut self, program: &mut AstProgram) -> Result<(), TypeCheckError> {
        self.register_structs(program);
        for func in &mut program.functions {
            self.check_function(func)?;
        }
        Ok(())
    }

    fn check_function(&mut self, function: &mut Function) -> Result<(), TypeCheckError> {
        self.variables.clear();
        self.loop_depth = 0;

        for param in &function.parameters {
            let param_name = param.name.text(&self.source);
            self.variables
                .insert(param_name.to_string(), param.type_annotation.clone());
        }

        for stmt in &mut function.body {
            self.check_statement(stmt)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, stmt: &mut Stmt) -> Result<(), TypeCheckError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                let mut inferred_type = self.check_expression(value)?;
                if let Some(annotation) = type_annotation {
                    let ann_clone = annotation.clone();
                    // Allow literal inference to honor the annotation.
                    match (&ann_clone, value) {
                        (ann, v) if is_integer_type(ann) => {
                            apply_expected_argument_type(v, ann);
                            inferred_type = ann.clone();
                        }
                        (Type::Array(elem_ann), Expr::ArrayLiteral { elements, .. }) => {
                            for elem in elements {
                                apply_expected_argument_type(elem, elem_ann);
                            }
                            inferred_type = ann_clone.clone();
                        }
                        _ => {}
                    }
                    if !self.types_equal(annotation, &inferred_type) {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: annotation.clone(),
                            found: inferred_type,
                        });
                    }
                } else {
                    *type_annotation = Some(inferred_type.clone());
                }
                self.variables
                    .insert(name.text(&self.source).to_string(), inferred_type);
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }
            Stmt::Loop { body, .. } => {
                self.loop_depth += 1;
                for stmt in body {
                    self.check_statement(stmt)?;
                }
                self.loop_depth -= 1;
                Ok(())
            }
            Stmt::Break { span } => {
                if self.loop_depth == 0 {
                    Err(TypeCheckError::BreakOutsideLoop { span: *span })
                } else {
                    Ok(())
                }
            }
        }
    }

    fn check_expression(&mut self, expr: &mut Expr) -> Result<Type, TypeCheckError> {
        match expr {
            Expr::IntLiteral { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::I64;
                }
                Ok(ty.clone())
            }
            Expr::FloatLiteral { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::F64;
                }
                Ok(ty.clone())
            }
            Expr::BoolLiteral { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::Bool;
                }
                Ok(ty.clone())
            }
            Expr::StringLiteral { ty, .. } => {
                if matches!(ty, Type::Undetermined) {
                    *ty = Type::StrSlice;
                }
                Ok(ty.clone())
            }
            Expr::Identifier { span, .. } => self
                .variables
                .get(span.text(&self.source))
                .cloned()
                .or_else(|| {
                    self.functions
                        .get(span.text(&self.source))
                        .and_then(|rec| rec.return_type.clone())
                })
                .ok_or_else(|| {
                    TypeCheckError::UndefinedVariable(span.text(&self.source).to_string())
                }),
            Expr::Binary {
                left,
                op,
                right,
                ty,
            } => {
                let left_ty = self.check_expression(left)?;
                let right_ty = self.check_expression(right)?;
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if !is_numeric_type(&left_ty) || !is_numeric_type(&right_ty) {
                            return Err(TypeCheckError::InvalidBinaryOperands {
                                op: op.clone(),
                                left: left_ty,
                                right: right_ty,
                            });
                        }
                        if !self.types_equal(&left_ty, &right_ty) {
                            return Err(TypeCheckError::InvalidBinaryOperands {
                                op: op.clone(),
                                left: left_ty,
                                right: right_ty,
                            });
                        }
                        *ty = left_ty.clone();
                        Ok(ty.clone())
                    }
                    BinaryOp::Equals => {
                        if !is_equality_compatible(&left_ty)
                            || !self.types_equal(&left_ty, &right_ty)
                        {
                            return Err(TypeCheckError::InvalidBinaryOperands {
                                op: op.clone(),
                                left: left_ty,
                                right: right_ty,
                            });
                        }
                        *ty = Type::Bool;
                        Ok(Type::Bool)
                    }
                }
            }
            Expr::FunctionCall { path, args, ty } => {
                let func_name = path
                    .segments
                    .last()
                    .map(|s| s.text(&self.source))
                    .unwrap_or("");
                if let Some(record) = self.functions.get(func_name).cloned() {
                    if let Some(expected_params) = &record.params {
                        if expected_params.len() != args.len() {
                            return Err(TypeCheckError::UndefinedFunction(func_name.to_string()));
                        }
                        for (arg, expected) in args.iter_mut().zip(expected_params.iter()) {
                            let inferred = self.check_expression(arg)?;
                            if !self.types_equal(&inferred, expected) {
                                apply_expected_argument_type(arg, expected);
                            }
                        }
                    } else {
                        for arg in args.iter_mut() {
                            self.check_expression(arg)?;
                        }
                    }
                    *ty = record.return_type.clone().unwrap_or(Type::Undetermined);
                    Ok(ty.clone())
                } else {
                    Err(TypeCheckError::UndefinedFunction(func_name.to_string()))
                }
            }
            Expr::StructLiteral { name, fields, ty } => {
                for field in fields {
                    self.check_expression(&mut field.value)?;
                }
                *ty = Type::Struct(*name);
                Ok(Type::Struct(*name))
            }
            Expr::FieldAccess { object, field, ty } => {
                let obj_ty = self.check_expression(object)?;
                match obj_ty {
                    Type::Struct(_) => {
                        *ty = Type::Undetermined;
                        Ok(ty.clone())
                    }
                    other => Err(TypeCheckError::TypeMismatch {
                        expected: Type::Struct(*field),
                        found: other,
                    }),
                }
            }
            Expr::Dereference { operand, ty } => {
                let operand_ty = self.check_expression(operand)?;
                if let Type::Pointer { pointee, .. } = operand_ty {
                    *ty = *pointee;
                    Ok(ty.clone())
                } else {
                    Err(TypeCheckError::InvalidDereference(operand_ty))
                }
            }
            Expr::ArrayLiteral { elements, ty } => {
                let mut element_type: Option<Type> = None;
                for elem in elements {
                    let elem_ty = self.check_expression(elem)?;
                    match &element_type {
                        None => element_type = Some(elem_ty),
                        Some(existing) if self.types_equal(existing, &elem_ty) => {}
                        Some(existing) => {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: existing.clone(),
                                found: elem_ty,
                            });
                        }
                    }
                }
                let arr_ty = Type::Array(Box::new(element_type.unwrap_or(Type::Undetermined)));
                *ty = arr_ty.clone();
                Ok(arr_ty)
            }
        }
    }

    fn types_equal(&self, left: &Type, right: &Type) -> bool {
        match (left, right) {
            (Type::Struct(a), Type::Struct(b)) => self.struct_name(*a) == self.struct_name(*b),
            (
                Type::Pointer {
                    mutability: mut_a,
                    pointee: pointee_a,
                },
                Type::Pointer {
                    mutability: mut_b,
                    pointee: pointee_b,
                },
            ) => mut_a == mut_b && self.types_equal(pointee_a.as_ref(), pointee_b.as_ref()),
            (Type::Array(a), Type::Array(b)) => self.types_equal(a.as_ref(), b.as_ref()),
            _ => left == right,
        }
    }
}

fn is_equality_compatible(ty: &Type) -> bool {
    matches!(
        ty,
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

fn apply_expected_argument_type(arg: &mut Expr, expected: &Type) {
    match (arg, expected) {
        (Expr::IntLiteral { ty, .. }, exp) if is_integer_type(exp) => {
            *ty = exp.clone();
        }
        (Expr::FloatLiteral { ty, .. }, Type::F32) => {
            *ty = Type::F32;
        }
        (Expr::FloatLiteral { ty, .. }, Type::F64) => {
            *ty = Type::F64;
        }
        (Expr::BoolLiteral { ty, .. }, Type::Bool) => {
            *ty = Type::Bool;
        }
        _ => {}
    }
}

fn is_numeric_type(ty: &Type) -> bool {
    matches!(
        ty,
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

fn is_integer_type(ty: &Type) -> bool {
    matches!(
        ty,
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
