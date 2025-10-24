use crate::{BinaryOp, Expr, Function, Program, Stmt, Type};
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Clone, Debug)]
struct FunctionRecord {
    params: Option<Vec<Type>>,
    return_type: Option<Type>,
}

#[derive(Debug)]
pub struct TypeChecker {
    /// Variable name -> Type mapping for current scope
    variables: HashMap<String, Type>,
    /// Function name -> return type mapping
    functions: HashMap<String, FunctionRecord>,
    /// Source text for span lookups
    source: String,
    #[allow(clippy::type_complexity)]
    traits: HashMap<String, HashMap<String, (Vec<Type>, Option<Type>)>>,
    seen_impls: HashSet<(String, String)>,
}

#[derive(Debug)]
pub enum TypeCheckError {
    UndefinedVariable(String),
    UndefinedFunction(String),
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
    UnknownTrait(String),
    DuplicateImpl {
        trait_name: String,
        struct_name: String,
    },
    MissingImplMethod {
        trait_name: String,
        struct_name: String,
        method: String,
    },
    MethodSignatureMismatch {
        trait_name: String,
        struct_name: String,
        method: String,
    },
}

impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeCheckError::UndefinedVariable(name) => {
                write!(f, "Undefined variable '{}'", name)
            }
            TypeCheckError::UndefinedFunction(name) => {
                write!(f, "Undefined function '{}'", name)
            }
            TypeCheckError::TypeMismatch { expected, found } => {
                write!(
                    f,
                    "Type mismatch: expected {}, found {}",
                    type_name(expected),
                    type_name(found)
                )
            }
            TypeCheckError::InvalidBinaryOperands { op, left, right } => {
                write!(
                    f,
                    "Invalid binary operands for '{}' (left: {}, right: {})",
                    binary_op_symbol(op),
                    type_name(left),
                    type_name(right)
                )
            }
            TypeCheckError::InvalidDereference(ty) => {
                write!(f, "Cannot dereference value of type {}", type_name(ty))
            }
            TypeCheckError::UnknownTrait(name) => {
                write!(f, "Unknown trait '{}'", name)
            }
            TypeCheckError::DuplicateImpl {
                trait_name,
                struct_name,
            } => {
                write!(
                    f,
                    "Duplicate impl of trait '{}' for struct '{}'",
                    trait_name, struct_name
                )
            }
            TypeCheckError::MissingImplMethod {
                trait_name,
                struct_name,
                method,
            } => {
                write!(
                    f,
                    "Missing method '{}' in impl of trait '{}' for struct '{}'",
                    method, trait_name, struct_name
                )
            }
            TypeCheckError::MethodSignatureMismatch {
                trait_name,
                struct_name,
                method,
            } => {
                write!(
                    f,
                    "Signature mismatch for method '{}' in impl of trait '{}' for struct '{}'",
                    method, trait_name, struct_name
                )
            }
        }
    }
}

impl std::error::Error for TypeCheckError {}

impl TypeCheckError {
    pub fn error_code(&self) -> &'static str {
        match self {
            TypeCheckError::UndefinedVariable(_) => "TYP001",
            TypeCheckError::UndefinedFunction(_) => "TYP002",
            TypeCheckError::TypeMismatch { .. } => "TYP003",
            TypeCheckError::InvalidBinaryOperands { .. } => "TYP004",
            TypeCheckError::InvalidDereference(_) => "TYP005",
            TypeCheckError::UnknownTrait(_) => "TYP006",
            TypeCheckError::DuplicateImpl { .. } => "TYP007",
            TypeCheckError::MissingImplMethod { .. } => "TYP008",
            TypeCheckError::MethodSignatureMismatch { .. } => "TYP009",
        }
    }

    pub fn category(&self) -> &'static str {
        match self {
            TypeCheckError::UnknownTrait(_)
            | TypeCheckError::DuplicateImpl { .. }
            | TypeCheckError::MissingImplMethod { .. }
            | TypeCheckError::MethodSignatureMismatch { .. } => "Trait checking",
            _ => "Type checking",
        }
    }

    pub fn position(&self) -> Option<usize> {
        None
    }

    pub fn context(&self) -> Option<&'static str> {
        match self {
            TypeCheckError::InvalidBinaryOperands { .. } => Some("binary expression"),
            TypeCheckError::TypeMismatch { .. } => Some("type compatibility"),
            TypeCheckError::InvalidDereference(_) => Some("dereference"),
            TypeCheckError::UndefinedVariable(_) => Some("name resolution"),
            TypeCheckError::UndefinedFunction(_) => Some("name resolution"),
            TypeCheckError::UnknownTrait(_) => Some("trait lookup"),
            TypeCheckError::DuplicateImpl { .. } => Some("trait implementation"),
            TypeCheckError::MissingImplMethod { .. } => Some("trait implementation"),
            TypeCheckError::MethodSignatureMismatch { .. } => Some("trait implementation"),
        }
    }
}

impl TypeChecker {
    fn types_equal(&self, left: &Type, right: &Type) -> bool {
        match (left, right) {
            (Type::Struct(a), Type::Struct(b)) => a.text(&self.source) == b.text(&self.source),
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

    pub fn new(source: &str) -> Self {
        let mut checker = Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            source: source.to_string(),
            traits: HashMap::new(),
            seen_impls: HashSet::new(),
        };

        // Add built-in functions
        checker.functions.insert(
            "println".to_string(),
            FunctionRecord {
                params: None,
                return_type: None,
            },
        );
        // Seed intrinsic std.mem functions (runtime-provided)
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

    pub fn check_program(&mut self, mut program: Program) -> Result<Program, TypeCheckError> {
        // Index trait method signatures
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

        // Validate impls
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
                        if iparams != tparams || iret != tret {
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
        // First pass: collect function signatures
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

        // Second pass: type check each function (mutating in place)
        for func in &mut program.functions {
            self.check_function(func)?;
        }

        Ok(program)
    }

    fn check_function(&mut self, function: &mut Function) -> Result<(), TypeCheckError> {
        // Enter function scope - clear variables and add parameters
        self.variables.clear();

        // Add parameters to variable scope
        for param in &function.parameters {
            let param_name = param.name.text(&self.source);
            self.variables
                .insert(param_name.to_string(), param.type_annotation.clone());
        }

        // Type check function body
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
                // If there's a type annotation, use it to guide type checking
                if let Some(annotated_type) = type_annotation {
                    // If the annotated type is an integer, set integer literal expectation
                    if let Expr::IntLiteral { ty, .. } = value {
                        if matches!(
                            annotated_type,
                            Type::I8
                                | Type::I16
                                | Type::I32
                                | Type::I64
                                | Type::U8
                                | Type::U16
                                | Type::U32
                                | Type::U64
                                | Type::Isize
                                | Type::Usize
                        ) {
                            *ty = annotated_type.clone();
                        }
                    }
                    // If the annotated type is an array, push element expectations into array literal
                    if let Type::Array(elem_ty) = annotated_type {
                        if let Expr::ArrayLiteral { elements, .. } = value {
                            for el in elements.iter_mut() {
                                if let Expr::IntLiteral { ty, .. } = el {
                                    *ty = *elem_ty.clone();
                                }
                            }
                        }
                    }
                }

                let value_type = self.check_expression(value)?;

                // If there's a type annotation, verify compatibility
                if let Some(annotated_type) = type_annotation {
                    if !self.types_equal(annotated_type, &value_type) {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: annotated_type.clone(),
                            found: value_type.clone(),
                        });
                    }
                }

                // Add variable to scope with inferred or annotated type
                let var_name = name.text(&self.source);
                let var_type = type_annotation.clone().unwrap_or(value_type);
                self.variables.insert(var_name.to_string(), var_type);

                Ok(())
            }
            Stmt::Expr(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }
        }
    }

    fn check_expression(&mut self, expr: &mut Expr) -> Result<Type, TypeCheckError> {
        let ty = match expr {
            Expr::IntLiteral { ty, .. } => {
                // Use existing type if already set, otherwise default to I64
                if *ty == Type::Undetermined {
                    let inferred_type = Type::I64; // Default integer type
                    *ty = inferred_type.clone();
                    inferred_type
                } else {
                    ty.clone()
                }
            }
            Expr::FloatLiteral { ty, .. } => {
                let inferred_type = Type::F64;
                *ty = inferred_type.clone();
                inferred_type
            }
            Expr::BoolLiteral { ty, .. } => {
                let inferred_type = Type::Bool;
                *ty = inferred_type.clone();
                inferred_type
            }
            Expr::StringLiteral { ty, .. } => {
                // String literals have slice representation (ptr, len)
                let inferred_type = Type::StrSlice;
                *ty = inferred_type.clone();
                inferred_type
            }

            Expr::Identifier { span, ty } => {
                let name = span.text(&self.source);
                let var_type = self
                    .variables
                    .get(name)
                    .cloned()
                    .ok_or_else(|| TypeCheckError::UndefinedVariable(name.to_string()))?;
                *ty = var_type.clone();
                var_type
            }

            Expr::FunctionCall { path, args, ty } => {
                // Build function key: join path with double underscores (module__function)
                let original_name = path.mangle(&self.source, "__");
                let (param_types, return_type_opt) = if let Some(record) = self
                    .functions
                    .get(&original_name)
                    .map(|record| (record.params.clone(), record.return_type.clone()))
                {
                    record
                } else if let Some(record) = path
                    .segments
                    .last()
                    .map(|seg| seg.text(&self.source).to_string())
                    .and_then(|name| {
                        self.functions
                            .get(&name)
                            .map(|r| (r.params.clone(), r.return_type.clone()))
                    })
                {
                    record
                } else {
                    return Err(TypeCheckError::UndefinedFunction(original_name));
                };

                if let Some(param_types) = param_types {
                    let expected_len = param_types.len();
                    for (arg, expected_type) in args.iter_mut().zip(param_types.iter()) {
                        if matches!(expected_type, Type::Struct(_) | Type::Array(_)) {
                            self.check_expression(arg)?;
                            continue;
                        }
                        apply_expected_argument_type(arg, expected_type);
                        let arg_type = self.check_expression(arg)?;
                        if !self.types_equal(&arg_type, expected_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: expected_type.clone(),
                                found: arg_type,
                            });
                        }
                    }
                    if args.len() > expected_len {
                        for arg in args.iter_mut().skip(expected_len) {
                            self.check_expression(arg)?;
                        }
                    }
                } else {
                    for arg in args {
                        self.check_expression(arg)?;
                    }
                }

                let return_type = return_type_opt.unwrap_or(Type::I64);
                *ty = return_type.clone();
                return_type
            }

            Expr::Binary {
                left,
                op,
                right,
                ty,
            } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if !is_numeric_type(&left_type) || !is_numeric_type(&right_type) {
                            return Err(TypeCheckError::InvalidBinaryOperands {
                                op: op.clone(),
                                left: left_type,
                                right: right_type,
                            });
                        }
                        if !self.types_equal(&left_type, &right_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: left_type.clone(),
                                found: right_type,
                            });
                        }

                        *ty = left_type.clone();
                        left_type
                    }
                    BinaryOp::Equals => {
                        if !self.types_equal(&left_type, &right_type) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: left_type.clone(),
                                found: right_type,
                            });
                        }
                        if !is_equality_compatible(&left_type) {
                            return Err(TypeCheckError::InvalidBinaryOperands {
                                op: op.clone(),
                                left: left_type.clone(),
                                right: right_type,
                            });
                        }

                        *ty = Type::Bool;
                        Type::Bool
                    }
                }
            }

            Expr::Dereference { operand, ty } => {
                let operand_type = self.check_expression(operand)?;

                match operand_type {
                    Type::Pointer { pointee, .. } => {
                        let pointee_type = *pointee;
                        *ty = pointee_type.clone();
                        pointee_type
                    }
                    other => return Err(TypeCheckError::InvalidDereference(other)),
                }
            }

            // For other expressions, return a placeholder type for now
            Expr::StructLiteral { name, fields, ty } => {
                for field in fields.iter_mut() {
                    self.check_expression(&mut field.value)?;
                }
                let struct_type = Type::Struct(*name);
                *ty = struct_type.clone();
                struct_type
            }
            Expr::FieldAccess { object, ty, .. } => {
                self.check_expression(object)?;
                let placeholder_type = Type::I64; // TODO: implement field type checking
                *ty = placeholder_type.clone();
                placeholder_type
            }
            Expr::ArrayLiteral { elements, ty } => {
                // Infer array element type from elements; default to i64 if empty
                // Note: precise inference is limited; for now, use i64 unless prior expectation set
                for el in elements.iter_mut() {
                    self.check_expression(el)?;
                }
                // Use first element type if available, else default to i64
                let elem_ty = if let Some(first) = elements.first() {
                    match first.clone() {
                        Expr::IntLiteral { ty, .. }
                        | Expr::FloatLiteral { ty, .. }
                        | Expr::BoolLiteral { ty, .. }
                        | Expr::StringLiteral { ty, .. }
                        | Expr::Identifier { ty, .. }
                        | Expr::Binary { ty, .. }
                        | Expr::FunctionCall { ty, .. }
                        | Expr::StructLiteral { ty, .. }
                        | Expr::FieldAccess { ty, .. }
                        | Expr::Dereference { ty, .. }
                        | Expr::ArrayLiteral { ty, .. } => ty,
                    }
                } else {
                    Type::I64
                };
                let arr_ty = Type::Array(Box::new(elem_ty));
                *ty = arr_ty.clone();
                arr_ty
            }
        };

        Ok(ty)
    }
}

fn binary_op_symbol(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Equals => "==",
    }
}

fn type_name(ty: &Type) -> &'static str {
    match ty {
        Type::U8 => "u8",
        Type::I8 => "i8",
        Type::U16 => "u16",
        Type::I16 => "i16",
        Type::U32 => "u32",
        Type::I32 => "i32",
        Type::U64 => "u64",
        Type::I64 => "i64",
        Type::Usize => "usize",
        Type::Isize => "isize",
        Type::F32 => "f32",
        Type::F64 => "f64",
        Type::Bool => "bool",
        Type::StrSlice => "str",
        Type::Array(_) => "array",
        Type::Struct(_) => "struct",
        Type::Pointer { .. } => "pointer",
        Type::Undetermined => "undetermined",
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

pub fn type_check(program: Program, source: &str) -> Result<Program, TypeCheckError> {
    let mut checker = TypeChecker::new(source);
    checker.check_program(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Type, parse};

    #[test]
    fn test_basic_variable_typing() {
        let source = "fn main() { let x: i32 = 42; let y = x }";
        let program = parse(source).unwrap();
        let typed = type_check(program, source).unwrap();

        assert_eq!(typed.functions.len(), 1);
    }

    #[test]
    fn test_type_checking_fills_expr_types() {
        let source = "fn main() { let x = 42 }";
        let program = parse(source).unwrap();

        // Before type checking - expression should have Type::Undetermined
        let main_func = &program.functions[0];
        if let crate::Stmt::Let { value, .. } = &main_func.body[0] {
            if let crate::Expr::IntLiteral { ty, .. } = value {
                assert_eq!(*ty, Type::Undetermined);
            } else {
                panic!("Expected IntLiteral");
            }
        } else {
            panic!("Expected Let statement");
        }

        let typed = type_check(program, source).unwrap();

        // After type checking - expression should have concrete type
        let main_func = &typed.functions[0];
        if let crate::Stmt::Let { value, .. } = &main_func.body[0] {
            if let crate::Expr::IntLiteral { ty, .. } = value {
                assert_eq!(*ty, Type::I64);
            } else {
                panic!("Expected IntLiteral");
            }
        } else {
            panic!("Expected Let statement");
        }
    }

    #[test]
    fn test_function_parameter_typing() {
        let source = "fn test(x: i32) -> i32 { x } fn main() {}";
        let program = parse(source).unwrap();
        let typed = type_check(program, source).unwrap();

        assert_eq!(typed.functions.len(), 2);
    }

    #[test]
    fn test_equality_expression_is_bool() {
        let source = "fn main() { let flag = 1 == 2 }";
        let program = parse(source).unwrap();
        let typed = type_check(program, source).unwrap();

        let main_func = typed
            .functions
            .iter()
            .find(|f| f.name.text(source) == "main")
            .unwrap();
        if let crate::Stmt::Let { value, .. } = &main_func.body[0] {
            if let crate::Expr::Binary { ty, .. } = value {
                assert_eq!(*ty, Type::Bool);
            } else {
                panic!("Expected binary expression");
            }
        } else {
            panic!("Expected let statement");
        }
    }

    #[test]
    fn test_boolean_arithmetic_is_rejected() {
        let source = "fn main() { let flag = true + false }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        match result {
            Err(TypeCheckError::InvalidBinaryOperands { op, left, right }) => {
                assert_eq!(op, BinaryOp::Add);
                assert_eq!(left, Type::Bool);
                assert_eq!(right, Type::Bool);
            }
            other => panic!("Expected InvalidBinaryOperands error, got {:?}", other),
        }
    }

    #[test]
    fn test_boolean_division_is_rejected() {
        let source = "fn main() { let flag = false / true }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        assert!(matches!(
            result,
            Err(TypeCheckError::InvalidBinaryOperands {
                op: BinaryOp::Divide,
                ..
            })
        ));
    }

    #[test]
    fn test_mixed_type_equality_errors() {
        let source = "fn main() { let flag = true == 1 }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        match result {
            Err(TypeCheckError::TypeMismatch { expected, found }) => {
                assert_eq!(expected, Type::Bool);
                assert_eq!(found, Type::I64);
            }
            other => panic!("Expected TypeMismatch, got {:?}", other),
        }
    }

    #[test]
    fn test_pointer_dereference_typing() {
        let source = "fn test(ptr: *const i32) -> i32 { *ptr } fn main() {}";
        let program = parse(source).unwrap();

        assert_eq!(program.functions.len(), 2);

        // Before type checking - dereference should have Type::Undetermined
        let test_func = program
            .functions
            .iter()
            .find(|f| f.name.text(source) == "test")
            .unwrap();
        if let crate::Stmt::Expr(expr) = &test_func.body[0] {
            if let crate::Expr::Dereference { ty, .. } = expr {
                assert_eq!(*ty, Type::Undetermined);
            } else {
                panic!("Expected dereference expression");
            }
        } else {
            panic!("Expected expression statement");
        }

        let typed = type_check(program, source).unwrap();

        // After type checking - dereference should now have type i32
        let test_func = typed
            .functions
            .iter()
            .find(|f| f.name.text(source) == "test")
            .unwrap();
        if let crate::Stmt::Expr(expr) = &test_func.body[0] {
            if let crate::Expr::Dereference { ty, .. } = expr {
                // The dereference should now have type i32 (extracted from *const i32)
                assert_eq!(*ty, Type::I32);
            } else {
                panic!("Expected dereference expression");
            }
        } else {
            panic!("Expected expression statement");
        }
    }

    #[test]
    fn test_undefined_variable_error() {
        let source = "fn main() { let x = y }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        assert!(matches!(result, Err(TypeCheckError::UndefinedVariable(_))));
    }

    #[test]
    fn test_invalid_dereference_error() {
        let source = "fn main() { let x = 42; let y = *x }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        assert!(matches!(result, Err(TypeCheckError::InvalidDereference(_))));
    }

    #[test]
    fn test_trait_impl_happy() {
        let source = r#"
            struct S {}
            trait T { fn f(a: i32) -> i32; }
            impl T for S { fn f(a: i32) -> i32 { a } }
            fn main() {}
        "#;
        let program = parse(source).unwrap();
        let res = type_check(program, source);
        assert!(res.is_ok());
    }

    #[test]
    fn test_trait_impl_missing_method() {
        let source = r#"
            struct S {}
            trait T { fn f(a: i32) -> i32; }
            impl T for S {}
            fn main() {}
        "#;
        let program = parse(source).unwrap();
        let res = type_check(program, source);
        match res {
            Err(TypeCheckError::MissingImplMethod {
                trait_name,
                struct_name,
                method,
            }) => {
                assert_eq!(trait_name, "T");
                assert_eq!(struct_name, "S");
                assert_eq!(method, "f");
            }
            other => panic!("Expected MissingImplMethod, got {:?}", other),
        }
    }

    #[test]
    fn test_trait_impl_signature_mismatch() {
        let source = r#"
            struct S {}
            trait T { fn f(a: i32) -> i32; }
            impl T for S { fn f(a: i64) -> i32 { 0 } }
            fn main() {}
        "#;
        let program = parse(source).unwrap();
        let res = type_check(program, source);
        assert!(matches!(
            res,
            Err(TypeCheckError::MethodSignatureMismatch { .. })
        ));
    }

    #[test]
    fn test_duplicate_impl() {
        let source = r#"
            struct S {}
            trait T {}
            impl T for S {}
            impl T for S {}
            fn main() {}
        "#;
        let program = parse(source).unwrap();
        let res = type_check(program, source);
        assert!(matches!(res, Err(TypeCheckError::DuplicateImpl { .. })));
    }

    #[test]
    fn test_unknown_trait_impl() {
        let source = r#"
            struct S {}
            impl U for S {}
            fn main() {}
        "#;
        let program = parse(source).unwrap();
        let res = type_check(program, source);
        assert!(matches!(res, Err(TypeCheckError::UnknownTrait(_))));
    }

    #[test]
    fn test_demonstrates_type_checker_solves_pointer_problem() {
        // This test demonstrates that the type checker solves the original problem:
        // Before: pointer dereference always loaded 8 bytes regardless of type
        // After: pointer dereference has correct type information for proper codegen

        let source = "fn test(ptr: *const i32) -> i32 { *ptr } fn main() {}";
        let program = parse(source).unwrap();

        let typed = type_check(program, source).unwrap();

        // Find the dereference expression and verify it has the correct type
        let test_func = typed
            .functions
            .iter()
            .find(|f| f.name.text(source) == "test")
            .unwrap();

        if let crate::Stmt::Expr(expr) = &test_func.body[0] {
            if let crate::Expr::Dereference { ty, .. } = expr {
                // SUCCESS: The dereference expression now has Type::I32
                // This means codegen can generate the correct load instruction:
                // - Before: always load 8 bytes (i64)
                // - After: load 4 bytes for i32, 1 byte for u8, etc.
                assert_eq!(*ty, Type::I32);
            } else {
                panic!("Expected dereference expression");
            }
        } else {
            panic!("Expected expression statement");
        }
    }
}
