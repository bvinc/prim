use crate::{Expr, Function, Program, Stmt, Type};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct TypeChecker {
    /// Variable name -> Type mapping for current scope
    variables: HashMap<String, Type>,
    /// Function name -> return type mapping
    functions: HashMap<String, Option<Type>>,
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

impl TypeChecker {
    pub fn new(source: &str) -> Self {
        let mut checker = Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            source: source.to_string(),
            traits: HashMap::new(),
            seen_impls: HashSet::new(),
        };

        // Add built-in functions
        checker.functions.insert("println".to_string(), None);

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
                match impl_methods.get(mname) {
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
            self.functions
                .insert(func_name.to_string(), func.return_type.clone());
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
                    // First set the expected type on the expression if it's an integer literal
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
                }

                let value_type = self.check_expression(value)?;

                // If there's a type annotation, verify compatibility
                if let Some(annotated_type) = type_annotation {
                    if *annotated_type != value_type {
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

            Expr::FunctionCall { name, args, ty } => {
                let func_name = name.text(&self.source);
                let func_name_string = func_name.to_string();

                // Check function exists
                if !self.functions.contains_key(&func_name_string) {
                    return Err(TypeCheckError::UndefinedFunction(func_name_string));
                }

                // Type check arguments
                for arg in args {
                    self.check_expression(arg)?;
                }

                // Return the function's return type, or i64 for built-ins like println
                let return_type = self
                    .functions
                    .get(&func_name_string)
                    .unwrap()
                    .clone()
                    .unwrap_or(Type::I64);
                *ty = return_type.clone();
                return_type
            }

            Expr::Binary {
                left,
                op: _,
                right,
                ty,
            } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                // For simplicity, assume binary operations return the left operand's type
                if left_type != right_type {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: left_type.clone(),
                        found: right_type,
                    });
                }

                *ty = left_type.clone();
                left_type
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
            Expr::StructLiteral { ty, .. } => {
                let placeholder_type = Type::I64; // TODO: implement struct type checking
                *ty = placeholder_type.clone();
                placeholder_type
            }
            Expr::FieldAccess { ty, .. } => {
                let placeholder_type = Type::I64; // TODO: implement field type checking
                *ty = placeholder_type.clone();
                placeholder_type
            }
        };

        Ok(ty)
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
