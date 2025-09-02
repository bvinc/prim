use crate::{Expr, Function, Program, Stmt, Type};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeChecker {
    /// Variable name -> Type mapping for current scope
    variables: HashMap<String, Type>,
    /// Function name -> return type mapping
    functions: HashMap<String, Option<Type>>,
    /// Source text for span lookups
    source: String,
}

#[derive(Debug)]
pub enum TypeCheckError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeMismatch { expected: Type, found: Type },
    InvalidDereference(Type),
}

impl TypeChecker {
    pub fn new(source: &str) -> Self {
        let mut checker = Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            source: source.to_string(),
        };

        // Add built-in functions
        checker.functions.insert("println".to_string(), None);

        checker
    }

    pub fn check_program(&mut self, mut program: Program) -> Result<Program, TypeCheckError> {
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
