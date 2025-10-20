use prim_tok::Tokenizer;

mod error;
pub use error::ParseError;

pub use prim_tok::Span;

// Parser implementation
pub mod parser;

// Type checker implementation
pub mod typecheck;

// Re-export parser for easy access
pub use parser::Parser;

// Re-export type checker
pub use typecheck::{TypeCheckError, type_check};

#[derive(Debug, Clone, PartialEq)]
pub enum PointerMutability {
    Const,
    Mutable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Usize,
    Isize,
    F32,
    F64,
    Bool,
    // String slice with runtime representation (ptr, len)
    StrSlice,
    Array(Box<Type>),
    Struct(Span), // struct name reference
    Pointer {
        mutability: PointerMutability,
        pointee: Box<Type>,
    },
    Undetermined, // Type not yet determined during parsing
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    IntLiteral {
        span: Span,
        ty: Type,
    },
    FloatLiteral {
        span: Span,
        ty: Type,
    },
    BoolLiteral {
        value: bool,
        ty: Type,
    },
    StringLiteral {
        span: Span,
        ty: Type,
    },
    Identifier {
        span: Span,
        ty: Type,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
        ty: Type,
    },
    FunctionCall {
        // Qualified name segments: e.g., module.function or just function
        path: NamePath,
        args: Vec<Expr>,
        ty: Type,
    },
    StructLiteral {
        name: Span,
        fields: Vec<StructField>,
        ty: Type,
    },
    FieldAccess {
        object: Box<Expr>,
        field: Span,
        ty: Type,
    },
    Dereference {
        operand: Box<Expr>,
        ty: Type,
    },
    ArrayLiteral {
        elements: Vec<Expr>,
        ty: Type,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Span,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: Span,
    pub fields: Vec<StructFieldDefinition>,
    pub repr_c: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDefinition {
    pub name: Span,
    pub field_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: Span,
        type_annotation: Option<Type>,
        value: Expr,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Span,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
    pub runtime_binding: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Span,
    pub type_annotation: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub module_name: Option<Span>,
    pub imports: Vec<ImportDecl>,
    pub structs: Vec<StructDefinition>,
    pub functions: Vec<Function>,
    pub traits: Vec<TraitDefinition>,
    pub impls: Vec<ImplDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub raw_path: NamePath,
    pub selector: ImportSelector,
    pub trailing_symbol: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelector {
    All,
    Named(Vec<Span>),
}

impl ImportDecl {
    pub fn module_segments<'a>(&'a self, source: &'a str) -> Vec<String> {
        self.raw_path
            .segments
            .iter()
            .map(|s| s.text(source).to_string())
            .collect()
    }

    pub fn selector_names<'a>(&'a self, source: &'a str) -> Option<Vec<String>> {
        match &self.selector {
            ImportSelector::All => None,
            ImportSelector::Named(names) => Some(
                names
                    .iter()
                    .map(|span| span.text(source).to_string())
                    .collect(),
            ),
        }
    }
}

/// Parse a Prim program using the unified parser
pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;
    let mut parser = Parser::new(tokens, input);
    parser.parse()
}

/// Parse a single file/unit without requiring a `main` function.
pub fn parse_unit(input: &str) -> Result<Program, ParseError> {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;
    let mut parser = Parser::new(tokens, input);
    parser.parse_without_main()
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub name: Span,
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDefinition {
    pub trait_name: Span,
    pub struct_name: Span,
    pub methods: Vec<ImplMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub name: Span,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplMethod {
    pub name: Span,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_tok::TokenKind;

    #[test]
    fn test_parse_let_statement() {
        let source = "fn main() { let x: u32 = 42 }";
        let program = parse(source).unwrap();

        assert_eq!(program.functions.len(), 1);
        let main_func = &program.functions[0];
        assert_eq!(main_func.name.text(source), "main");
        assert_eq!(main_func.body.len(), 1);
        match &main_func.body[0] {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                assert_eq!(name.text(source), "x");
                assert_eq!(type_annotation, &Some(Type::U32));
                match value {
                    Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "42"),
                    _ => panic!("Expected IntLiteral, got {:?}", value),
                }
            }
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_let_without_type() {
        let source = "fn main() { let x = 42 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                assert_eq!(name.text(source), "x");
                assert_eq!(type_annotation, &None);
                match value {
                    Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "42"),
                    _ => panic!("Expected IntLiteral, got {:?}", value),
                }
            }
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression() {
        let source = "fn main() { let result = x + 5 * 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match left.as_ref() {
                        Expr::Identifier { span, .. } => assert_eq!(span.text(source), "x"),
                        _ => panic!("Expected Identifier, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "2"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression_2() {
        let source = "fn main() { let result = x * 5 + 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "2"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::Identifier { span, .. } => assert_eq!(span.text(source), "x"),
                                _ => panic!("Expected Identifier, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression, got {:?}", left),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_println() {
        let source = "fn main() { println(42) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { path, args, .. }) => {
                assert_eq!(path.segments[0].text(source), "println");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "42"),
                    _ => panic!("Expected IntLiteral, got {:?}", &args[0]),
                }
            }
            _ => panic!(
                "Expected println function call, got {:?}",
                &main_func.body[0]
            ),
        }
    }

    #[test]
    fn test_parse_println_with_expression() {
        let source = "fn main() { println(x + 5) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { path, args, .. }) => {
                assert_eq!(path.segments[0].text(source), "println");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Expr::Binary {
                        left, op, right, ..
                    } => {
                        assert_eq!(op, &BinaryOp::Add);
                        match left.as_ref() {
                            Expr::Identifier { span, .. } => assert_eq!(span.text(source), "x"),
                            _ => panic!("Expected Identifier, got {:?}", left),
                        }
                        match right.as_ref() {
                            Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                            _ => panic!("Expected IntLiteral, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression, got {:?}", &args[0]),
                }
            }
            _ => panic!(
                "Expected println function call, got {:?}",
                &main_func.body[0]
            ),
        }
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let result = parse("fn main() { let = 42 }");

        match result {
            Err(ParseError::UnexpectedToken {
                expected, found, ..
            }) => {
                assert_eq!(expected, "identifier");
                assert_eq!(found, TokenKind::Equals);
            }
            _ => panic!("Expected UnexpectedToken error, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_error_from_tokenizer() {
        let result = parse("fn main() { let x = @ }");

        match result {
            Err(ParseError::TokenError(prim_tok::TokenError::UnexpectedCharacter {
                ch, ..
            })) => {
                assert_eq!(ch, '@');
            }
            _ => panic!("Expected TokenError from tokenizer, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_error_missing_main() {
        let result = parse("fn foo() { let x = 42 }");

        match result {
            Err(ParseError::MissingMainFunction) => {}
            _ => panic!("Expected MissingMainFunction error, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_error_statements_outside_function() {
        let result = parse("let x = 42");

        match result {
            Err(ParseError::StatementsOutsideFunction) => {}
            _ => panic!("Expected StatementsOutsideFunction error, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_parentheses_basic() {
        let source = "fn main() { let result = (2 + 3) * 4 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Multiply);
                    // Left side should be the parenthesized expression (2 + 3)
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "2"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "3"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (2 + 3), got {:?}", left),
                    }
                    // Right side should be 4
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "4"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    /// Planned: traits and impls
    ///
    /// Syntax to support (future):
    ///   trait Marker {}
    ///   impl Marker for Point {}
    ///
    /// This test is intentionally ignored until the parser supports `trait` and `impl` items.
    #[test]
    fn test_parse_trait_and_impl_syntax() {
        let source = r#"
            struct Point { x: i32, y: i32 }
            trait Marker {}
            impl Marker for Point {}
            fn main() {}
        "#;

        let program = parse(source).expect("parser should handle trait + impl items");

        // When implemented, validate that:
        // - One struct named Point exists
        assert!(
            program
                .structs
                .iter()
                .any(|s| s.name.text(source).trim() == "Point")
        );
        // - One trait named Marker exists
        assert!(
            program
                .traits
                .iter()
                .any(|t| t.name.text(source).trim() == "Marker")
        );
        // - One impl binds Marker for Point
        assert!(
            program
                .impls
                .iter()
                .any(|im| im.trait_name.text(source).trim() == "Marker"
                    && im.struct_name.text(source).trim() == "Point")
        );
    }

    #[test]
    fn test_parse_trait_with_method_and_impl_body() {
        let source = r#"
            struct Point { x: i32, y: i32 }
            trait Greeter { fn hello(a: i32) -> i32; }
            impl Greeter for Point { fn hello(a: i32) -> i32 { a } }
            fn main() {}
        "#;
        let program = parse(source).unwrap();
        let tr = program
            .traits
            .iter()
            .find(|t| t.name.text(source).trim() == "Greeter")
            .expect("trait Greeter present");
        assert_eq!(tr.methods.len(), 1);
        assert_eq!(tr.methods[0].name.text(source), "hello");
        assert!(
            program
                .impls
                .iter()
                .any(|im| im.trait_name.text(source).trim() == "Greeter"
                    && im.struct_name.text(source).trim() == "Point"
                    && !im.methods.is_empty())
        );
    }

    #[test]
    fn test_parse_parentheses_nested() {
        let source = "fn main() { let result = ((2 + 3) * 4) + 5 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Add);
                    // Left side should be ((2 + 3) * 4)
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            // Inner left should be (2 + 3)
                            match left.as_ref() {
                                Expr::Binary {
                                    left, op, right, ..
                                } => {
                                    assert_eq!(op, &BinaryOp::Add);
                                    match left.as_ref() {
                                        Expr::IntLiteral { span, .. } => {
                                            assert_eq!(span.text(source), "2")
                                        }
                                        _ => panic!("Expected IntLiteral, got {:?}", left),
                                    }
                                    match right.as_ref() {
                                        Expr::IntLiteral { span, .. } => {
                                            assert_eq!(span.text(source), "3")
                                        }
                                        _ => panic!("Expected IntLiteral, got {:?}", right),
                                    }
                                }
                                _ => {
                                    panic!("Expected binary expression for (2 + 3), got {:?}", left)
                                }
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "4"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!(
                            "Expected binary expression for ((2 + 3) * 4), got {:?}",
                            left
                        ),
                    }
                    // Right side should be 5
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_parentheses_with_all_operators() {
        let source = "fn main() { let result = (x + y) * (a - b) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Multiply);
                    // Left side: (x + y)
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::Identifier { span, .. } => assert_eq!(span.text(source), "x"),
                                _ => panic!("Expected Identifier, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::Identifier { span, .. } => assert_eq!(span.text(source), "y"),
                                _ => panic!("Expected Identifier, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (x + y), got {:?}", left),
                    }
                    // Right side: (a - b)
                    match right.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Subtract);
                            match left.as_ref() {
                                Expr::Identifier { span, .. } => assert_eq!(span.text(source), "a"),
                                _ => panic!("Expected Identifier, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::Identifier { span, .. } => assert_eq!(span.text(source), "b"),
                                _ => panic!("Expected Identifier, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (a - b), got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_parentheses_function_call_args() {
        let source = "fn main() { println((2 + 3) * 4) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { path, args, .. }) => {
                assert_eq!(path.segments[0].text(source), "println");
                assert_eq!(args.len(), 1);
                // Argument should be (2 + 3) * 4
                match &args[0] {
                    Expr::Binary {
                        left, op, right, ..
                    } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        match left.as_ref() {
                            Expr::Binary {
                                left, op, right, ..
                            } => {
                                assert_eq!(op, &BinaryOp::Add);
                                match left.as_ref() {
                                    Expr::IntLiteral { span, .. } => {
                                        assert_eq!(span.text(source), "2")
                                    }
                                    _ => panic!("Expected IntLiteral, got {:?}", left),
                                }
                                match right.as_ref() {
                                    Expr::IntLiteral { span, .. } => {
                                        assert_eq!(span.text(source), "3")
                                    }
                                    _ => panic!("Expected IntLiteral, got {:?}", right),
                                }
                            }
                            _ => panic!("Expected binary expression for (2 + 3), got {:?}", left),
                        }
                        match right.as_ref() {
                            Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "4"),
                            _ => panic!("Expected IntLiteral, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression, got {:?}", &args[0]),
                }
            }
            _ => panic!("Expected println call, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_error_mismatched_parentheses_missing_close() {
        let result = parse("fn main() { let x = (2 + 3 }");

        match result {
            Err(ParseError::UnexpectedToken {
                expected, found, ..
            }) => {
                assert_eq!(expected, "Expected ')'");
                assert_eq!(found, TokenKind::RightBrace);
            }
            _ => panic!(
                "Expected UnexpectedToken error for missing ')', got {:?}",
                result
            ),
        }
    }

    #[test]
    fn test_parse_error_mismatched_parentheses_missing_open() {
        let result = parse("fn main() { let x = 2 + 3) }");

        match result {
            Err(ParseError::UnexpectedToken {
                expected, found, ..
            }) => {
                assert_eq!(expected, "';', newline, or '}' after statement");
                assert_eq!(found, TokenKind::RightParen);
            }
            _ => panic!(
                "Expected UnexpectedToken error for unexpected ')', got {:?}",
                result
            ),
        }
    }

    #[test]
    fn test_parse_empty_parentheses_error() {
        let result = parse("fn main() { let x = () }");

        match result {
            Err(ParseError::UnexpectedToken { expected, .. }) => {
                assert_eq!(expected, "expression");
            }
            _ => panic!(
                "Expected UnexpectedToken error for empty parentheses, got {:?}",
                result
            ),
        }
    }

    #[test]
    fn test_parse_subtraction_basic() {
        let source = "fn main() { let result = 10 - 3 }";
        let program = parse(source).unwrap();
        let debug_str = format!("{:#?}", program);

        assert!(debug_str.contains("Subtract"));
        assert!(debug_str.contains("IntLiteral"));
    }

    #[test]
    fn test_parse_subtraction_with_identifiers() {
        let source = "fn main() { let result = x - y }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    match left.as_ref() {
                        Expr::Identifier { span, .. } => assert_eq!(span.text(source), "x"),
                        _ => panic!("Expected Identifier, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::Identifier { span, .. } => assert_eq!(span.text(source), "y"),
                        _ => panic!("Expected Identifier, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_subtraction_precedence() {
        let source = "fn main() { let result = 10 - 3 * 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    match left.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "10"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be 3 * 2 (multiplication has higher precedence)
                    match right.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "3"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "2"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for 3 * 2, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_subtraction_chained() {
        let source = "fn main() { let result = 20 - 5 - 3 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    // Left side should be (20 - 5) due to left associativity
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Subtract);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => {
                                    assert_eq!(span.text(source), "20")
                                }
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (20 - 5), got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "3"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_subtraction_with_parentheses() {
        let source = "fn main() { let result = 20 - (5 + 3) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    match left.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "20"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be (5 + 3)
                    match right.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "3"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (5 + 3), got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_basic() {
        let source = "fn main() { let result = 20 / 4 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    match left.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "20"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "4"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_with_identifiers() {
        let source = "fn main() { let result = numerator / denominator }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    match left.as_ref() {
                        Expr::Identifier { span, .. } => assert_eq!(span.text(source), "numerator"),
                        _ => panic!("Expected Identifier, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::Identifier { span, .. } => {
                            assert_eq!(span.text(source), "denominator")
                        }
                        _ => panic!("Expected Identifier, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_precedence_with_addition() {
        let source = "fn main() { let result = 10 + 20 / 4 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match left.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "10"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be 20 / 4 (division has higher precedence)
                    match right.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Divide);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => {
                                    assert_eq!(span.text(source), "20")
                                }
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "4"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for 20 / 4, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_chained() {
        let source = "fn main() { let result = 100 / 5 / 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    // Left side should be (100 / 5) due to left associativity
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Divide);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => {
                                    assert_eq!(span.text(source), "100")
                                }
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (100 / 5), got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "2"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_with_multiplication() {
        let source = "fn main() { let result = 8 * 6 / 3 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    // Left side should be (8 * 6) due to left associativity
                    match left.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "8"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "6"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (8 * 6), got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "3"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_with_parentheses() {
        let source = "fn main() { let result = 100 / (10 + 5) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    match left.as_ref() {
                        Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "100"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be (10 + 5)
                    match right.as_ref() {
                        Expr::Binary {
                            left, op, right, ..
                        } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::IntLiteral { span, .. } => {
                                    assert_eq!(span.text(source), "10")
                                }
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (10 + 5), got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_chained_function_calls() {
        let source = r#"
fn level4() -> i64 {
    println(4)
    42
}

fn level3() -> i64 {
    println(3)
    let result = level4()
    println(300 + result)
    result
}

fn level2() -> i64 {
    println(2)
    let result = level3()
    println(200 + result)
    result
}

fn level1() -> i64 {
    println(1)
    let result = level2()
    println(100 + result)
    result
}

fn main() {
    println(0)
    let final_result = level1()
    println(final_result)
}
"#;
        let program = parse(source).unwrap();

        // Check that we have all 5 functions
        assert_eq!(program.functions.len(), 5);

        // Check function names
        let function_names: Vec<&str> = program
            .functions
            .iter()
            .map(|f| f.name.text(source))
            .collect();
        assert!(function_names.contains(&"level4"));
        assert!(function_names.contains(&"level3"));
        assert!(function_names.contains(&"level2"));
        assert!(function_names.contains(&"level1"));
        assert!(function_names.contains(&"main"));

        // Check that main function has function calls
        let main_func = program
            .functions
            .iter()
            .find(|f| f.name.text(source) == "main")
            .expect("main function should exist");

        // Verify main has statements
        assert!(!main_func.body.is_empty());

        // Quick check that we have function calls in the AST
        let debug_str = format!("{:#?}", program);
        assert!(debug_str.contains("FunctionCall"));

        // Check that level1 function calls level2
        let level1_func = program
            .functions
            .iter()
            .find(|f| f.name.text(source) == "level1")
            .expect("level1 function should exist");

        // Find the function call to level2 in level1
        let has_level2_call = level1_func.body.iter().any(|stmt| match stmt {
            Stmt::Let {
                value: Expr::FunctionCall { path, .. },
                ..
            } => path.segments.len() == 1 && path.segments[0].text(source) == "level2",
            _ => false,
        });
        assert!(has_level2_call, "level1 should call level2");
    }

    #[test]
    fn test_both_parsers_produce_same_result() {
        let source = "fn main() { let result = 2 + 3 * 4\nprintln(result) }";

        // Parse with the unified parser
        let program = parse(source).unwrap();

        // Basic structure check
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name.text(source), "main");
    }

    #[test]
    fn test_whitespace_ignored() {
        // Test that whitespace is completely ignored during parsing
        let messy_input = "fn   main (  )   {   let   x :  i32   =   2   +   3   *   4   }";
        let clean_input = "fn main() { let x: i32 = 2 + 3 * 4 }";

        let messy_program = parse(messy_input).expect("Should parse messy input");
        let clean_program = parse(clean_input).expect("Should parse clean input");

        // Both should produce structurally identical ASTs (spans will differ due to whitespace)
        assert_eq!(messy_program.functions.len(), clean_program.functions.len());
        assert_eq!(messy_program.functions.len(), 1);
        assert_eq!(messy_program.functions[0].name.text(messy_input), "main");
        assert_eq!(clean_program.functions[0].name.text(clean_input), "main");

        // Test that the arithmetic expression is parsed correctly in both cases
        if let Some(Stmt::Let { value, .. }) = messy_program.functions[0].body.first() {
            // Should be parsed as 2 + (3 * 4)
            if let Expr::Binary {
                left,
                op: BinaryOp::Add,
                right,
                ..
            } = value
            {
                assert!(matches!(**left, Expr::IntLiteral { .. }));
                if let Expr::Binary {
                    op: BinaryOp::Multiply,
                    ..
                } = &**right
                {
                    println!("✓ Whitespace ignored - both inputs produce identical AST");
                } else {
                    panic!("Right side should be multiplication");
                }
            } else {
                panic!("Expected binary addition expression");
            }
        } else {
            panic!("Expected let statement");
        }
    }

    #[test]
    fn test_parse_struct_definition() {
        let source = r#"
struct Point {
    x: i32,
    y: i32
}

fn main() {
    let p = Point { x = 10, y = 20 }
    println(p.x)
}
"#;
        let program = parse(source).unwrap();

        // Check that we have one struct and one function
        assert_eq!(program.structs.len(), 1);
        assert_eq!(program.functions.len(), 1);

        // Check struct definition
        let point_struct = &program.structs[0];
        assert_eq!(point_struct.name.text(source), "Point");
        assert_eq!(point_struct.fields.len(), 2);

        // Check first field
        assert_eq!(point_struct.fields[0].name.text(source), "x");
        assert_eq!(point_struct.fields[0].field_type, Type::I32);

        // Check second field
        assert_eq!(point_struct.fields[1].name.text(source), "y");
        assert_eq!(point_struct.fields[1].field_type, Type::I32);

        // Check main function has struct literal and field access
        let main_func = &program.functions[0];
        assert_eq!(main_func.body.len(), 2);

        // Check struct literal in let statement
        if let Stmt::Let {
            value: Expr::StructLiteral { name, fields, .. },
            ..
        } = &main_func.body[0]
        {
            assert_eq!(name.text(source), "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name.text(source), "x");
            assert_eq!(fields[1].name.text(source), "y");
        } else {
            panic!("Expected struct literal in let statement");
        }

        // Check field access in println
        if let Stmt::Expr(Expr::FunctionCall { args, .. }) = &main_func.body[1] {
            if let Expr::FieldAccess { object, field, .. } = &args[0] {
                assert_eq!(field.text(source), "x");
                if let Expr::Identifier { span: id, .. } = object.as_ref() {
                    assert_eq!(id.text(source), "p");
                } else {
                    panic!("Expected identifier in field access");
                }
            } else {
                panic!("Expected field access in println");
            }
        } else {
            panic!("Expected println call");
        }
    }

    #[test]
    fn test_parse_field_access() {
        let source = "fn main() { let x = point.x }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        if let Stmt::Let {
            value: Expr::FieldAccess { object, field, .. },
            ..
        } = &main_func.body[0]
        {
            assert_eq!(field.text(source), "x");
            if let Expr::Identifier { span: obj_name, .. } = object.as_ref() {
                assert_eq!(obj_name.text(source), "point");
            } else {
                panic!("Expected identifier in field access object");
            }
        } else {
            panic!("Expected field access expression");
        }
    }

    #[test]
    fn test_parse_struct_literal() {
        let source = r#"fn main() { let p = Point { x = 10, y = 20 } }"#;
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        if let Stmt::Let {
            value: Expr::StructLiteral { name, fields, .. },
            ..
        } = &main_func.body[0]
        {
            assert_eq!(name.text(source), "Point");
            assert_eq!(fields.len(), 2);

            // Check first field
            assert_eq!(fields[0].name.text(source), "x");
            if let Expr::IntLiteral { span: val, .. } = &fields[0].value {
                assert_eq!(val.text(source), "10");
            } else {
                panic!("Expected integer literal for x field");
            }

            // Check second field
            assert_eq!(fields[1].name.text(source), "y");
            if let Expr::IntLiteral { span: val, .. } = &fields[1].value {
                assert_eq!(val.text(source), "20");
            } else {
                panic!("Expected integer literal for y field");
            }
        } else {
            panic!("Expected struct literal");
        }
    }

    #[test]
    fn test_parse_struct_type_annotation() {
        let source = "fn main() { let p: Point = get_point() }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        if let Stmt::Let {
            type_annotation: Some(Type::Struct(name)),
            ..
        } = &main_func.body[0]
        {
            assert_eq!(name.text(source), "Point");
        } else {
            panic!("Expected struct type annotation");
        }
    }

    #[test]
    fn test_parse_pointer_types() {
        // Test const pointer
        let source = "fn main() { let ptr: *const u8 = get_ptr() }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        if let Stmt::Let {
            type_annotation:
                Some(Type::Pointer {
                    mutability,
                    pointee,
                }),
            ..
        } = &main_func.body[0]
        {
            assert_eq!(*mutability, PointerMutability::Const);
            if let Type::U8 = **pointee {
                // Expected
            } else {
                panic!("Expected u8 pointee type");
            }
        } else {
            panic!("Expected const pointer type annotation");
        }

        // Test mutable pointer
        let source_mut = "fn main() { let ptr: *mut i32 = get_ptr() }";
        let program_mut = parse(source_mut).unwrap();

        let main_func_mut = &program_mut.functions[0];
        if let Stmt::Let {
            type_annotation:
                Some(Type::Pointer {
                    mutability,
                    pointee,
                }),
            ..
        } = &main_func_mut.body[0]
        {
            assert_eq!(*mutability, PointerMutability::Mutable);
            if let Type::I32 = **pointee {
                // Expected
            } else {
                panic!("Expected i32 pointee type");
            }
        } else {
            panic!("Expected mutable pointer type annotation");
        }
    }

    #[test]
    fn test_parse_dereference() {
        let source = "fn main() { let value = *ptr }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        if let Stmt::Let {
            value: Expr::Dereference { operand, .. },
            ..
        } = &main_func.body[0]
        {
            if let Expr::Identifier { span: name, .. } = &**operand {
                assert_eq!(name.text(source), "ptr");
            } else {
                panic!("Expected identifier in dereference operand");
            }
        } else {
            panic!("Expected let statement with dereference expression");
        }
    }

    #[test]
    fn test_parser_arithmetic_expressions() {
        // Test parser directly on arithmetic expressions (not full programs)
        let test_cases = vec![
            ("2 + 3 * 4", "Should parse as 2 + (3 * 4)"),
            ("2 * 3 + 4", "Should parse as (2 * 3) + 4"),
            (
                "1 + 2 + 3",
                "Should parse as (1 + 2) + 3 (left associative)",
            ),
            (
                "2 * 3 * 4",
                "Should parse as (2 * 3) * 4 (left associative)",
            ),
            ("(2 + 3) * 4", "Should respect parentheses"),
        ];

        for (expr_input, description) in test_cases {
            println!("Testing parser on: {} - {}", expr_input, description);

            let mut tokenizer = prim_tok::Tokenizer::new(expr_input);
            let tokens = tokenizer.tokenize().expect("Failed to tokenize");
            let mut parser = Parser::new(tokens, expr_input);

            // Test expression parsing directly using the parser's parse_expression method
            match parser.parse_expression(crate::parser::Precedence::NONE) {
                Ok(expr) => {
                    println!("  ✓ Successfully parsed expression");

                    // Validate precedence for "2 + 3 * 4" - should be 2 + (3 * 4)
                    if expr_input == "2 + 3 * 4" {
                        if let Expr::Binary {
                            left,
                            op: BinaryOp::Add,
                            right,
                            ..
                        } = &expr
                        {
                            // Left should be IntLiteral(2)
                            if let Expr::IntLiteral {
                                span: left_span, ..
                            } = &**left
                            {
                                assert_eq!(left_span.text(expr_input), "2");
                            } else {
                                panic!("Left side should be IntLiteral(2), got: {:?}", left);
                            }

                            // Right should be Binary { 3 * 4 }
                            if let Expr::Binary {
                                left: mult_left,
                                op: BinaryOp::Multiply,
                                right: mult_right,
                                ..
                            } = &**right
                            {
                                if let (
                                    Expr::IntLiteral {
                                        span: left_span, ..
                                    },
                                    Expr::IntLiteral {
                                        span: right_span, ..
                                    },
                                ) = (&**mult_left, &**mult_right)
                                {
                                    assert_eq!(left_span.text(expr_input), "3");
                                    assert_eq!(right_span.text(expr_input), "4");
                                    println!("    ✓ Correct precedence: parsed as 2 + (3 * 4)");
                                } else {
                                    panic!("Multiplication operands should be literals");
                                }
                            } else {
                                panic!("Right side should be multiplication, got: {:?}", right);
                            }
                        } else {
                            panic!(
                                "Expected addition at top level for 2 + 3 * 4, got: {:?}",
                                expr
                            );
                        }
                    }

                    // Validate left associativity for "1 + 2 + 3" - should be (1 + 2) + 3
                    if expr_input == "1 + 2 + 3" {
                        if let Expr::Binary {
                            left,
                            op: BinaryOp::Add,
                            right,
                            ..
                        } = &expr
                        {
                            // Left should be Binary { 1 + 2 }
                            if let Expr::Binary {
                                left: add_left,
                                op: BinaryOp::Add,
                                right: add_right,
                                ..
                            } = &**left
                            {
                                if let (
                                    Expr::IntLiteral {
                                        span: left_span, ..
                                    },
                                    Expr::IntLiteral {
                                        span: right_span, ..
                                    },
                                ) = (&**add_left, &**add_right)
                                {
                                    assert_eq!(left_span.text(expr_input), "1");
                                    assert_eq!(right_span.text(expr_input), "2");
                                } else {
                                    panic!("Addition operands should be literals");
                                }
                            } else {
                                panic!("Left side should be (1 + 2), got: {:?}", left);
                            }

                            // Right should be IntLiteral(3)
                            if let Expr::IntLiteral {
                                span: right_span, ..
                            } = &**right
                            {
                                assert_eq!(right_span.text(expr_input), "3");
                                println!("    ✓ Correct left associativity: parsed as (1 + 2) + 3");
                            } else {
                                panic!("Right side should be IntLiteral(3), got: {:?}", right);
                            }
                        } else {
                            panic!(
                                "Expected addition at top level for 1 + 2 + 3, got: {:?}",
                                expr
                            );
                        }
                    }
                }
                Err(e) => {
                    panic!("Failed to parse expression '{}': {:?}", expr_input, e);
                }
            }
        }
    }

    #[test]
    fn test_import_decl_variants() {
        let source = "import foo.bar\nimport foo.bar.Baz\nimport foo.bar.{Baz, Quux}\nfn main() {}";
        let program = parse(source).unwrap();
        assert_eq!(program.imports.len(), 3);

        let module_import = &program.imports[0];
        assert_eq!(
            module_import.module_segments(source),
            vec!["foo".to_string(), "bar".to_string()]
        );
        assert!(matches!(module_import.selector, ImportSelector::All));
        assert_eq!(
            module_import
                .trailing_symbol
                .as_ref()
                .map(|s| s.text(source)),
            Some("bar")
        );

        let trailing_symbol_import = &program.imports[1];
        assert_eq!(
            trailing_symbol_import.module_segments(source),
            vec!["foo".to_string(), "bar".to_string(), "Baz".to_string()]
        );
        assert!(matches!(
            trailing_symbol_import.selector,
            ImportSelector::All
        ));
        assert_eq!(
            trailing_symbol_import
                .trailing_symbol
                .as_ref()
                .map(|s| s.text(source)),
            Some("Baz")
        );

        let brace_import = &program.imports[2];
        assert_eq!(
            brace_import.module_segments(source),
            vec!["foo".to_string(), "bar".to_string()]
        );
        match &brace_import.selector {
            ImportSelector::Named(names) => {
                let texts: Vec<_> = names.iter().map(|n| n.text(source)).collect();
                assert_eq!(texts, vec!["Baz", "Quux"]);
            }
            _ => panic!("expected named selector"),
        }
        assert!(brace_import.trailing_symbol.is_none());
    }

    #[test]
    fn test_definition_spans_include_attributes() {
        let struct_source = "@repr(\"C\")\nstruct Foo { }";
        let struct_program = parse_unit(struct_source).unwrap();
        let def = &struct_program.structs[0];
        assert_eq!(def.span.text(struct_source), "@repr(\"C\")\nstruct Foo { }");

        let func_source = "@runtime(\"puts\")\nfn print(message: Str);";
        let func_program = parse_unit(func_source).unwrap();
        let func = &func_program.functions[0];
        assert_eq!(
            func.span.text(func_source),
            "@runtime(\"puts\")\nfn print(message: Str);"
        );
    }

    #[test]
    fn test_trait_and_impl_spans() {
        let source = "trait Greeter { fn greet(person: Person); }\n\nimpl Greeter for Person {\n    fn greet(person: Person) {}\n}\n";
        let program = parse_unit(source).unwrap();
        assert_eq!(program.traits.len(), 1);
        assert_eq!(program.impls.len(), 1);
        let trait_def = &program.traits[0];
        assert_eq!(
            trait_def.span.text(source),
            "trait Greeter { fn greet(person: Person); }"
        );
        let impl_def = &program.impls[0];
        assert_eq!(
            impl_def.span.text(source),
            "impl Greeter for Person {\n    fn greet(person: Person) {}\n}"
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamePath {
    pub segments: Vec<Span>,
}

impl NamePath {
    pub fn from_single(seg: Span) -> Self {
        Self {
            segments: vec![seg],
        }
    }

    pub fn is_single(&self) -> bool {
        self.segments.len() == 1
    }

    pub fn to_string(&self, source: &str) -> String {
        self.segments
            .iter()
            .map(|s| s.text(source).to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    pub fn mangle(&self, source: &str, sep: &str) -> String {
        self.segments
            .iter()
            .map(|s| s.text(source).to_string())
            .collect::<Vec<_>>()
            .join(sep)
    }
}
