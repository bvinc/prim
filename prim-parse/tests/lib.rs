use prim_parse::{
    BinaryOp, Expr, ImportSelector, ParseError, PointerMutability, Stmt, Type, parse,
};
use prim_tok::TokenKind;

fn parse_ok(source: &str) -> prim_parse::Program {
    parse(source).0.unwrap()
}

#[test]
fn test_error_same_line_statements() {
    let source = "fn main() { let x = 1 let y = 2 }";
    let (result, diagnostics) = parse(source);

    // Should fail to parse
    assert!(result.is_err());

    // Should have a diagnostic about same-line statements
    assert_eq!(diagnostics.len(), 1);
    assert!(
        diagnostics[0]
            .message
            .contains("statements on the same line")
    );
    assert_eq!(diagnostics[0].span.start(), source.find("let y").unwrap());
}

#[test]
fn test_parse_let_statement() {
    let source = "fn main() { let x: u32 = 42 }";
    let program = parse_ok(source);

    assert_eq!(program.functions.len(), 1);
    let main_func = &program.functions[0];
    assert_eq!(program.resolve(main_func.name), "main");
    assert_eq!(main_func.body.len(), 1);
    match &main_func.body[0] {
        Stmt::Let {
            name,
            mutable,
            type_annotation,
            value,
        } => {
            assert_eq!(name.text(source), "x");
            assert!(!mutable);
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
fn test_parse_loop_with_break() {
    let source = "fn main() { loop { break } }";
    let program = parse_ok(source);
    let main_func = &program.functions[0];
    assert_eq!(main_func.body.len(), 1);
    match &main_func.body[0] {
        Stmt::Loop { body, span } => {
            assert_eq!(body.len(), 1);
            assert_eq!(span.text(source), "loop { break }");
            assert!(matches!(body[0], Stmt::Break { .. }));
        }
        other => panic!("Expected loop statement, found {:?}", other),
    }
}

#[test]
fn test_parse_nested_loops_preserve_spans() {
    let source = "fn main() {\n    loop {\n        loop {\n            break\n        }\n        break\n    }\n}";
    let program = parse_ok(source);
    let main_func = &program.functions[0];
    match &main_func.body[0] {
        Stmt::Loop { body, .. } => {
            assert_eq!(body.len(), 2);
            match &body[0] {
                Stmt::Loop { body: inner, .. } => {
                    assert!(matches!(inner[0], Stmt::Break { .. }));
                }
                other => panic!("Expected inner loop, found {:?}", other),
            }
            assert!(matches!(body[1], Stmt::Break { .. }));
        }
        other => panic!("Expected outer loop, found {:?}", other),
    }
}

#[test]
fn test_parse_let_without_type() {
    let source = "fn main() { let x = 42 }";
    let program = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body[0] {
        Stmt::Let {
            name,
            mutable,
            type_annotation,
            value,
        } => {
            assert_eq!(name.text(source), "x");
            assert!(!mutable);
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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let result = parse("fn main() { let = 42 }").0;

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
    let result = parse("fn main() { let x = @ }").0;

    match result {
        Err(ParseError::TokenError(prim_tok::TokenError::UnexpectedCharacter { ch, .. })) => {
            assert_eq!(ch, '@');
        }
        _ => panic!("Expected TokenError from tokenizer, got {:?}", result),
    }
}

#[test]
fn test_parse_error_statements_outside_function() {
    let result = parse("let x = 42").0;

    match result {
        Err(ParseError::StatementsOutsideFunction) => {}
        _ => panic!("Expected StatementsOutsideFunction error, got {:?}", result),
    }
}

#[test]
fn test_parse_parentheses_basic() {
    let source = "fn main() { let result = (2 + 3) * 4 }";
    let program = parse_ok(source);

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

#[test]
fn test_parse_trait_and_impl_syntax() {
    let source = r#"
            struct Point { x: i32, y: i32 }
            trait Marker {}
            impl Marker for Point {}
            fn main() {}
        "#;

    let program = parse_ok(source);

    assert!(
        program
            .structs
            .iter()
            .any(|s| s.name.text(source).trim() == "Point")
    );
    assert!(
        program
            .traits
            .iter()
            .any(|t| t.name.text(source).trim() == "Marker")
    );
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
    let program = parse_ok(source);
    let tr = program
        .traits
        .iter()
        .find(|t| t.name.text(source).trim() == "Greeter")
        .expect("trait Greeter present");
    assert_eq!(tr.methods.len(), 1);
    assert_eq!(program.resolve(tr.methods[0].name), "hello");
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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let result = parse("fn main() { let x = (2 + 3 }").0;

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
    let source = "fn main() { let x = 2 + 3) }";
    let (result, diagnostics) = parse(source);

    // Should emit diagnostic about same-line statements (because ) is on same line)
    assert_eq!(diagnostics.len(), 1);
    assert!(
        diagnostics[0]
            .message
            .contains("statements on the same line")
    );

    // Should fail with parse error
    assert!(result.is_err());
}

#[test]
fn test_parse_empty_parentheses_error() {
    let result = parse("fn main() { let x = () }").0;

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
    let program = parse_ok(source);
    let debug_str = format!("{:#?}", program);

    assert!(debug_str.contains("Subtract"));
    assert!(debug_str.contains("IntLiteral"));
}

#[test]
fn test_parse_subtraction_with_identifiers() {
    let source = "fn main() { let result = x - y }";
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

    // Check that we have all 5 functions
    assert_eq!(program.functions.len(), 5);

    // Check function names
    let function_names: Vec<&str> = program
        .functions
        .iter()
        .map(|f| program.resolve(f.name))
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
        .find(|f| program.resolve(f.name) == "main")
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
        .find(|f| program.resolve(f.name) == "level1")
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
    let program = parse_ok(source);

    // Basic structure check
    assert_eq!(program.functions.len(), 1);
    assert_eq!(program.resolve(program.functions[0].name), "main");
}

#[test]
fn test_whitespace_ignored() {
    // Test that whitespace is completely ignored during parsing
    let messy_input = "fn   main (  )   {   let   x :  i32   =   2   +   3   *   4   }";
    let clean_input = "fn main() { let x: i32 = 2 + 3 * 4 }";

    let messy_program = parse_ok(messy_input);
    let clean_program = parse_ok(clean_input);

    // Both should produce structurally identical ASTs (spans will differ due to whitespace)
    assert_eq!(messy_program.functions.len(), clean_program.functions.len());
    assert_eq!(messy_program.functions.len(), 1);
    assert_eq!(
        messy_program.resolve(messy_program.functions[0].name),
        "main"
    );
    assert_eq!(
        clean_program.resolve(clean_program.functions[0].name),
        "main"
    );

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
            assert!(matches!(
                &**right,
                Expr::Binary {
                    op: BinaryOp::Multiply,
                    ..
                }
            ));
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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
    let program = parse_ok(source);

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
        assert!(matches!(**pointee, Type::U8));
    } else {
        panic!("Expected const pointer type annotation");
    }

    // Test mutable pointer
    let source_mut = "fn main() { let ptr: *mut i32 = get_ptr() }";
    let program_mut = parse_ok(source_mut);

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
        assert!(matches!(**pointee, Type::I32));
    } else {
        panic!("Expected mutable pointer type annotation");
    }
}

#[test]
fn test_parse_dereference() {
    let source = "fn main() { let value = *ptr }";
    let program = parse_ok(source);

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
fn test_import_decl_variants() {
    let source = "import foo.bar\nimport foo.bar.Baz\nimport foo.bar.{Baz, Quux}\nfn main() {}";
    let program = parse_ok(source);
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
    let struct_program = parse_ok(struct_source);
    let def = &struct_program.structs[0];
    assert_eq!(def.span.text(struct_source), "@repr(\"C\")\nstruct Foo { }");

    let func_source = "@runtime(\"puts\")\nfn print(message: Str);";
    let func_program = parse_ok(func_source);
    let func = &func_program.functions[0];
    assert_eq!(
        func.span.text(func_source),
        "@runtime(\"puts\")\nfn print(message: Str);"
    );
}

#[test]
fn test_trait_and_impl_spans() {
    let source = "trait Greeter { fn greet(person: Person); }\n\nimpl Greeter for Person {\n    fn greet(person: Person) {}\n}\n";
    let program = parse_ok(source);
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

#[test]
fn test_parse_let_mut() {
    let source = "fn main() { let mut x: i32 = 42 }";
    let program = parse_ok(source);

    let main_func = &program.functions[0];
    assert_eq!(main_func.body.len(), 1);
    match &main_func.body[0] {
        Stmt::Let {
            name,
            mutable,
            type_annotation,
            value,
        } => {
            assert_eq!(name.text(source), "x");
            assert!(*mutable);
            assert_eq!(type_annotation, &Some(Type::I32));
            match value {
                Expr::IntLiteral { span, .. } => assert_eq!(span.text(source), "42"),
                _ => panic!("Expected IntLiteral, got {:?}", value),
            }
        }
        _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
    }
}

#[test]
fn test_parse_let_mut_without_type() {
    let source = "fn main() { let mut counter = 0 }";
    let program = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body[0] {
        Stmt::Let {
            name,
            mutable,
            type_annotation,
            ..
        } => {
            assert_eq!(name.text(source), "counter");
            assert!(*mutable);
            assert_eq!(type_annotation, &None);
        }
        _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
    }
}
