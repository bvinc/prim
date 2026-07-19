use prim_parse::{
    BinaryOp, ExprKind, ImportSelector, Interner, ParseError, PassMode, Pattern, Stmt, Type, parse,
};
use prim_tok::TokenKind;
use std::sync::Arc;

fn parse_ok(source: &str) -> (prim_parse::Program, Arc<Interner>) {
    let interner = Arc::new(Interner::new());
    let program = parse(source, &interner).0.unwrap();
    (program, interner)
}

/// The expression of a block's last statement. Blocks are statement lists
/// (no trailing-expression value), so a bare expression on the last line is
/// a `Stmt::Expr`.
fn tail_expr(body: &prim_parse::Block) -> &prim_parse::Expr {
    match body.stmts.last() {
        Some(Stmt::Expr(e)) => e,
        other => panic!("expected a trailing expression statement, got {:?}", other),
    }
}

#[test]
fn test_error_same_line_statements() {
    let source = "fn main() { let x = 1 let y = 2 }";
    let interner = Interner::new();
    let (result, diagnostics) = parse(source, &interner);

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
fn test_call_must_be_glued_to_name() {
    // A `(` separated from the name by whitespace is not a call, so `id (5)`
    // is two adjacent expressions on one line — a same-line statement error
    // rather than a silent call.
    let source = "fn main() {\n    id (5usize)\n}";
    let (result, diagnostics) = parse(source, &Interner::new());
    assert!(result.is_err());
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("statements on the same line")),
        "expected a same-line diagnostic, got {diagnostics:?}"
    );
}

#[test]
fn test_glued_call_still_parses() {
    // The adjacent form is still a call.
    let source = "fn main() { id(5usize) }";
    let (program, _) = parse_ok(source);
    let body = &program.functions[0].body;
    let call = tail_expr(body);
    assert!(
        matches!(call.kind, ExprKind::FunctionCall { .. }),
        "expected FunctionCall, got {:?}",
        call.kind
    );
}

#[test]
fn test_paren_line_is_grouping_not_call() {
    // A parenthesized expression on the line after a complete statement is its
    // own grouped expression, not a call of the previous line. Before call
    // adjacency was enforced this glued into `id(5usize)(a + 1usize)`.
    let source = "fn f() -> usize {\n    let a = id(5usize)\n    (a + 1usize)\n}";
    let (program, _) = parse_ok(source);
    let body = &program.functions[0].body;
    assert_eq!(body.stmts.len(), 2, "the let plus the grouped expression");
    let trailing = tail_expr(body);
    assert!(
        matches!(trailing.kind, ExprKind::Binary { .. }),
        "expected a grouped Binary, got {:?}",
        trailing.kind
    );
}

#[test]
fn test_turbofish_args_must_be_glued() {
    // The call args must be glued to the closing `]`: `f[T](x)`, never
    // `f[T] (x)`.
    let source = "fn main() { f[u8] (1usize) }";
    let (result, _diagnostics) = parse(source, &Interner::new());
    assert!(result.is_err(), "f[u8] (x) with a space should not parse");
}

#[test]
fn test_parse_let_statement() {
    let source = "fn main() { let x: u32 = 42 }";
    let (program, interner) = parse_ok(source);

    assert_eq!(program.functions.len(), 1);
    let main_func = &program.functions[0];
    assert_eq!(interner.resolve(&main_func.name.sym), "main");
    assert_eq!(main_func.body.stmts.len(), 1);
    match &main_func.body.stmts[0] {
        Stmt::Let {
            pattern: Pattern::Binding { name, mutable, .. },
            type_annotation,
            value,
            ..
        } => {
            assert_eq!(interner.resolve(&name.sym), "x");
            assert!(!mutable);
            assert_eq!(type_annotation, &Some(Type::U32));
            match &value.kind {
                ExprKind::Int(_) => assert_eq!(value.span.text(source), "42"),
                _ => panic!("Expected Int, got {:?}", value),
            }
        }
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_struct_pattern() {
    // Shorthand binding, `mut` shorthand, and renamed `field: subpat`.
    let source = "fn main() { let Point { x, mut y, z: w } = p }";
    let (program, interner) = parse_ok(source);
    match &program.functions[0].body.stmts[0] {
        Stmt::Let {
            pattern: Pattern::Struct { name, fields, .. },
            ..
        } => {
            assert_eq!(interner.resolve(&name.sym), "Point");
            assert_eq!(fields.len(), 3);
            assert_eq!(interner.resolve(&fields[0].field.sym), "x");
            match &fields[0].pattern {
                Pattern::Binding { name, mutable, .. } => {
                    assert_eq!(interner.resolve(&name.sym), "x");
                    assert!(!mutable);
                }
                other => panic!("expected binding, got {:?}", other),
            }
            // `mut y` shorthand binds y mutably.
            assert_eq!(interner.resolve(&fields[1].field.sym), "y");
            match &fields[1].pattern {
                Pattern::Binding { mutable, .. } => assert!(mutable),
                other => panic!("expected mut binding, got {:?}", other),
            }
            // `z: w` renames the field z to w.
            assert_eq!(interner.resolve(&fields[2].field.sym), "z");
            match &fields[2].pattern {
                Pattern::Binding { name, .. } => assert_eq!(interner.resolve(&name.sym), "w"),
                other => panic!("expected renamed binding, got {:?}", other),
            }
        }
        other => panic!("expected struct pattern let, got {:?}", other),
    }
}

#[test]
fn test_parse_take_pattern() {
    // `take` on a field shorthand and on a whole-value binding sets the binding
    // mode to Take; a plain field binding stays View.
    let source = "fn f() { let r = match v { E.V { take x, y } => x, take rest => rest } }";
    let (program, interner) = parse_ok(source);
    let Stmt::Let { value, .. } = &program.functions[0].body.stmts[0] else {
        panic!("expected let");
    };
    let ExprKind::Match { arms, .. } = &value.kind else {
        panic!("expected match");
    };
    // Arm 0: variant with a `take x` field and a plain `y` field.
    let Pattern::Variant { fields, .. } = &arms[0].pattern else {
        panic!("expected variant pattern");
    };
    assert_eq!(interner.resolve(&fields[0].field.sym), "x");
    match &fields[0].pattern {
        Pattern::Binding { mode, .. } => assert_eq!(*mode, PassMode::Take),
        other => panic!("expected take binding, got {:?}", other),
    }
    match &fields[1].pattern {
        Pattern::Binding { mode, .. } => assert_eq!(*mode, PassMode::Read),
        other => panic!("expected read binding, got {:?}", other),
    }
    // Arm 1: `take rest` binds the whole scrutinee by move.
    match &arms[1].pattern {
        Pattern::Binding { name, mode, .. } => {
            assert_eq!(interner.resolve(&name.sym), "rest");
            assert_eq!(*mode, PassMode::Take);
        }
        other => panic!("expected take binding, got {:?}", other),
    }
}

#[test]
fn test_parse_loop_with_break() {
    let source = "fn main() { loop { break } }";
    let (program, _) = parse_ok(source);
    let main_func = &program.functions[0];
    assert_eq!(main_func.body.stmts.len(), 1);
    match &main_func.body.stmts[0] {
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
    let (program, _) = parse_ok(source);
    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
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
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let {
            pattern: Pattern::Binding { name, mutable, .. },
            type_annotation,
            value,
            ..
        } => {
            assert_eq!(interner.resolve(&name.sym), "x");
            assert!(!mutable);
            assert_eq!(type_annotation, &None);
            match &value.kind {
                ExprKind::Int(_) => assert_eq!(value.span.text(source), "42"),
                _ => panic!("Expected Int, got {:?}", value),
            }
        }
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_arithmetic_expression() {
    let source = "fn main() { let result = x + 5 * 2 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Add);
                match &left.kind {
                    ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                    _ => panic!("Expected Ident, got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        match &left.kind {
                            ExprKind::Int(_) => assert_eq!(left.span.text(source), "5"),
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "2"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_arithmetic_expression_2() {
    let source = "fn main() { let result = x * 5 + 2 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Add);
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "2"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        match &left.kind {
                            ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                            _ => panic!("Expected Ident, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "5"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression, got {:?}", left),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_println() {
    let source = "fn main() { println(42) }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    // The function call is a trailing expression (no semicolon)
    let expr = tail_expr(&main_func.body);
    match &expr.kind {
        ExprKind::FunctionCall { path, args, .. } => {
            assert_eq!(interner.resolve(&path.segments[0].sym), "println");
            assert_eq!(args.len(), 1);
            match &args[0].kind {
                ExprKind::Int(_) => assert_eq!(args[0].span.text(source), "42"),
                _ => panic!("Expected Int, got {:?}", &args[0]),
            }
        }
        _ => panic!(
            "Expected println function call, got {:?}",
            &main_func.body.expr
        ),
    }
}

#[test]
fn test_parse_println_with_expression() {
    let source = "fn main() { println(x + 5) }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    // The function call is a trailing expression (no semicolon)
    let expr = tail_expr(&main_func.body);
    match &expr.kind {
        ExprKind::FunctionCall { path, args, .. } => {
            assert_eq!(interner.resolve(&path.segments[0].sym), "println");
            assert_eq!(args.len(), 1);
            match &args[0].kind {
                ExprKind::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match &left.kind {
                        ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                        _ => panic!("Expected Ident, got {:?}", left),
                    }
                    match &right.kind {
                        ExprKind::Int(_) => assert_eq!(right.span.text(source), "5"),
                        _ => panic!("Expected Int, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", &args[0]),
            }
        }
        _ => panic!(
            "Expected println function call, got {:?}",
            &main_func.body.expr
        ),
    }
}

#[test]
fn test_parse_error_unexpected_token() {
    let interner = Interner::new();
    let result = parse("fn main() { let = 42 }", &interner).0;

    match result {
        Err(ParseError::UnexpectedToken {
            expected, found, ..
        }) => {
            assert_eq!(expected, "Expected pattern");
            assert_eq!(found, TokenKind::Equals);
        }
        _ => panic!("Expected UnexpectedToken error, got {:?}", result),
    }
}

#[test]
fn test_parse_error_unknown_expression_attribute() {
    // Only @dbg is allowed at expression position; anything else is an error.
    let interner = Interner::new();
    let result = parse("fn main() { let x = @foo(1) }", &interner).0;

    match result {
        Err(ParseError::InvalidAttributeUsage { message, .. }) => {
            assert!(
                message.contains("@foo"),
                "expected message to mention @foo, got: {message}"
            );
        }
        _ => panic!("Expected InvalidAttributeUsage, got {:?}", result),
    }
}

#[test]
fn test_parse_error_statements_outside_function() {
    let interner = Interner::new();
    // A bare expression at the top level is rejected; `let` (with a type
    // annotation) is allowed as a module-level global, so use something
    // unambiguously stmt-shaped.
    let result = parse("42", &interner).0;

    match result {
        Err(ParseError::StatementsOutsideFunction { .. }) => {}
        _ => panic!("Expected StatementsOutsideFunction error, got {:?}", result),
    }
}

#[test]
fn test_parse_parentheses_basic() {
    let source = "fn main() { let result = (2 + 3) * 4 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Multiply);
                // Left side should be the parenthesized expression (2 + 3)
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Add);
                        match &left.kind {
                            ExprKind::Int(_) => assert_eq!(left.span.text(source), "2"),
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "3"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (2 + 3), got {:?}", left),
                }
                // Right side should be 4
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "4"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
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

    let (program, interner) = parse_ok(source);

    assert!(
        program
            .structs
            .iter()
            .any(|s| interner.resolve(&s.name.sym).trim() == "Point")
    );
    assert!(
        program
            .traits
            .iter()
            .any(|t| interner.resolve(&t.name.sym).trim() == "Marker")
    );
    assert!(program.impls.iter().any(|im| {
        im.trait_name
            .is_some_and(|t| interner.resolve(&t.sym).trim() == "Marker")
            && matches!(&im.target, Type::Struct(s, _) if interner.resolve(s).trim() == "Point")
    }));
}

#[test]
fn test_parse_trait_with_method_and_impl_body() {
    let source = r#"
            struct Point { x: i32, y: i32 }
            trait Greeter { fn hello(a: i32) -> i32; }
            impl Greeter for Point { fn hello(a: i32) -> i32 { a } }
            fn main() {}
        "#;
    let (program, interner) = parse_ok(source);
    let tr = program
        .traits
        .iter()
        .find(|t| interner.resolve(&t.name.sym).trim() == "Greeter")
        .expect("trait Greeter present");
    assert_eq!(tr.methods.len(), 1);
    assert_eq!(interner.resolve(&tr.methods[0].name.sym), "hello");
    assert!(program.impls.iter().any(|im| {
        im.trait_name
            .is_some_and(|t| interner.resolve(&t.sym).trim() == "Greeter")
            && matches!(&im.target, Type::Struct(s, _) if interner.resolve(s).trim() == "Point")
            && !im.methods.is_empty()
    }));
}

#[test]
fn test_parse_runtime_impl_method() {
    // `@runtime` associated function inside an impl: bodyless, terminated by
    // `;`, carrying the runtime binding (used by the primitive conversions).
    let source = r#"
            impl u64 {
                @runtime("prim_rt_conv_ext_i32_u")
                fn from_u32(x: u32) -> u64;
            }
            fn main() {}
        "#;
    let (program, interner) = parse_ok(source);
    let im = program
        .impls
        .iter()
        .find(|im| matches!(&im.target, Type::U64))
        .expect("impl u64 present");
    assert_eq!(im.methods.len(), 1);
    let m = &im.methods[0];
    assert_eq!(interner.resolve(&m.name.sym), "from_u32");
    assert_eq!(m.runtime.as_deref(), Some("prim_rt_conv_ext_i32_u"));
    assert!(m.body.stmts.is_empty() && m.body.expr.is_none());
}

#[test]
fn test_runtime_impl_method_requires_no_body() {
    // A `@runtime` impl method with a body is rejected.
    let source = r#"
            impl u64 {
                @runtime("prim_rt_conv_ext_i32_u")
                fn from_u32(x: u32) -> u64 { x }
            }
        "#;
    let interner = Interner::new();
    assert!(parse(source, &interner).0.is_err());
}

#[test]
fn test_unary_minus_parses_as_neg() {
    let (program, _) = parse_ok("fn main() { let b = -a }");
    match &program.functions[0].body.stmts[0] {
        Stmt::Let { value, .. } => assert!(
            matches!(value.kind, ExprKind::Neg(_)),
            "expected Neg, got {:?}",
            value.kind
        ),
        other => panic!("expected let, got {:?}", other),
    }
}

#[test]
fn test_negative_literal_is_neg_of_int() {
    // `-5` is the negation operator applied to the literal `5`, not a folded
    // `Int(-5)` and not `0 - 5`.
    let (program, _) = parse_ok("fn main() { let b = -5 }");
    match &program.functions[0].body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Neg(inner) => assert!(
                matches!(inner.kind, ExprKind::Int(5)),
                "expected Int(5) operand, got {:?}",
                inner.kind
            ),
            other => panic!("expected Neg, got {:?}", other),
        },
        other => panic!("expected let, got {:?}", other),
    }
}

#[test]
fn test_unary_plus_is_identity() {
    let (program, _) = parse_ok("fn main() { let b = +a }");
    match &program.functions[0].body.stmts[0] {
        Stmt::Let { value, .. } => assert!(
            matches!(value.kind, ExprKind::Ident(_)),
            "expected bare identifier, got {:?}",
            value.kind
        ),
        other => panic!("expected let, got {:?}", other),
    }
}

#[test]
fn test_parse_parentheses_nested() {
    let source = "fn main() { let result = ((2 + 3) * 4) + 5 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Add);
                // Left side should be ((2 + 3) * 4)
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        // Inner left should be (2 + 3)
                        match &left.kind {
                            ExprKind::Binary { left, op, right } => {
                                assert_eq!(op, &BinaryOp::Add);
                                match &left.kind {
                                    ExprKind::Int(_) => {
                                        assert_eq!(left.span.text(source), "2")
                                    }
                                    _ => panic!("Expected Int, got {:?}", left),
                                }
                                match &right.kind {
                                    ExprKind::Int(_) => {
                                        assert_eq!(right.span.text(source), "3")
                                    }
                                    _ => panic!("Expected Int, got {:?}", right),
                                }
                            }
                            _ => {
                                panic!("Expected binary expression for (2 + 3), got {:?}", left)
                            }
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "4"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!(
                        "Expected binary expression for ((2 + 3) * 4), got {:?}",
                        left
                    ),
                }
                // Right side should be 5
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "5"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_parentheses_with_all_operators() {
    let source = "fn main() { let result = (x + y) * (a - b) }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Multiply);
                // Left side: (x + y)
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Add);
                        match &left.kind {
                            ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                            _ => panic!("Expected Ident, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Ident(_) => assert_eq!(right.span.text(source), "y"),
                            _ => panic!("Expected Ident, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (x + y), got {:?}", left),
                }
                // Right side: (a - b)
                match &right.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Subtract);
                        match &left.kind {
                            ExprKind::Ident(_) => assert_eq!(left.span.text(source), "a"),
                            _ => panic!("Expected Ident, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Ident(_) => assert_eq!(right.span.text(source), "b"),
                            _ => panic!("Expected Ident, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (a - b), got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_parentheses_function_call_args() {
    let source = "fn main() { println((2 + 3) * 4) }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    let expr = tail_expr(&main_func.body);
    match &expr.kind {
        ExprKind::FunctionCall { path, args, .. } => {
            assert_eq!(interner.resolve(&path.segments[0].sym), "println");
            assert_eq!(args.len(), 1);
            // Argument should be (2 + 3) * 4
            match &args[0].kind {
                ExprKind::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Multiply);
                    match &left.kind {
                        ExprKind::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match &left.kind {
                                ExprKind::Int(_) => {
                                    assert_eq!(left.span.text(source), "2")
                                }
                                _ => panic!("Expected Int, got {:?}", left),
                            }
                            match &right.kind {
                                ExprKind::Int(_) => {
                                    assert_eq!(right.span.text(source), "3")
                                }
                                _ => panic!("Expected Int, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (2 + 3), got {:?}", left),
                    }
                    match &right.kind {
                        ExprKind::Int(_) => assert_eq!(right.span.text(source), "4"),
                        _ => panic!("Expected Int, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", &args[0]),
            }
        }
        _ => panic!("Expected FunctionCall, got {:?}", &main_func.body.expr),
    }
}

#[test]
fn test_parse_error_mismatched_parentheses_missing_close() {
    let interner = Interner::new();
    let result = parse("fn main() { let x = (2 + 3 }", &interner).0;

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
    let interner = Interner::new();
    let (result, diagnostics) = parse(source, &interner);

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
    let interner = Interner::new();
    let result = parse("fn main() { let x = () }", &interner).0;

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
    let (program, _) = parse_ok(source);
    let debug_str = format!("{:#?}", program);

    assert!(debug_str.contains("Subtract"));
    assert!(debug_str.contains("Int("));
}

#[test]
fn test_parse_subtraction_with_identifiers() {
    let source = "fn main() { let result = x - y }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Subtract);
                match &left.kind {
                    ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                    _ => panic!("Expected Ident, got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Ident(_) => assert_eq!(right.span.text(source), "y"),
                    _ => panic!("Expected Ident, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_subtraction_precedence() {
    let source = "fn main() { let result = 10 - 3 * 2 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Subtract);
                match &left.kind {
                    ExprKind::Int(_) => assert_eq!(left.span.text(source), "10"),
                    _ => panic!("Expected Int, got {:?}", left),
                }
                // Right side should be 3 * 2 (multiplication has higher precedence)
                match &right.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        match &left.kind {
                            ExprKind::Int(_) => assert_eq!(left.span.text(source), "3"),
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "2"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for 3 * 2, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_subtraction_chained() {
    let source = "fn main() { let result = 20 - 5 - 3 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Subtract);
                // Left side should be (20 - 5) due to left associativity
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Subtract);
                        match &left.kind {
                            ExprKind::Int(_) => {
                                assert_eq!(left.span.text(source), "20")
                            }
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "5"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (20 - 5), got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "3"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_subtraction_with_parentheses() {
    let source = "fn main() { let result = 20 - (5 + 3) }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Subtract);
                match &left.kind {
                    ExprKind::Int(_) => assert_eq!(left.span.text(source), "20"),
                    _ => panic!("Expected Int, got {:?}", left),
                }
                // Right side should be (5 + 3)
                match &right.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Add);
                        match &left.kind {
                            ExprKind::Int(_) => assert_eq!(left.span.text(source), "5"),
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "3"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (5 + 3), got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_division_basic() {
    let source = "fn main() { let result = 20 / 4 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Divide);
                match &left.kind {
                    ExprKind::Int(_) => assert_eq!(left.span.text(source), "20"),
                    _ => panic!("Expected Int, got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "4"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_division_with_identifiers() {
    let source = "fn main() { let result = numerator / denominator }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Divide);
                match &left.kind {
                    ExprKind::Ident(_) => assert_eq!(left.span.text(source), "numerator"),
                    _ => panic!("Expected Ident, got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Ident(_) => {
                        assert_eq!(right.span.text(source), "denominator")
                    }
                    _ => panic!("Expected Ident, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_division_precedence_with_addition() {
    let source = "fn main() { let result = 10 + 20 / 4 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Add);
                match &left.kind {
                    ExprKind::Int(_) => assert_eq!(left.span.text(source), "10"),
                    _ => panic!("Expected Int, got {:?}", left),
                }
                // Right side should be 20 / 4 (division has higher precedence)
                match &right.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Divide);
                        match &left.kind {
                            ExprKind::Int(_) => {
                                assert_eq!(left.span.text(source), "20")
                            }
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "4"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for 20 / 4, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_division_chained() {
    let source = "fn main() { let result = 100 / 5 / 2 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Divide);
                // Left side should be (100 / 5) due to left associativity
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Divide);
                        match &left.kind {
                            ExprKind::Int(_) => {
                                assert_eq!(left.span.text(source), "100")
                            }
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "5"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (100 / 5), got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "2"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_division_with_multiplication() {
    let source = "fn main() { let result = 8 * 6 / 3 }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Divide);
                // Left side should be (8 * 6) due to left associativity
                match &left.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        match &left.kind {
                            ExprKind::Int(_) => assert_eq!(left.span.text(source), "8"),
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "6"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (8 * 6), got {:?}", left),
                }
                match &right.kind {
                    ExprKind::Int(_) => assert_eq!(right.span.text(source), "3"),
                    _ => panic!("Expected Int, got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_division_with_parentheses() {
    let source = "fn main() { let result = 100 / (10 + 5) }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinaryOp::Divide);
                match &left.kind {
                    ExprKind::Int(_) => assert_eq!(left.span.text(source), "100"),
                    _ => panic!("Expected Int, got {:?}", left),
                }
                // Right side should be (10 + 5)
                match &right.kind {
                    ExprKind::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Add);
                        match &left.kind {
                            ExprKind::Int(_) => {
                                assert_eq!(left.span.text(source), "10")
                            }
                            _ => panic!("Expected Int, got {:?}", left),
                        }
                        match &right.kind {
                            ExprKind::Int(_) => assert_eq!(right.span.text(source), "5"),
                            _ => panic!("Expected Int, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression for (10 + 5), got {:?}", right),
                }
            }
            _ => panic!("Expected binary expression, got {:?}", value),
        },
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
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
    let (program, interner) = parse_ok(source);

    // Check that we have all 5 functions
    assert_eq!(program.functions.len(), 5);

    // Check function names
    let function_names: Vec<&str> = program
        .functions
        .iter()
        .map(|f| interner.resolve(&f.name.sym))
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
        .find(|f| interner.resolve(&f.name.sym) == "main")
        .expect("main function should exist");

    // Verify main has statements
    assert!(!main_func.body.stmts.is_empty());

    // Quick check that we have function calls in the AST
    let debug_str = format!("{:#?}", program);
    assert!(debug_str.contains("FunctionCall"));

    // Check that level1 function calls level2
    let level1_func = program
        .functions
        .iter()
        .find(|f| interner.resolve(&f.name.sym) == "level1")
        .expect("level1 function should exist");

    // Find the function call to level2 in level1
    let has_level2_call = level1_func.body.stmts.iter().any(|stmt| match stmt {
        Stmt::Let { value, .. } => matches!(
            &value.kind,
            ExprKind::FunctionCall { path, .. } if path.segments.len() == 1 && interner.resolve(&path.segments[0].sym) == "level2"
        ),
        _ => false,
    });
    assert!(has_level2_call, "level1 should call level2");
}

#[test]
fn test_both_parsers_produce_same_result() {
    let source = "fn main() { let result = 2 + 3 * 4\nprintln(result) }";

    // Parse with the unified parser
    let (program, interner) = parse_ok(source);

    // Basic structure check
    assert_eq!(program.functions.len(), 1);
    assert_eq!(interner.resolve(&program.functions[0].name.sym), "main");
}

#[test]
fn test_whitespace_ignored() {
    // Test that whitespace is completely ignored during parsing
    let messy_input = "fn   main (  )   {   let   x :  i32   =   2   +   3   *   4   }";
    let clean_input = "fn main() { let x: i32 = 2 + 3 * 4 }";

    let (messy_program, messy_interner) = parse_ok(messy_input);
    let (clean_program, clean_interner) = parse_ok(clean_input);

    // Both should produce structurally identical ASTs (spans will differ due to whitespace)
    assert_eq!(messy_program.functions.len(), clean_program.functions.len());
    assert_eq!(messy_program.functions.len(), 1);
    assert_eq!(
        messy_interner.resolve(&messy_program.functions[0].name.sym),
        "main"
    );
    assert_eq!(
        clean_interner.resolve(&clean_program.functions[0].name.sym),
        "main"
    );

    // Test that the arithmetic expression is parsed correctly in both cases
    if let Some(Stmt::Let { value, .. }) = messy_program.functions[0].body.stmts.first() {
        // Should be parsed as 2 + (3 * 4)
        if let ExprKind::Binary {
            left,
            op: BinaryOp::Add,
            right,
        } = &value.kind
        {
            assert!(matches!(left.kind, ExprKind::Int(_)));
            assert!(matches!(
                &right.kind,
                ExprKind::Binary {
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
    let (program, interner) = parse_ok(source);

    // Check that we have one struct and one function
    assert_eq!(program.structs.len(), 1);
    assert_eq!(program.functions.len(), 1);

    // Check struct definition
    let point_struct = &program.structs[0];
    assert_eq!(interner.resolve(&point_struct.name.sym), "Point");
    assert_eq!(point_struct.fields.len(), 2);

    // Check first field
    assert_eq!(interner.resolve(&point_struct.fields[0].name.sym), "x");
    assert_eq!(point_struct.fields[0].field_type, Type::I32);

    // Check second field
    assert_eq!(interner.resolve(&point_struct.fields[1].name.sym), "y");
    assert_eq!(point_struct.fields[1].field_type, Type::I32);

    // Check main function has struct literal and field access
    let main_func = &program.functions[0];
    // The let statement and the println call are both statements now.
    assert_eq!(main_func.body.stmts.len(), 2);

    // Check struct literal in let statement
    if let Stmt::Let { value, .. } = &main_func.body.stmts[0] {
        if let ExprKind::StructLiteral { name, fields } = &value.kind {
            assert_eq!(interner.resolve(&name.sym), "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(interner.resolve(&fields[0].name.sym), "x");
            assert_eq!(interner.resolve(&fields[1].name.sym), "y");
        } else {
            panic!("Expected struct literal in let statement");
        }
    } else {
        panic!("Expected let statement");
    }

    // Dotted identifier chains parse as paths. Lowering decides whether
    // this is a value field access or a module/enum path.
    let expr = tail_expr(&main_func.body);
    if let ExprKind::FunctionCall { args, .. } = &expr.kind {
        if let ExprKind::Path(path) = &args[0].kind {
            assert_eq!(path.segments.len(), 2);
            assert_eq!(interner.resolve(&path.segments[0].sym), "p");
            assert_eq!(interner.resolve(&path.segments[1].sym), "x");
        } else {
            panic!("Expected path in println");
        }
    } else {
        panic!("Expected function call");
    }
}

#[test]
fn test_parse_field_access() {
    let source = "fn main() { let x = point.x }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    if let Stmt::Let { value, .. } = &main_func.body.stmts[0] {
        if let ExprKind::Path(path) = &value.kind {
            assert_eq!(path.segments.len(), 2);
            assert_eq!(interner.resolve(&path.segments[0].sym), "point");
            assert_eq!(interner.resolve(&path.segments[1].sym), "x");
        } else {
            panic!("Expected path expression");
        }
    } else {
        panic!("Expected let statement");
    }
}

#[test]
fn test_parse_struct_literal() {
    let source = r#"fn main() { let p = Point { x = 10, y = 20 } }"#;
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    if let Stmt::Let { value, .. } = &main_func.body.stmts[0] {
        if let ExprKind::StructLiteral { name, fields } = &value.kind {
            assert_eq!(interner.resolve(&name.sym), "Point");
            assert_eq!(fields.len(), 2);

            // Check first field
            assert_eq!(interner.resolve(&fields[0].name.sym), "x");
            if let ExprKind::Int(_) = &fields[0].value.kind {
                assert_eq!(fields[0].value.span.text(source), "10");
            } else {
                panic!("Expected integer literal for x field");
            }

            // Check second field
            assert_eq!(interner.resolve(&fields[1].name.sym), "y");
            if let ExprKind::Int(_) = &fields[1].value.kind {
                assert_eq!(fields[1].value.span.text(source), "20");
            } else {
                panic!("Expected integer literal for y field");
            }
        } else {
            panic!("Expected struct literal");
        }
    } else {
        panic!("Expected let statement");
    }
}

#[test]
fn test_parse_struct_type_annotation() {
    let source = "fn main() { let p: Point = get_point() }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    if let Stmt::Let {
        type_annotation: Some(Type::Struct(name, _)),
        ..
    } = &main_func.body.stmts[0]
    {
        assert_eq!(interner.resolve(name), "Point");
    } else {
        panic!("Expected struct type annotation");
    }
}

#[test]
fn test_parse_pointer_types() {
    // Test const pointer
    let source = "fn main() { let ptr: *const u8 = get_ptr() }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    if let Stmt::Let {
        type_annotation: Some(Type::Pointer { mutable, pointee }),
        ..
    } = &main_func.body.stmts[0]
    {
        assert!(!mutable);
        assert!(matches!(**pointee, Type::U8));
    } else {
        panic!("Expected const pointer type annotation");
    }

    // Test mutable pointer
    let source_mut = "fn main() { let ptr: *mut i32 = get_ptr() }";
    let (program_mut, _) = parse_ok(source_mut);

    let main_func_mut = &program_mut.functions[0];
    if let Stmt::Let {
        type_annotation: Some(Type::Pointer { mutable, pointee }),
        ..
    } = &main_func_mut.body.stmts[0]
    {
        assert!(mutable);
        assert!(matches!(**pointee, Type::I32));
    } else {
        panic!("Expected mutable pointer type annotation");
    }
}

#[test]
fn test_parse_dereference() {
    let source = "fn main() { let value = *ptr }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    if let Stmt::Let { value, .. } = &main_func.body.stmts[0] {
        if let ExprKind::Dereference(operand) = &value.kind {
            if let ExprKind::Ident(name) = &operand.kind {
                assert_eq!(interner.resolve(&name.sym), "ptr");
            } else {
                panic!("Expected identifier in dereference operand");
            }
        } else {
            panic!("Expected dereference expression");
        }
    } else {
        panic!("Expected let statement with dereference expression");
    }
}

#[test]
fn test_import_decl_variants() {
    let source = "import foo.bar\nimport foo.bar.Baz\nimport foo.bar.{Baz, Quux}\nfn main() {}";
    let (program, interner) = parse_ok(source);
    assert_eq!(program.imports.len(), 3);

    let module_import = &program.imports[0];
    assert_eq!(
        module_import.module_segments(&interner),
        vec!["foo".to_string(), "bar".to_string()]
    );
    assert!(matches!(module_import.selector, ImportSelector::All));
    assert_eq!(
        module_import
            .trailing_symbol
            .as_ref()
            .map(|ident| interner.resolve(&ident.sym)),
        Some("bar")
    );

    let trailing_symbol_import = &program.imports[1];
    assert_eq!(
        trailing_symbol_import.module_segments(&interner),
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
            .map(|ident| interner.resolve(&ident.sym)),
        Some("Baz")
    );

    let brace_import = &program.imports[2];
    assert_eq!(
        brace_import.module_segments(&interner),
        vec!["foo".to_string(), "bar".to_string()]
    );
    match &brace_import.selector {
        ImportSelector::Named(names) => {
            let texts: Vec<_> = names
                .iter()
                .map(|ident| interner.resolve(&ident.sym))
                .collect();
            assert_eq!(texts, vec!["Baz", "Quux"]);
        }
        _ => panic!("expected named selector"),
    }
    assert!(brace_import.trailing_symbol.is_none());
}

#[test]
fn test_definition_spans_include_attributes() {
    let struct_source = "@repr(\"C\")\nstruct Foo { }";
    let (struct_program, _) = parse_ok(struct_source);
    let def = &struct_program.structs[0];
    assert_eq!(def.span.text(struct_source), "@repr(\"C\")\nstruct Foo { }");

    let func_source = "@runtime(\"puts\")\nfn print(message: Str);";
    let (func_program, _) = parse_ok(func_source);
    let func = &func_program.functions[0];
    assert_eq!(
        func.span.text(func_source),
        "@runtime(\"puts\")\nfn print(message: Str);"
    );
}

#[test]
fn test_trait_and_impl_spans() {
    let source = "trait Greeter { fn greet(person: Person); }\n\nimpl Greeter for Person {\n    fn greet(person: Person) {}\n}\n";
    let (program, _) = parse_ok(source);
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
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    assert_eq!(main_func.body.stmts.len(), 1);
    match &main_func.body.stmts[0] {
        Stmt::Let {
            pattern: Pattern::Binding { name, mutable, .. },
            type_annotation,
            value,
            ..
        } => {
            assert_eq!(interner.resolve(&name.sym), "x");
            assert!(*mutable);
            assert_eq!(type_annotation, &Some(Type::I32));
            match &value.kind {
                ExprKind::Int(_) => assert_eq!(value.span.text(source), "42"),
                _ => panic!("Expected Int, got {:?}", value),
            }
        }
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_parse_let_mut_without_type() {
    let source = "fn main() { let mut counter = 0 }";
    let (program, interner) = parse_ok(source);

    let main_func = &program.functions[0];
    match &main_func.body.stmts[0] {
        Stmt::Let {
            pattern: Pattern::Binding { name, mutable, .. },
            type_annotation,
            ..
        } => {
            assert_eq!(interner.resolve(&name.sym), "counter");
            assert!(*mutable);
            assert_eq!(type_annotation, &None);
        }
        _ => panic!("Expected let statement, got {:?}", &main_func.body.stmts[0]),
    }
}

#[test]
fn test_if_condition_no_struct_literal_ambiguity() {
    // `if x { }` should parse as: if (x) { }, not if (x { }) which would be a struct literal
    let source = "fn main() { if x { } }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    // The `if` is now a statement (blocks carry no trailing value).
    let expr = tail_expr(&main_func.body);
    match &expr.kind {
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            // Condition should be a simple identifier, not a struct literal
            match &condition.kind {
                ExprKind::Ident(_) => {
                    assert_eq!(condition.span.text(source), "x");
                }
                _ => panic!("Expected identifier in condition, got {:?}", condition),
            }
            assert!(then_branch.stmts.is_empty() && then_branch.expr.is_none());
            assert!(else_branch.is_none());
        }
        other => panic!("Expected if expression, got {:?}", other),
    }
}

#[test]
fn test_while_condition_no_struct_literal_ambiguity() {
    // `while x { }` should parse as: while (x) { }, not while (x { }) which would be a struct literal
    let source = "fn main() { while x { } }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    assert_eq!(main_func.body.stmts.len(), 1);
    match &main_func.body.stmts[0] {
        Stmt::While {
            condition, body, ..
        } => {
            // Condition should be a simple identifier, not a struct literal
            match &condition.kind {
                ExprKind::Ident(_) => {
                    assert_eq!(condition.span.text(source), "x");
                }
                _ => panic!("Expected identifier in condition, got {:?}", condition),
            }
            assert!(body.is_empty());
        }
        _ => panic!(
            "Expected while statement, got {:?}",
            &main_func.body.stmts[0]
        ),
    }
}

#[test]
fn test_while_condition_with_comparison() {
    // `while x < y { }` should parse the full comparison as the condition
    let source = "fn main() { while x < y { } }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    assert_eq!(main_func.body.stmts.len(), 1);
    match &main_func.body.stmts[0] {
        Stmt::While {
            condition, body, ..
        } => {
            match &condition.kind {
                ExprKind::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Less);
                    match &left.kind {
                        ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                        _ => panic!("Expected identifier for left, got {:?}", left),
                    }
                    match &right.kind {
                        ExprKind::Ident(_) => assert_eq!(right.span.text(source), "y"),
                        _ => panic!("Expected identifier for right, got {:?}", right),
                    }
                }
                _ => panic!(
                    "Expected binary expression in condition, got {:?}",
                    condition
                ),
            }
            assert!(body.is_empty());
        }
        _ => panic!(
            "Expected while statement, got {:?}",
            &main_func.body.stmts[0]
        ),
    }
}

#[test]
fn test_if_condition_with_comparison() {
    // `if x < y { }` should parse the full comparison as the condition
    let source = "fn main() { if x < y { } }";
    let (program, _) = parse_ok(source);

    let main_func = &program.functions[0];
    // The `if` is now a statement (blocks carry no trailing value).
    let expr = tail_expr(&main_func.body);
    match &expr.kind {
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            match &condition.kind {
                ExprKind::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Less);
                    match &left.kind {
                        ExprKind::Ident(_) => assert_eq!(left.span.text(source), "x"),
                        _ => panic!("Expected identifier for left, got {:?}", left),
                    }
                    match &right.kind {
                        ExprKind::Ident(_) => assert_eq!(right.span.text(source), "y"),
                        _ => panic!("Expected identifier for right, got {:?}", right),
                    }
                }
                _ => panic!(
                    "Expected binary expression in condition, got {:?}",
                    condition
                ),
            }
            assert!(then_branch.stmts.is_empty() && then_branch.expr.is_none());
            assert!(else_branch.is_none());
        }
        _ => panic!("Expected if expression, got {:?}", &main_func.body.expr),
    }
}

#[test]
fn test_go_continuation_after_trailing_operator() {
    // A line ending in a binary operator continues onto the next line.
    let source = "fn main() {\n    let x = 1 +\n        2\n}";
    let (program, _) = parse_ok(source);
    match &program.functions[0].body.stmts[0] {
        Stmt::Let { value, .. } => assert!(
            matches!(value.kind, ExprKind::Binary { .. }),
            "expected `1 + 2` to parse as one binary expression, got {:?}",
            value.kind
        ),
        other => panic!("expected let, got {other:?}"),
    }
}

#[test]
fn test_go_chain_continues_on_trailing_dot() {
    // A line ending in `.` continues the chain onto the next line.
    let source = "fn main() {\n    let a = p.\n        x\n}";
    let (program, _) = parse_ok(source);
    match &program.functions[0].body.stmts[0] {
        // `p.x` spanning the trailing `.` parses as one two-segment access
        // (a path here, since both sides are identifiers), not a bare `p`.
        Stmt::Let { value, .. } => match &value.kind {
            ExprKind::Path(path) => assert_eq!(
                path.segments.len(),
                2,
                "expected `p.x` to continue across the trailing dot, got {value:?}"
            ),
            other => panic!("expected `p.x` access, got {other:?}"),
        },
        other => panic!("expected let, got {other:?}"),
    }
}

#[test]
fn test_go_chain_breaks_on_leading_dot() {
    // A line ending in an identifier ends the statement, so a `.` starting the
    // next line is not a continuation — it is a parse error.
    let source = "fn main() {\n    let a = p\n        .x\n}";
    let (result, _) = parse(source, &Interner::new());
    assert!(
        result.is_err(),
        "expected leading-dot continuation to be rejected"
    );
}

#[test]
fn test_go_return_newline_is_bare_return() {
    // `return` ends a statement, so a value on the next line is not captured.
    let source = "fn f() -> i32 {\n    return\n    42\n}";
    let (program, _) = parse_ok(source);
    let has_bare_return = program.functions[0]
        .body
        .stmts
        .iter()
        .any(|s| matches!(s, Stmt::Return { value: None, .. }));
    assert!(
        has_bare_return,
        "expected `return` to parse as a bare return"
    );
}

#[test]
fn test_go_return_value_on_same_line() {
    // A value on the same line as `return` is captured.
    let source = "fn f() -> i32 {\n    return 42\n}";
    let (program, _) = parse_ok(source);
    let has_value_return = program.functions[0]
        .body
        .stmts
        .iter()
        .any(|s| matches!(s, Stmt::Return { value: Some(_), .. }));
    assert!(
        has_value_return,
        "expected `return 42` to capture its value"
    );
}
