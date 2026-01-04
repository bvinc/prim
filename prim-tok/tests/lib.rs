use prim_tok::{TokenError, TokenKind, Tokenizer};

#[test]
fn test_simple_tokens() {
    let src = "let x = 42";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[0].span.text(src), "let");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].span.text(src), "x");
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[2].span.text(src), "=");
    assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[3].span.text(src), "42");
}

#[test]
fn test_type_annotations() {
    let src = "let x: u32 = 0u32";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[0].span.text(src), "let");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].span.text(src), "x");
    assert_eq!(tokens[2].kind, TokenKind::Colon);
    assert_eq!(tokens[2].span.text(src), ":");
    assert_eq!(tokens[3].kind, TokenKind::U32);
    assert_eq!(tokens[3].span.text(src), "u32");
    assert_eq!(tokens[4].kind, TokenKind::Equals);
    assert_eq!(tokens[4].span.text(src), "=");
    assert_eq!(tokens[5].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[5].span.text(src), "0u32");
}

#[test]
fn test_function_signature() {
    let src = "fn double(x: u32) -> u32";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    // Whitespace is now automatically filtered out during tokenization
    assert_eq!(tokens[0].kind, TokenKind::Fn);
    assert_eq!(tokens[0].span.text(src), "fn");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].span.text(src), "double");
    assert_eq!(tokens[2].kind, TokenKind::LeftParen);
    assert_eq!(tokens[2].span.text(src), "(");
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].span.text(src), "x");
    assert_eq!(tokens[4].kind, TokenKind::Colon);
    assert_eq!(tokens[4].span.text(src), ":");
    assert_eq!(tokens[5].kind, TokenKind::U32);
    assert_eq!(tokens[5].span.text(src), "u32");
    assert_eq!(tokens[6].kind, TokenKind::RightParen);
    assert_eq!(tokens[6].span.text(src), ")");
    assert_eq!(tokens[7].kind, TokenKind::Arrow);
    assert_eq!(tokens[7].span.text(src), "->");
    assert_eq!(tokens[8].kind, TokenKind::U32);
    assert_eq!(tokens[8].span.text(src), "u32");
}

#[test]
fn test_line_comments() {
    let src = "let x = 42 // this is a comment";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[4].kind, TokenKind::Comment);
    assert_eq!(tokens[4].span.text(src), "// this is a comment");
}

#[test]
fn test_line_comment_at_end_with_newline() {
    let src = "let x = 42 // comment\nlet y = 5";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[4].kind, TokenKind::Comment);
    assert_eq!(tokens[4].span.text(src), "// comment");
    assert_eq!(tokens[5].kind, TokenKind::Let);
    assert_eq!(tokens[6].kind, TokenKind::Identifier);
    assert_eq!(tokens[7].kind, TokenKind::Equals);
    assert_eq!(tokens[8].kind, TokenKind::IntLiteral);
}

#[test]
fn test_division_vs_comments() {
    let mut tokenizer = Tokenizer::new("let x = a / b");
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[4].kind, TokenKind::Slash);
    assert_eq!(tokens[5].kind, TokenKind::Identifier);
}

#[test]
fn test_pointer_keywords() {
    let src = "*const u8 *mut i32";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::UnaryStar);
    assert_eq!(tokens[0].span.text(src), "*");
    assert_eq!(tokens[1].kind, TokenKind::Const);
    assert_eq!(tokens[1].span.text(src), "const");
    assert_eq!(tokens[2].kind, TokenKind::U8);
    assert_eq!(tokens[2].span.text(src), "u8");
    assert_eq!(tokens[3].kind, TokenKind::UnaryStar);
    assert_eq!(tokens[3].span.text(src), "*");
    assert_eq!(tokens[4].kind, TokenKind::Mut);
    assert_eq!(tokens[4].span.text(src), "mut");
    assert_eq!(tokens[5].kind, TokenKind::I32);
    assert_eq!(tokens[5].span.text(src), "i32");
}

#[test]
fn test_loop_and_break_tokens() {
    let src = "loop { break looped }";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Loop);
    assert_eq!(tokens[1].kind, TokenKind::LeftBrace);
    assert_eq!(tokens[2].kind, TokenKind::Break);
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].span.text(src), "looped");
}

#[test]
fn test_string_literals() {
    let src = r#"let s = "hello world""#;
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
    assert_eq!(tokens[3].span.text(src), r#""hello world""#);
}

#[test]
fn test_string_with_escapes() {
    let src = r#"let s = "hello\nworld\t!""#;
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
    assert_eq!(tokens[3].span.text(src), r#""hello\nworld\t!""#);
}

#[test]
fn test_string_with_escaped_quote() {
    let src = r#"let s = "say \"hello\"""#;
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
    assert_eq!(tokens[3].span.text(src), r#""say \"hello\"""#);
}

#[test]
fn test_empty_string() {
    let src = r#"let s = """#;
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
    assert_eq!(tokens[3].span.text(src), r#""""#);
}

#[test]
fn test_unterminated_string() {
    let mut tokenizer = Tokenizer::new(r#"let s = "unterminated"#);
    let result = tokenizer.tokenize();

    match result {
        Err(TokenError::UnterminatedString { position }) => {
            assert_eq!(position, 8); // Position of opening quote
        }
        _ => panic!("Expected UnterminatedString error, got {:?}", result),
    }
}

#[test]
fn test_char_literals() {
    let src = "let c = 'a'";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::CharLiteral);
    assert_eq!(tokens[3].span.text(src), "'a'");
}

#[test]
fn test_escaped_char_literals() {
    let test_cases = vec![
        ("'\\n'", r"'\n'"),
        ("'\\t'", r"'\t'"),
        ("'\\''", r"'\''"),
        ("'\\\\'", r"'\\'"),
    ];

    for (input, expected) in test_cases {
        let input_string = format!("let c = {}", input);
        let mut tokenizer = Tokenizer::new(&input_string);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::CharLiteral);
        assert_eq!(
            tokens[3].span.text(&input_string),
            expected,
            "Failed for input: {}",
            input
        );
    }
}

#[test]
fn test_unterminated_char_literal() {
    let mut tokenizer = Tokenizer::new("let c = 'a");
    let result = tokenizer.tokenize();

    match result {
        Err(TokenError::UnterminatedString { position }) => {
            assert_eq!(position, 8); // Position of opening quote
        }
        _ => panic!("Expected UnterminatedString error, got {:?}", result),
    }
}

#[test]
fn test_if_keyword() {
    let src = "if x == 42";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::If);
    assert_eq!(tokens[0].span.text(src), "if");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::DoubleEquals);
    assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
}

#[test]
fn test_mixed_literals() {
    let src = r#"let x = 42; let s = "hello"; let c = 'a'"#;
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    // First assignment: let x = 42
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[4].kind, TokenKind::Semicolon);

    // Second assignment: let s = "hello"
    assert_eq!(tokens[5].kind, TokenKind::Let);
    assert_eq!(tokens[6].kind, TokenKind::Identifier);
    assert_eq!(tokens[7].kind, TokenKind::Equals);
    assert_eq!(tokens[8].kind, TokenKind::StringLiteral);
    assert_eq!(tokens[8].span.text(src), r#""hello""#);
    assert_eq!(tokens[9].kind, TokenKind::Semicolon);

    // Third assignment: let c = 'a'
    assert_eq!(tokens[10].kind, TokenKind::Let);
    assert_eq!(tokens[11].kind, TokenKind::Identifier);
    assert_eq!(tokens[12].kind, TokenKind::Equals);
    assert_eq!(tokens[13].kind, TokenKind::CharLiteral);
    assert_eq!(tokens[13].span.text(src), "'a'");
}

#[test]
fn test_boolean_literals() {
    let src = "let flag = true";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::True);
    assert_eq!(tokens[3].span.text(src), "true");
}

#[test]
fn test_false_literal() {
    let src = "let flag = false";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[3].kind, TokenKind::False);
    assert_eq!(tokens[3].span.text(src), "false");
}

#[test]
fn test_bool_type() {
    let src = "let flag: bool = true";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Colon);
    assert_eq!(tokens[3].kind, TokenKind::Bool);
    assert_eq!(tokens[3].span.text(src), "bool");
    assert_eq!(tokens[4].kind, TokenKind::Equals);
    assert_eq!(tokens[5].kind, TokenKind::True);
}

#[test]
fn test_boolean_expressions() {
    let mut tokenizer = Tokenizer::new("if flag == true");
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::If);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::DoubleEquals);
    assert_eq!(tokens[3].kind, TokenKind::True);
}

#[test]
fn test_mixed_boolean_and_other_literals() {
    let mut tokenizer = Tokenizer::new(r#"let x = 42; let flag = true; let name = "test""#);
    let tokens = tokenizer.tokenize().unwrap();

    // First: let x = 42
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Equals);
    assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[4].kind, TokenKind::Semicolon);

    // Second: let flag = true
    assert_eq!(tokens[5].kind, TokenKind::Let);
    assert_eq!(tokens[6].kind, TokenKind::Identifier);
    assert_eq!(tokens[7].kind, TokenKind::Equals);
    assert_eq!(tokens[8].kind, TokenKind::True);
    assert_eq!(tokens[9].kind, TokenKind::Semicolon);

    // Third: let name = "test"
    assert_eq!(tokens[10].kind, TokenKind::Let);
    assert_eq!(tokens[11].kind, TokenKind::Identifier);
    assert_eq!(tokens[12].kind, TokenKind::Equals);
    assert_eq!(tokens[13].kind, TokenKind::StringLiteral);
}

#[test]
fn test_boolean_in_function_parameters() {
    let mut tokenizer = Tokenizer::new("fn test(active: bool, count: u32)");
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Fn);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::LeftParen);
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[4].kind, TokenKind::Colon);
    assert_eq!(tokens[5].kind, TokenKind::Bool);
    assert_eq!(tokens[6].kind, TokenKind::Comma);
    assert_eq!(tokens[7].kind, TokenKind::Identifier);
    assert_eq!(tokens[8].kind, TokenKind::Colon);
    assert_eq!(tokens[9].kind, TokenKind::U32);
    assert_eq!(tokens[10].kind, TokenKind::RightParen);
}

#[test]
fn test_boolean_keywords_case_sensitive() {
    // Should tokenize as identifiers, not boolean keywords
    let src = "True False BOOL";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[0].span.text(src), "True");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].span.text(src), "False");
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].span.text(src), "BOOL");
}

#[test]
fn test_boolean_as_part_of_identifier() {
    // Should tokenize as identifiers, not split into keywords
    let src = "truthy falsey boolean";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[0].span.text(src), "truthy");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].span.text(src), "falsey");
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].span.text(src), "boolean");
}

#[test]
fn test_dot_token() {
    let src = "point.x";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[0].span.text(src), "point");
    assert_eq!(tokens[1].kind, TokenKind::Dot);
    assert_eq!(tokens[1].span.text(src), ".");
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].span.text(src), "x");
}

#[test]
fn test_dot_vs_float() {
    // Test that standalone dots are tokenized as dots
    let src = "x.field";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();
    assert_eq!(tokens[1].kind, TokenKind::Dot);

    // Test that dots in numbers are part of float literals
    let src2 = "3.14";
    let mut tokenizer = Tokenizer::new(src2);
    let tokens = tokenizer.tokenize().unwrap();
    assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
    assert_eq!(tokens[0].span.text(src2), "3.14");
}

#[test]
fn test_leading_dot_float() {
    let src = ".5";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();
    assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
    assert_eq!(tokens[0].span.text(src), ".5");
}

#[test]
fn test_exponent_float_tokens() {
    let src = "12E+99_f64 2. 0.1f32";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();
    assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
    assert_eq!(tokens[0].span.text(src), "12E+99_f64");
    assert_eq!(tokens[1].kind, TokenKind::FloatLiteral);
    assert_eq!(tokens[1].span.text(src), "2.");
    assert_eq!(tokens[2].kind, TokenKind::FloatLiteral);
    assert_eq!(tokens[2].span.text(src), "0.1f32");
}

#[test]
fn test_all_float_literal_formats() {
    let literals = [
        ".5",
        "0.5",
        "5.",
        "123.0f64",
        "0.1f64",
        "0.1f32",
        "12E+99_f64",
        "6e10",
        "6E-10",
        "6.0e10",
        "6.0E-10",
        "1_000.25",
        "1_000.25_f32",
    ];
    let src = literals.join(" ");
    let mut tokenizer = Tokenizer::new(&src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens.len(), literals.len());
    for (token, literal) in tokens.iter().zip(literals.iter()) {
        assert_eq!(token.kind, TokenKind::FloatLiteral);
        assert_eq!(token.span.text(&src), *literal);
    }
}

#[test]
fn test_all_integer_literal_formats() {
    let literals = [
        "0",
        "42",
        "1_000_000",
        "0b1010",
        "0b1010_0110",
        "0o755",
        "0o7_55",
        "0xdeadbeef",
        "0xdead_beef",
        "5i32",
        "10i64",
        "255u8",
        "1usize",
        "1isize",
        "1u8",
        "1i8",
        "1u16",
        "1i16",
        "1u32",
        "1i32",
        "1u64",
        "1i64",
    ];
    let src = literals.join(" ");
    let mut tokenizer = Tokenizer::new(&src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens.len(), literals.len());
    for (token, literal) in tokens.iter().zip(literals.iter()) {
        assert_eq!(token.kind, TokenKind::IntLiteral);
        assert_eq!(token.span.text(&src), *literal);
    }
}

#[test]
fn test_struct_tokens() {
    let src = "struct Point { x: i32, y: i32 }";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Struct);
    assert_eq!(tokens[0].span.text(src), "struct");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].span.text(src), "Point");
    assert_eq!(tokens[2].kind, TokenKind::LeftBrace);
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].span.text(src), "x");
    assert_eq!(tokens[4].kind, TokenKind::Colon);
    assert_eq!(tokens[5].kind, TokenKind::I32);
    assert_eq!(tokens[6].kind, TokenKind::Comma);
    assert_eq!(tokens[7].kind, TokenKind::Identifier);
    assert_eq!(tokens[7].span.text(src), "y");
    assert_eq!(tokens[8].kind, TokenKind::Colon);
    assert_eq!(tokens[9].kind, TokenKind::I32);
    assert_eq!(tokens[10].kind, TokenKind::RightBrace);
}

#[test]
fn test_field_access_tokens() {
    let src = "p.x";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[0].span.text(src), "p");
    assert_eq!(tokens[1].kind, TokenKind::Dot);
    assert_eq!(tokens[1].span.text(src), ".");
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].span.text(src), "x");
}

#[test]
fn test_struct_literal_tokens() {
    let src = "Point { x: 10, y: 20 }";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[0].span.text(src), "Point");
    assert_eq!(tokens[1].kind, TokenKind::LeftBrace);
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].span.text(src), "x");
    assert_eq!(tokens[3].kind, TokenKind::Colon);
    assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[4].span.text(src), "10");
    assert_eq!(tokens[5].kind, TokenKind::Comma);
    assert_eq!(tokens[6].kind, TokenKind::Identifier);
    assert_eq!(tokens[6].span.text(src), "y");
    assert_eq!(tokens[7].kind, TokenKind::Colon);
    assert_eq!(tokens[8].kind, TokenKind::IntLiteral);
    assert_eq!(tokens[8].span.text(src), "20");
    assert_eq!(tokens[9].kind, TokenKind::RightBrace);
}
