use prim_codegen::{CodegenError, generate_object_code};
use prim_parse::{ParseError, parse};
use prim_tok::TokenError;

#[test]
fn test_tokenizer_error_unexpected_character() {
    let result = parse("fn main() { let x = @ }");

    match result {
        Err(ParseError::TokenError(TokenError::UnexpectedCharacter { ch, position })) => {
            assert_eq!(ch, '@');
            assert_eq!(position, 20);
        }
        _ => panic!("Expected TokenError::UnexpectedCharacter, got {:?}", result),
    }
}

#[test]
fn test_parser_error_unexpected_token() {
    let result = parse("fn main() { let = 42 }");

    match result {
        Err(ParseError::UnexpectedToken {
            expected,
            found,
            position,
        }) => {
            assert_eq!(expected, "identifier");
            assert_eq!(found, prim_tok::TokenKind::Equals);
            assert_eq!(position, 16);
        }
        _ => panic!("Expected ParseError::UnexpectedToken, got {:?}", result),
    }
}

#[test]
fn test_codegen_error_undefined_variable() {
    let program = parse("fn main() { let x = unknown_var }").unwrap();
    let result = generate_object_code(&program);

    match result {
        Err(CodegenError::UndefinedVariable { name, context: _ }) => {
            assert_eq!(name, "unknown_var");
        }
        _ => panic!("Expected CodegenError::UndefinedVariable, got {:?}", result),
    }
}

#[test]
fn test_codegen_error_unsupported_function() {
    let program = parse("fn main() { unsupported_func() }").unwrap();
    let result = generate_object_code(&program);

    match result {
        Err(CodegenError::UnsupportedFunctionCall { name, context: _ }) => {
            assert_eq!(name, "unsupported_func");
        }
        _ => panic!(
            "Expected CodegenError::UnsupportedFunctionCall, got {:?}",
            result
        ),
    }
}

#[test]
fn test_successful_compilation() {
    let program = parse("fn main() { let x: u32 = 42\nprintln(x) }").unwrap();
    let result = generate_object_code(&program);

    assert!(
        result.is_ok(),
        "Expected successful compilation, got {:?}",
        result
    );
    let object_code = result.unwrap();
    assert!(!object_code.is_empty(), "Object code should not be empty");
}

#[test]
fn test_arithmetic_expression() {
    let program = parse("fn main() { let result = 2 + 3 * 4\nprintln(result) }").unwrap();
    let result = generate_object_code(&program);

    assert!(
        result.is_ok(),
        "Expected successful compilation, got {:?}",
        result
    );
}

#[test]
fn test_parser_error_missing_main() {
    let result = parse("fn foo() { let x = 42 }");

    match result {
        Err(ParseError::MissingMainFunction) => {}
        _ => panic!("Expected ParseError::MissingMainFunction, got {:?}", result),
    }
}

#[test]
fn test_parser_error_statements_outside_function() {
    let result = parse("let x = 42");

    match result {
        Err(ParseError::StatementsOutsideFunction) => {}
        _ => panic!(
            "Expected ParseError::StatementsOutsideFunction, got {:?}",
            result
        ),
    }
}

#[test]
fn test_multiple_functions_compilation() {
    let program =
        parse("fn helper() -> u32 { let x = 10\nlet y = 5\nx + y }\nfn main() { println(42) }")
            .unwrap();
    let result = generate_object_code(&program);

    assert!(
        result.is_ok(),
        "Expected successful compilation with multiple functions, got {:?}",
        result
    );
}

#[test]
fn test_function_with_return_type() {
    let program =
        parse("fn add(a: i64, b: i64) -> i64 { a + b }\nfn main() { println(5) }").unwrap();
    let result = generate_object_code(&program);

    assert!(
        result.is_ok(),
        "Expected successful compilation with function parameters and return type, got {:?}",
        result
    );
}

#[test]
fn test_cli_build_command() {
    use std::fs;
    use std::process::Command;

    // Create a simple test program
    let test_program = "fn main() { println(42) }";
    let test_file = "test_cli_build.prim";
    fs::write(test_file, test_program).expect("Failed to write test file");

    // Test build command
    let output = Command::new("cargo")
        .args(["run", "--", "build", test_file])
        .output()
        .expect("Failed to execute build command");

    assert!(
        output.status.success(),
        "Build command failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check that executable was created
    let executable_name = "test_cli_build";
    assert!(
        fs::metadata(executable_name).is_ok(),
        "Executable was not created"
    );

    // Test that executable runs correctly
    let run_output = Command::new(format!("./{}", executable_name))
        .output()
        .expect("Failed to run generated executable");

    assert!(
        run_output.status.success(),
        "Generated executable failed to run"
    );
    assert_eq!(String::from_utf8_lossy(&run_output.stdout).trim(), "42");

    // Clean up
    let _ = fs::remove_file(test_file);
    let _ = fs::remove_file(executable_name);
}

#[test]
fn test_cli_run_command() {
    use std::fs;
    use std::process::Command;

    // Create a simple test program
    let test_program = "fn main() { println(123) }";
    let test_file = "test_cli_run.prim";
    fs::write(test_file, test_program).expect("Failed to write test file");

    // Test run command
    let output = Command::new("cargo")
        .args(["run", "--", "run", test_file])
        .output()
        .expect("Failed to execute run command");

    assert!(
        output.status.success(),
        "Run command failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "123");

    // Clean up
    let _ = fs::remove_file(test_file);
}

#[test]
fn test_unified_error_handling() {
    // Test that error codes and categories work correctly
    let parse_result = parse("let x = @");
    match parse_result {
        Err(err) => {
            assert_eq!(err.error_code(), "TOK001"); // UnexpectedCharacter
            assert_eq!(err.category(), "Tokenization");
            assert!(err.position().is_some());
            assert_eq!(err.context(), Some("character scanning"));
        }
        Ok(_) => panic!("Expected error for invalid syntax"),
    }

    // Test parse error codes
    let parse_result = parse("fn main() { let = 42 }");
    match parse_result {
        Err(err) => {
            assert_eq!(err.error_code(), "PAR001"); // UnexpectedToken
            assert_eq!(err.category(), "Parsing");
            assert!(err.position().is_some());
            assert_eq!(err.context(), Some("syntax parsing"));
        }
        Ok(_) => panic!("Expected error for invalid syntax"),
    }

    // Test codegen error codes
    let program = parse("fn main() { let x = unknown_var }").unwrap();
    let result = generate_object_code(&program);
    match result {
        Err(err) => {
            assert_eq!(err.error_code(), "COD001"); // UndefinedVariable
            assert_eq!(err.category(), "Code Generation");
            assert_eq!(err.context(), Some("expression evaluation"));
        }
        Ok(_) => panic!("Expected error for undefined variable"),
    }
}
