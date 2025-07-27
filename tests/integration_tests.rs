use prim_parse::{parse, ParseError};
use prim_codegen::{generate_object_code, CodegenError};
use prim_tok::TokenError;

#[test]
fn test_tokenizer_error_unexpected_character() {
    let result = parse("let x = @");
    
    match result {
        Err(ParseError::TokenError(TokenError::UnexpectedCharacter { ch, position })) => {
            assert_eq!(ch, '@');
            assert_eq!(position, 8);
        }
        _ => panic!("Expected TokenError::UnexpectedCharacter, got {:?}", result),
    }
}

#[test]
fn test_parser_error_unexpected_token() {
    let result = parse("let = 42");
    
    match result {
        Err(ParseError::UnexpectedToken { expected, found, position }) => {
            assert_eq!(expected, "identifier");
            assert_eq!(found, prim_tok::TokenKind::Equals);
            assert_eq!(position, 4);
        }
        _ => panic!("Expected ParseError::UnexpectedToken, got {:?}", result),
    }
}

#[test]
fn test_codegen_error_undefined_variable() {
    let program = parse("let x = unknown_var").unwrap();
    let result = generate_object_code(&program);
    
    match result {
        Err(CodegenError::UndefinedVariable { name }) => {
            assert_eq!(name, "unknown_var");
        }
        _ => panic!("Expected CodegenError::UndefinedVariable, got {:?}", result),
    }
}

#[test]
fn test_codegen_error_unsupported_function() {
    let program = parse("unsupported_func()").unwrap();
    let result = generate_object_code(&program);
    
    match result {
        Err(CodegenError::UnsupportedFunctionCall { name }) => {
            assert_eq!(name, "unsupported_func");
        }
        _ => panic!("Expected CodegenError::UnsupportedFunctionCall, got {:?}", result),
    }
}

#[test]
fn test_successful_compilation() {
    let program = parse("let x: u32 = 42\nprintln(x)").unwrap();
    let result = generate_object_code(&program);
    
    assert!(result.is_ok(), "Expected successful compilation, got {:?}", result);
    let object_code = result.unwrap();
    assert!(!object_code.is_empty(), "Object code should not be empty");
}

#[test]
fn test_arithmetic_expression() {
    let program = parse("let result = 2 + 3 * 4\nprintln(result)").unwrap();
    let result = generate_object_code(&program);
    
    assert!(result.is_ok(), "Expected successful compilation, got {:?}", result);
}