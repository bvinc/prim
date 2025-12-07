//! Unified error handling for the Prim compiler
//!
//! This module defines a common trait that all error types in the compiler
//! implement to ensure consistent error handling and reporting.

use prim_codegen::CodegenError;
use prim_hir::typecheck::TypeCheckError;
use prim_parse::ParseError;
use prim_tok::TokenError;

/// A unified trait for all compiler errors
///
/// This trait ensures that all errors in the Prim compiler have consistent
/// capabilities for error reporting and debugging.
pub trait PrimError: std::error::Error + std::fmt::Debug {
    /// Get an error code for programmatic handling
    fn error_code(&self) -> &'static str;

    /// Get a human-readable error category
    fn category(&self) -> &'static str;

    /// Get position information if available
    fn position(&self) -> Option<usize>;

    /// Get additional context information
    fn context(&self) -> Option<&str>;
}

/// A top-level error type that can represent any compiler error
#[derive(Debug)]
pub enum CompilerError {
    Token(TokenError),
    Parse(ParseError),
    Codegen(CodegenError),
    TypeCheck(TypeCheckError),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::Token(err) => err.fmt(f),
            CompilerError::Parse(err) => err.fmt(f),
            CompilerError::Codegen(err) => err.fmt(f),
            CompilerError::TypeCheck(err) => err.fmt(f),
        }
    }
}

impl std::error::Error for CompilerError {}

impl From<TokenError> for CompilerError {
    fn from(err: TokenError) -> Self {
        CompilerError::Token(err)
    }
}

impl From<ParseError> for CompilerError {
    fn from(err: ParseError) -> Self {
        CompilerError::Parse(err)
    }
}

impl From<CodegenError> for CompilerError {
    fn from(err: CodegenError) -> Self {
        CompilerError::Codegen(err)
    }
}

impl From<TypeCheckError> for CompilerError {
    fn from(err: TypeCheckError) -> Self {
        CompilerError::TypeCheck(err)
    }
}

impl PrimError for CompilerError {
    fn error_code(&self) -> &'static str {
        match self {
            CompilerError::Token(err) => err.error_code(),
            CompilerError::Parse(err) => err.error_code(),
            CompilerError::Codegen(err) => err.error_code(),
            CompilerError::TypeCheck(err) => err.error_code(),
        }
    }

    fn category(&self) -> &'static str {
        match self {
            CompilerError::Token(err) => err.category(),
            CompilerError::Parse(err) => err.category(),
            CompilerError::Codegen(err) => err.category(),
            CompilerError::TypeCheck(err) => err.category(),
        }
    }

    fn position(&self) -> Option<usize> {
        match self {
            CompilerError::Token(err) => err.position(),
            CompilerError::Parse(err) => err.position(),
            CompilerError::Codegen(err) => err.position(),
            CompilerError::TypeCheck(err) => err.position(),
        }
    }

    fn context(&self) -> Option<&str> {
        match self {
            CompilerError::Token(err) => err.context(),
            CompilerError::Parse(err) => err.context(),
            CompilerError::Codegen(err) => err.context(),
            CompilerError::TypeCheck(err) => err.context(),
        }
    }
}

/// Result type for operations that can fail with any compiler error
pub type CompilerResult<T> = Result<T, CompilerError>;
