use prim_tok::{Span, TokenError, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        span: Span,
    },
    UnexpectedEof,
    TokenError(TokenError),
    StatementsOutsideFunction,
    InvalidAttributeUsage {
        message: String,
        span: Span,
    },
    InvalidIntegerLiteral {
        literal: String,
        span: Span,
    },
    InvalidFloatLiteral {
        literal: String,
        span: Span,
    },
    /// Parsing completed but error-level diagnostics were emitted
    HasErrors,
}

impl From<TokenError> for ParseError {
    fn from(err: TokenError) -> Self {
        ParseError::TokenError(err)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => {
                write!(
                    f,
                    "Parse error at {}: expected {}, found {:?}",
                    span, expected, found
                )
            }
            ParseError::UnexpectedEof => {
                write!(f, "Parse error: unexpected end of file")
            }
            ParseError::TokenError(token_err) => {
                write!(f, "Tokenizer {}", token_err)
            }
            ParseError::StatementsOutsideFunction => {
                write!(f, "Parse error: statements must be inside a function")
            }
            ParseError::InvalidAttributeUsage { message, span } => {
                write!(f, "Parse error at {}: {}", span, message)
            }
            ParseError::InvalidIntegerLiteral { literal, .. } => {
                write!(f, "Invalid integer literal: {}", literal)
            }
            ParseError::InvalidFloatLiteral { literal, .. } => {
                write!(f, "Invalid float literal: {}", literal)
            }
            ParseError::HasErrors => {
                write!(f, "Parse error: compilation failed due to previous errors")
            }
        }
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::UnexpectedToken { span, .. } => Some(*span),
            ParseError::UnexpectedEof => None,
            ParseError::TokenError(token_err) => Some(token_err.span()),
            ParseError::StatementsOutsideFunction => None,
            ParseError::InvalidAttributeUsage { span, .. } => Some(*span),
            ParseError::InvalidIntegerLiteral { span, .. } => Some(*span),
            ParseError::InvalidFloatLiteral { span, .. } => Some(*span),
            ParseError::HasErrors => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
    pub severity: Severity,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let level = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
        };
        write!(f, "{} at {}: {}", level, self.span, self.message)
    }
}
