use prim_tok::{TokenError, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        position: usize,
    },
    UnexpectedEof,
    TokenError(TokenError),
    MissingMainFunction,
    StatementsOutsideFunction,
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
                position,
            } => {
                write!(
                    f,
                    "Parse error at position {}: expected {}, found {:?}",
                    position, expected, found
                )
            }
            ParseError::UnexpectedEof => {
                write!(f, "Parse error: unexpected end of file")
            }
            ParseError::TokenError(token_err) => {
                write!(f, "Tokenizer {}", token_err)
            }
            ParseError::MissingMainFunction => {
                write!(f, "Parse error: program must have a main function")
            }
            ParseError::StatementsOutsideFunction => {
                write!(f, "Parse error: statements must be inside a function")
            }
        }
    }
}

impl std::error::Error for ParseError {}
