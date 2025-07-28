#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    UnexpectedCharacter { ch: char, position: usize },
    UnterminatedString { position: usize },
    InvalidNumber { text: String, position: usize },
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::UnexpectedCharacter { ch, position } => {
                write!(f, "Unexpected character '{}' at position {}", ch, position)
            }
            TokenError::UnterminatedString { position } => {
                write!(f, "Unterminated string literal at position {}", position)
            }
            TokenError::InvalidNumber { text, position } => {
                write!(f, "Invalid number '{}' at position {}", text, position)
            }
        }
    }
}

impl std::error::Error for TokenError {}

pub type TokenResult<T> = Result<T, TokenError>;
