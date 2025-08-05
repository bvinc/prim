#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    UnexpectedCharacter { ch: char, position: usize },
    UnterminatedString { position: usize },
    UnterminatedComment { position: usize },
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
            TokenError::UnterminatedComment { position } => {
                write!(f, "Unterminated block comment at position {}", position)
            }
            TokenError::InvalidNumber { text, position } => {
                write!(f, "Invalid number '{}' at position {}", text, position)
            }
        }
    }
}

impl std::error::Error for TokenError {}

/// Implementation of unified error trait for TokenError
impl TokenError {
    pub fn error_code(&self) -> &'static str {
        match self {
            TokenError::UnexpectedCharacter { .. } => "TOK001",
            TokenError::UnterminatedString { .. } => "TOK002",
            TokenError::UnterminatedComment { .. } => "TOK003",
            TokenError::InvalidNumber { .. } => "TOK004",
        }
    }

    pub fn category(&self) -> &'static str {
        "Tokenization"
    }

    pub fn position(&self) -> Option<usize> {
        match self {
            TokenError::UnexpectedCharacter { position, .. } => Some(*position),
            TokenError::UnterminatedString { position } => Some(*position),
            TokenError::UnterminatedComment { position } => Some(*position),
            TokenError::InvalidNumber { position, .. } => Some(*position),
        }
    }

    pub fn context(&self) -> Option<&str> {
        match self {
            TokenError::UnexpectedCharacter { .. } => Some("character scanning"),
            TokenError::UnterminatedString { .. } => Some("string literal"),
            TokenError::UnterminatedComment { .. } => Some("block comment"),
            TokenError::InvalidNumber { .. } => Some("number literal"),
        }
    }
}
