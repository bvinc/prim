#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    UnexpectedCharacter { ch: char, position: usize },
    UnterminatedString { position: usize },
    InvalidNumber { text: String, position: usize },
    InvalidOperatorSpacing { op: &'static str, position: usize },
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
            TokenError::InvalidOperatorSpacing { op, position } => {
                write!(
                    f,
                    "Invalid spacing around operator '{}' at position {}",
                    op, position
                )
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
            TokenError::InvalidNumber { .. } => "TOK004",
            TokenError::InvalidOperatorSpacing { .. } => "TOK005",
        }
    }

    pub fn category(&self) -> &'static str {
        "Tokenization"
    }

    pub fn position(&self) -> Option<usize> {
        match self {
            TokenError::UnexpectedCharacter { position, .. } => Some(*position),
            TokenError::UnterminatedString { position } => Some(*position),
            TokenError::InvalidNumber { position, .. } => Some(*position),
            TokenError::InvalidOperatorSpacing { position, .. } => Some(*position),
        }
    }

    pub fn context(&self) -> Option<&str> {
        match self {
            TokenError::UnexpectedCharacter { .. } => Some("character scanning"),
            TokenError::UnterminatedString { .. } => Some("string literal"),
            TokenError::InvalidNumber { .. } => Some("number literal"),
            TokenError::InvalidOperatorSpacing { .. } => Some("operator spacing"),
        }
    }
}
