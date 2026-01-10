use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    UnexpectedCharacter { ch: char, span: Span },
    UnterminatedString { span: Span },
    InvalidNumber { text: String, span: Span },
    InvalidOperatorSpacing { op: &'static str, span: Span },
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::UnexpectedCharacter { ch, span } => {
                write!(f, "Unexpected character '{}' at {}", ch, span)
            }
            TokenError::UnterminatedString { span } => {
                write!(f, "Unterminated string literal at {}", span)
            }
            TokenError::InvalidNumber { text, span } => {
                write!(f, "Invalid number '{}' at {}", text, span)
            }
            TokenError::InvalidOperatorSpacing { op, span } => {
                write!(f, "Invalid spacing around operator '{}' at {}", op, span)
            }
        }
    }
}

impl std::error::Error for TokenError {}

impl TokenError {
    pub fn span(&self) -> Span {
        match self {
            TokenError::UnexpectedCharacter { span, .. } => *span,
            TokenError::UnterminatedString { span } => *span,
            TokenError::InvalidNumber { span, .. } => *span,
            TokenError::InvalidOperatorSpacing { span, .. } => *span,
        }
    }
}
