#[derive(Debug, Clone, PartialEq)]
pub enum CodegenError {
    UndefinedVariable { name: String, context: String },
    UnsupportedFunctionCall { name: String, context: String },
    UnsupportedTarget { message: String },
    CraneliftError { message: String },
    InvalidExpression { message: String, context: String },
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::UndefinedVariable { name, context } => {
                write!(f, "Undefined variable '{}' in {}", name, context)
            }
            CodegenError::UnsupportedFunctionCall { name, context } => {
                write!(f, "Unsupported function call '{}' in {}", name, context)
            }
            CodegenError::UnsupportedTarget { message } => {
                write!(f, "Unsupported target: {}", message)
            }
            CodegenError::CraneliftError { message } => {
                write!(f, "Cranelift error: {}", message)
            }
            CodegenError::InvalidExpression { message, context } => {
                write!(f, "Invalid expression in {}: {}", context, message)
            }
        }
    }
}

impl std::error::Error for CodegenError {}

/// Implementation of unified error trait for CodegenError
impl CodegenError {
    pub fn error_code(&self) -> &'static str {
        match self {
            CodegenError::UndefinedVariable { .. } => "COD001",
            CodegenError::UnsupportedFunctionCall { .. } => "COD002",
            CodegenError::UnsupportedTarget { .. } => "COD003",
            CodegenError::CraneliftError { .. } => "COD004",
            CodegenError::InvalidExpression { .. } => "COD005",
        }
    }

    pub fn category(&self) -> &'static str {
        "Code Generation"
    }

    pub fn position(&self) -> Option<usize> {
        // CodegenError doesn't currently have position information
        // This would require threading position info through the AST
        None
    }

    pub fn context(&self) -> Option<&str> {
        match self {
            CodegenError::UndefinedVariable { context, .. } => Some(context),
            CodegenError::UnsupportedFunctionCall { context, .. } => Some(context),
            CodegenError::UnsupportedTarget { .. } => Some("target initialization"),
            CodegenError::CraneliftError { .. } => Some("cranelift backend"),
            CodegenError::InvalidExpression { context, .. } => Some(context),
        }
    }
}

impl From<Box<dyn std::error::Error>> for CodegenError {
    fn from(err: Box<dyn std::error::Error>) -> Self {
        CodegenError::CraneliftError {
            message: err.to_string(),
        }
    }
}
