#[derive(Debug, Clone, PartialEq)]
pub enum CodegenError {
    UndefinedVariable { name: String },
    UnsupportedFunctionCall { name: String },
    UnsupportedTarget { message: String },
    CraneliftError { message: String },
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::UndefinedVariable { name } => {
                write!(f, "Undefined variable: {}", name)
            }
            CodegenError::UnsupportedFunctionCall { name } => {
                write!(f, "Unsupported function call: {}", name)
            }
            CodegenError::UnsupportedTarget { message } => {
                write!(f, "Unsupported target: {}", message)
            }
            CodegenError::CraneliftError { message } => {
                write!(f, "Cranelift error: {}", message)
            }
        }
    }
}

impl std::error::Error for CodegenError {}

impl From<Box<dyn std::error::Error>> for CodegenError {
    fn from(err: Box<dyn std::error::Error>) -> Self {
        CodegenError::CraneliftError {
            message: err.to_string(),
        }
    }
}

pub type CodegenResult<T> = Result<T, CodegenError>;