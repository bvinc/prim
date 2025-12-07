use std::error::Error;

#[derive(Debug)]
pub enum CodegenError {
    UndefinedVariable { name: String, context: String },
    UnsupportedFunctionCall { name: String, context: String },
    UnsupportedTarget { message: String },
    CraneliftSettingsError(cranelift::codegen::settings::SetError),
    CraneliftModuleError(Box<cranelift_module::ModuleError>),
    CraneliftCodegenError(cranelift::codegen::CodegenError),
    CraneliftObjectError(cranelift_object::object::write::Error),
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
            CodegenError::CraneliftSettingsError(error) => {
                write!(f, "Cranelift settings error: {}", error)?;
                let mut source = error.source();
                while let Some(err) = source {
                    write!(f, "\nCaused by: {}", err)?;
                    source = err.source();
                }
                Ok(())
            }
            CodegenError::CraneliftModuleError(error) => {
                write!(f, "Cranelift module error: {}", error)?;
                let mut source = error.source();
                while let Some(err) = source {
                    write!(f, "\nCaused by: {}", err)?;
                    source = err.source();
                }
                Ok(())
            }
            CodegenError::CraneliftCodegenError(error) => {
                write!(f, "Cranelift codegen error: {}", error)?;
                let mut source = error.source();
                while let Some(err) = source {
                    write!(f, "\nCaused by: {}", err)?;
                    source = err.source();
                }
                Ok(())
            }
            CodegenError::CraneliftObjectError(error) => {
                write!(f, "Cranelift object error: {}", error)?;
                let mut source = error.source();
                while let Some(err) = source {
                    write!(f, "\nCaused by: {}", err)?;
                    source = err.source();
                }
                Ok(())
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
            CodegenError::CraneliftSettingsError(_) => "COD004",
            CodegenError::CraneliftModuleError(_) => "COD005",
            CodegenError::CraneliftCodegenError(_) => "COD006",
            CodegenError::CraneliftObjectError(_) => "COD007",
            CodegenError::InvalidExpression { .. } => "COD008",
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
            CodegenError::CraneliftSettingsError(_) => Some("cranelift settings"),
            CodegenError::CraneliftModuleError(_) => Some("cranelift module"),
            CodegenError::CraneliftCodegenError(_) => Some("cranelift codegen"),
            CodegenError::CraneliftObjectError(_) => Some("cranelift object"),
            CodegenError::InvalidExpression { context, .. } => Some(context),
        }
    }
}

impl From<cranelift::codegen::settings::SetError> for CodegenError {
    fn from(error: cranelift::codegen::settings::SetError) -> Self {
        CodegenError::CraneliftSettingsError(error)
    }
}

impl From<cranelift_module::ModuleError> for CodegenError {
    fn from(error: cranelift_module::ModuleError) -> Self {
        CodegenError::CraneliftModuleError(Box::new(error))
    }
}

impl From<cranelift::codegen::CodegenError> for CodegenError {
    fn from(error: cranelift::codegen::CodegenError) -> Self {
        CodegenError::CraneliftCodegenError(error)
    }
}

impl From<cranelift_object::object::write::Error> for CodegenError {
    fn from(error: cranelift_object::object::write::Error) -> Self {
        CodegenError::CraneliftObjectError(error)
    }
}
