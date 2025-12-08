use std::error::Error;

#[derive(Debug)]
pub enum CodegenError {
    UnsupportedTarget { message: String },
    CraneliftSettingsError(cranelift::codegen::settings::SetError),
    CraneliftModuleError(Box<cranelift_module::ModuleError>),
    CraneliftCodegenError(cranelift::codegen::CodegenError),
    CraneliftObjectError(cranelift_object::object::write::Error),
    Unimplemented,
    MissingMain,
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::UnsupportedTarget { message } => {
                write!(f, "Unsupported target: {}", message)
            }
            CodegenError::CraneliftSettingsError(error) => {
                write!(f, "Cranelift settings error: {}", error)
            }
            CodegenError::CraneliftModuleError(error) => {
                write!(f, "Cranelift module error: {:?}", error)
            }
            CodegenError::CraneliftCodegenError(error) => {
                write!(f, "Cranelift codegen error: {}", error)
            }
            CodegenError::CraneliftObjectError(error) => {
                write!(f, "Cranelift object error: {}", error)
            }
            CodegenError::Unimplemented => write!(f, "Code generation not yet implemented"),
            CodegenError::MissingMain => write!(f, "main function not found"),
        }
    }
}

impl Error for CodegenError {}

impl CodegenError {
    pub fn error_code(&self) -> &'static str {
        match self {
            CodegenError::UnsupportedTarget { .. } => "COD003",
            CodegenError::CraneliftSettingsError(_) => "COD004",
            CodegenError::CraneliftModuleError(_) => "COD005",
            CodegenError::CraneliftCodegenError(_) => "COD006",
            CodegenError::CraneliftObjectError(_) => "COD007",
            CodegenError::Unimplemented => "COD999",
            CodegenError::MissingMain => "COD100",
        }
    }

    pub fn category(&self) -> &'static str {
        "Code Generation"
    }

    pub fn position(&self) -> Option<usize> {
        None
    }

    pub fn context(&self) -> Option<&str> {
        None
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
