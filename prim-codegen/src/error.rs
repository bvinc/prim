use std::error::Error;

#[derive(Debug)]
pub enum CodegenError {
    UnsupportedTarget {
        message: String,
    },
    CraneliftSettingsError(cranelift::codegen::settings::SetError),
    CraneliftModuleError(Box<cranelift_module::ModuleError>),
    CraneliftCodegenError(cranelift::codegen::CodegenError),
    CraneliftObjectError(cranelift_object::object::write::Error),
    UndefinedLocal(prim_hir::SymbolId),
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    ArgTypeMismatch {
        expected: cranelift::prelude::Type,
        found: cranelift::prelude::Type,
    },
    FieldTypeMismatch {
        expected: cranelift::prelude::Type,
        found: cranelift::prelude::Type,
    },
    MissingStructLayout(prim_hir::StructId),
    MissingStructField {
        struct_id: prim_hir::StructId,
        field: prim_hir::SymbolId,
    },
    MissingStructValue {
        struct_id: prim_hir::StructId,
        field: prim_hir::SymbolId,
    },
    MissingFunction(prim_hir::FuncId),
    InvalidFieldAccess,
    InvalidDereference,
    InvalidBreak,
    ReturnArityMismatch {
        expected: usize,
        found: usize,
    },
    ReturnTypeMismatch {
        expected: cranelift::prelude::Type,
        found: cranelift::prelude::Type,
    },
    MissingReturnValue,
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
            CodegenError::UndefinedLocal(sym) => write!(f, "Undefined local {:?}", sym),
            CodegenError::ArityMismatch { expected, found } => {
                write!(f, "Arity mismatch: expected {}, found {}", expected, found)
            }
            CodegenError::ArgTypeMismatch { expected, found } => {
                write!(
                    f,
                    "Argument type mismatch: expected {expected}, found {found}"
                )
            }
            CodegenError::FieldTypeMismatch { expected, found } => {
                write!(f, "Field type mismatch: expected {expected}, found {found}")
            }
            CodegenError::MissingStructLayout(id) => {
                write!(f, "Missing struct layout for {:?}", id)
            }
            CodegenError::MissingStructField { struct_id, field } => {
                write!(f, "Missing field {:?} for struct {:?}", field, struct_id)
            }
            CodegenError::MissingStructValue { struct_id, field } => {
                write!(f, "Missing value for {:?} on struct {:?}", field, struct_id)
            }
            CodegenError::MissingFunction(func) => write!(f, "Missing function {:?}", func),
            CodegenError::InvalidFieldAccess => write!(f, "Invalid field access"),
            CodegenError::InvalidDereference => write!(f, "Invalid dereference"),
            CodegenError::InvalidBreak => write!(f, "break outside loop"),
            CodegenError::ReturnArityMismatch { expected, found } => {
                write!(
                    f,
                    "Return arity mismatch: expected {}, found {}",
                    expected, found
                )
            }
            CodegenError::ReturnTypeMismatch { expected, found } => {
                write!(
                    f,
                    "Return type mismatch: expected {expected}, found {found}"
                )
            }
            CodegenError::MissingReturnValue => {
                write!(
                    f,
                    "Missing return value for function with declared return type"
                )
            }
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
            CodegenError::UndefinedLocal(_) => "COD010",
            CodegenError::ArityMismatch { .. } => "COD011",
            CodegenError::MissingStructLayout(_) => "COD012",
            CodegenError::MissingStructField { .. } => "COD013",
            CodegenError::MissingStructValue { .. } => "COD014",
            CodegenError::MissingFunction(_) => "COD015",
            CodegenError::InvalidFieldAccess => "COD016",
            CodegenError::InvalidDereference => "COD017",
            CodegenError::InvalidBreak => "COD018",
            CodegenError::ReturnArityMismatch { .. } => "COD019",
            CodegenError::ReturnTypeMismatch { .. } => "COD020",
            CodegenError::MissingReturnValue => "COD021",
            CodegenError::ArgTypeMismatch { .. } => "COD022",
            CodegenError::FieldTypeMismatch { .. } => "COD023",
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
