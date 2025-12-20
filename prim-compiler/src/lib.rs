pub mod hir_builder;
pub mod loader;
pub mod program;
pub mod resolver;

pub use hir_builder::lower_to_hir;
pub use loader::{
    LoadError, LoadOptions, LoadedProgram, load_program, load_program_with_options, prim_root,
};
pub use prim_hir::{HirProgram, TypeCheckError};
pub use program::{
    ExportTable, ImportCoverage, ImportRequest, Module, ModuleFile, ModuleId, ModuleKey,
    ModuleOrigin, Program,
};
pub use resolver::{ResolveError, resolve_names};

#[derive(Debug)]
pub enum CompileError {
    Resolve(Vec<ResolveError>),
    TypeCheck(TypeCheckError),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Resolve(errors) => {
                writeln!(f, "Name resolution failed with {} error(s):", errors.len())?;
                for err in errors {
                    writeln!(f, "- {}", err)?;
                }
                Ok(())
            }
            CompileError::TypeCheck(err) => err.fmt(f),
        }
    }
}

impl std::error::Error for CompileError {}

impl From<TypeCheckError> for CompileError {
    fn from(err: TypeCheckError) -> Self {
        CompileError::TypeCheck(err)
    }
}

/// Convenience helper: type check and immediately lower the structured program to HIR.
pub fn type_check_and_lower(program: &mut Program) -> Result<HirProgram, CompileError> {
    if let Err(errors) = resolve_names(program) {
        return Err(CompileError::Resolve(errors));
    }
    let mut hir = lower_to_hir(program);
    prim_hir::type_check(&mut hir)?;
    Ok(hir)
}
