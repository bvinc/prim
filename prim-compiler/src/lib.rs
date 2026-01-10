mod hir_builder;
mod loader;
mod program;
mod resolver;

pub use loader::{LoadError, LoadOptions, prim_root};
pub use prim_hir::{HirProgram, TypeCheckError};
pub use resolver::ResolveError;

#[derive(Debug)]
pub enum CompileError {
    Load(LoadError),
    Resolve(Vec<ResolveError>),
    TypeCheck(TypeCheckError),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Load(err) => err.fmt(f),
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

impl From<LoadError> for CompileError {
    fn from(err: LoadError) -> Self {
        CompileError::Load(err)
    }
}

impl From<TypeCheckError> for CompileError {
    fn from(err: TypeCheckError) -> Self {
        CompileError::TypeCheck(err)
    }
}

/// Compile a Prim source file to HIR.
///
/// This is the main entry point for compilation. It:
/// 1. Loads and parses the source file and its dependencies
/// 2. Resolves names across modules
/// 3. Lowers the AST to HIR
/// 4. Type checks the HIR
pub fn compile(path: &str) -> Result<HirProgram, CompileError> {
    compile_with_options(path, LoadOptions::default())
}

/// Compile with custom load options.
pub fn compile_with_options(path: &str, options: LoadOptions) -> Result<HirProgram, CompileError> {
    let mut loaded = loader::load_program_with_options(path, options)?;
    if let Err(errors) = resolver::resolve_names(&mut loaded.program) {
        return Err(CompileError::Resolve(errors));
    }
    let mut hir = hir_builder::lower_to_hir(&loaded.program);
    prim_hir::type_check(&mut hir)?;
    Ok(hir)
}
