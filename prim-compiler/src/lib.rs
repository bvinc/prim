mod hir_builder;
mod loader;
mod program;
mod resolver;

pub use hir_builder::LoweringError;
pub use loader::{LoadError, LoadOptions, prim_root};
pub use prim_hir::{HirProgram, TypeCheckError};
pub use program::FileId;
pub use resolver::ResolveError;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Maps file IDs to their paths for error reporting.
#[derive(Debug, Default)]
pub struct SourceMap {
    files: HashMap<FileId, PathBuf>,
}

impl SourceMap {
    /// Get the path for a file ID.
    pub fn get_path(&self, id: FileId) -> Option<&Path> {
        self.files.get(&id).map(|p| p.as_path())
    }

    /// Read the source content for a file ID.
    pub fn read_source(&self, id: FileId) -> std::io::Result<String> {
        match self.files.get(&id) {
            Some(path) => std::fs::read_to_string(path),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Unknown file ID: {:?}", id),
            )),
        }
    }
}

#[derive(Debug)]
pub enum CompileError {
    Load(LoadError),
    Resolve(Vec<ResolveError>),
    Lowering(Vec<LoweringError>),
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
            CompileError::Lowering(errors) => {
                writeln!(f, "Lowering failed with {} error(s):", errors.len())?;
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
    fn from(e: LoadError) -> Self {
        CompileError::Load(e)
    }
}

impl From<Vec<ResolveError>> for CompileError {
    fn from(e: Vec<ResolveError>) -> Self {
        CompileError::Resolve(e)
    }
}

impl From<Vec<LoweringError>> for CompileError {
    fn from(e: Vec<LoweringError>) -> Self {
        CompileError::Lowering(e)
    }
}

impl From<prim_hir::TypeCheckError> for CompileError {
    fn from(e: prim_hir::TypeCheckError) -> Self {
        CompileError::TypeCheck(e)
    }
}

/// Result of compilation: source map for error reporting plus the compilation result.
pub type CompileResult = (Arc<SourceMap>, Result<HirProgram, CompileError>);

/// Compile a Prim source file to HIR.
///
/// This is the main entry point for compilation. It:
/// 1. Loads and parses the source file and its dependencies
/// 2. Collects top-level symbols into module scopes
/// 3. Lowers the AST to HIR (resolving names inline)
/// 4. Type checks the HIR
///
/// Returns the source map (for error reporting) and the compilation result.
pub fn compile(path: &str, options: LoadOptions) -> CompileResult {
    let mut loaded = match loader::load_program(path, options) {
        Ok(l) => l,
        Err(e) => return (Arc::new(SourceMap::default()), Err(e.into())),
    };

    let source_map = build_source_map(&loaded.program);

    let result = (|| {
        let module_scopes = resolver::collect_scopes(&mut loaded.program)?;
        let mut hir = hir_builder::lower_to_hir(&loaded.program, &module_scopes)?;
        prim_hir::type_check(&mut hir)?;
        Ok(hir)
    })();

    (source_map, result)
}

fn build_source_map(program: &program::Program) -> Arc<SourceMap> {
    let mut files = HashMap::new();
    for module in &program.modules {
        for file in &module.files {
            files.insert(file.file_id, file.path.clone());
        }
    }
    Arc::new(SourceMap { files })
}
