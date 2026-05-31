pub mod hir;
mod hir_builder;
mod loader;
mod prelude;
mod program;
mod resolver;

pub use hir::{Program, TypeCheckError};
pub use hir_builder::LoweringError;
pub use loader::{LoadError, LoadOptions, prim_root};
pub use prim_tok::FileId;
pub use resolver::ResolveError;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Single source-of-truth for file metadata in a compilation: paths and
/// (lazily-cached) source bytes, keyed by `FileId`.
///
/// The loader registers each file as it's read, calling `add_file` with the
/// source it already has in hand so subsequent reads (e.g., for `@dbg` prefix
/// generation) hit the cache rather than disk. All methods take `&self` so
/// the map can be shared via `Arc<SourceMap>` across the rest of the
/// compilation (and, eventually, parallel workers).
#[derive(Debug, Default)]
pub struct SourceMap {
    paths: RwLock<HashMap<FileId, PathBuf>>,
    sources: RwLock<HashMap<FileId, Arc<str>>>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a file's path and its source bytes. Called by the loader as
    /// each file is read so the source isn't re-read from disk later.
    pub fn add_file(&self, id: FileId, path: PathBuf, source: Arc<str>) {
        self.paths.write().unwrap().insert(id, path);
        self.sources.write().unwrap().insert(id, source);
    }

    /// Look up a file's path. Returns an owned `PathBuf` because the internal
    /// map is behind a lock; callers wanting the path live for longer should
    /// hold the returned value.
    pub fn get_path(&self, id: FileId) -> Option<PathBuf> {
        self.paths.read().unwrap().get(&id).cloned()
    }

    /// Get source bytes for a file. Returns the cached copy if present,
    /// otherwise reads from disk and caches the result.
    pub fn read_source(&self, id: FileId) -> std::io::Result<Arc<str>> {
        if let Some(cached) = self.sources.read().unwrap().get(&id).cloned() {
            return Ok(cached);
        }
        let path = self
            .paths
            .read()
            .unwrap()
            .get(&id)
            .cloned()
            .ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("Unknown file ID: {:?}", id),
                )
            })?;
        let source: Arc<str> = Arc::from(std::fs::read_to_string(&path)?);
        self.sources.write().unwrap().insert(id, source.clone());
        Ok(source)
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

impl From<hir::TypeCheckError> for CompileError {
    fn from(e: hir::TypeCheckError) -> Self {
        CompileError::TypeCheck(e)
    }
}

/// Compile a Prim source file to HIR.
///
/// This is the main entry point for compilation. It:
/// 1. Loads and parses the source file and its dependencies
/// 2. Collects top-level symbols into module scopes
/// 3. Lowers the AST to HIR (resolving names inline)
/// 4. Type checks the HIR
///
/// Returns the source map (for error reporting) and the compilation result.
pub fn compile(
    path: &str,
    options: LoadOptions,
) -> (Arc<SourceMap>, Result<hir::Program, CompileError>) {
    let source_map = Arc::new(SourceMap::new());
    let include_prelude = options.include_prelude;
    let extra_modules = if include_prelude {
        prelude::extra_modules()
    } else {
        Vec::new()
    };

    let mut program = match loader::load_program(path, options, &extra_modules, source_map.clone())
    {
        Ok(p) => p,
        Err(e) => return (source_map, Err(e.into())),
    };

    if include_prelude {
        prelude::inject(&mut program);
    }

    let result = (|| {
        let module_scopes = resolver::collect_scopes(&mut program)?;
        let mut hir = hir_builder::lower_to_hir(&program, &module_scopes, source_map.clone())?;
        hir::type_check(&mut hir)?;
        Ok(hir)
    })();

    (source_map, result)
}
