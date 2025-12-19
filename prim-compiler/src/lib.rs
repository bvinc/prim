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

/// Convenience helper: type check and immediately lower the structured program to HIR.
pub fn type_check_and_lower(program: &mut Program) -> Result<HirProgram, TypeCheckError> {
    let _ = resolve_names(program);
    let mut hir = lower_to_hir(program);
    prim_hir::type_check(&mut hir)?;
    Ok(hir)
}
