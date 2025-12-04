pub mod hir_builder;
pub mod loader;
pub mod program;
pub mod resolver;
pub mod typecheck;

pub use hir_builder::lower_to_hir;
pub use loader::{
    LoadError, LoadOptions, LoadedProgram, load_program, load_program_with_options, prim_root,
};
pub use prim_hir::HirProgram;
pub use program::{
    ExportTable, ImportCoverage, ImportRequest, Module, ModuleFile, ModuleId, ModuleKey,
    ModuleOrigin, Program,
};
pub use resolver::{ResolveError, resolve_names};
pub use typecheck::type_check_program;

/// Convenience helper: type check and immediately lower the structured program to HIR.
pub fn type_check_and_lower(
    program: &mut Program,
) -> Result<HirProgram, prim_parse::TypeCheckError> {
    type_check_program(program)?;
    let _ = resolve_names(program);
    Ok(lower_to_hir(program))
}
