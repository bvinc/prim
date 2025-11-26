pub mod loader;
pub mod program;
pub mod typecheck;

pub use loader::{LoadError, LoadOptions, LoadedProgram, load_program, load_program_with_options};
pub use program::{
    ExportTable, ImportCoverage, ImportRequest, Module, ModuleFile, ModuleId, ModuleKey,
    ModuleOrigin, Program,
};
pub use typecheck::type_check_program;
