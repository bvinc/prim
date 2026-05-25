use prim_parse as ast;
use prim_parse::Span;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct ModuleId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum ModuleKey {
    Name(Vec<String>),
    Path(PathBuf),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModuleOrigin {
    User,
    Stdlib,
}

#[derive(Clone, Debug)]
pub(crate) struct Program {
    pub modules: Vec<Module>,
    pub root: ModuleId,
    pub module_index: HashMap<ModuleKey, ModuleId>,
    pub name_resolution: NameResolution,
}

#[derive(Clone, Debug)]
pub(crate) struct Module {
    pub id: ModuleId,
    pub name: Vec<String>,
    pub files: Vec<ModuleFile>,
    pub imports: Vec<ImportRequest>,
    pub exports: ExportTable,
}

#[derive(Clone, Debug)]
pub(crate) struct ModuleFile {
    pub file_id: FileId,
    pub path: PathBuf,
    pub ast: ast::Program,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ImportRequest {
    pub module: Vec<String>,
    pub coverage: ImportCoverage,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ImportCoverage {
    All,
    Symbols(Vec<String>),
}

pub(crate) type ExportTable = HashMap<String, SymbolKind>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum SymbolKind {
    Module,
    Function,
    Struct,
    Trait,
    Impl,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SymbolId(pub u32);

#[derive(Clone, Debug)]
pub(crate) struct SymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub module: Option<ModuleId>,
    #[allow(dead_code)] // Useful for diagnostics
    pub file: FileId,
    #[allow(dead_code)] // Useful for diagnostics
    pub span: Span,
}

/// Top-level symbol definitions collected during scope resolution.
#[derive(Clone, Debug, Default)]
pub(crate) struct NameResolution {
    pub symbols: Vec<SymbolInfo>,
}
