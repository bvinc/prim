use prim_parse as ast;
use prim_tok::{FileId, ModuleId, Span};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

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
    pub symbols: Vec<ResSymbolInfo>,
    /// One interner shared by every file in this compilation. Identifiers
    /// from any file are universally comparable.
    pub interner: Arc<ast::Interner>,
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

pub(crate) type ExportTable = HashMap<String, ResSymbolKind>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum ResSymbolKind {
    Module,
    Function,
    Struct,
    Trait,
    Impl,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct ResSymbolId(pub u32);

#[derive(Clone, Debug)]
pub(crate) struct ResSymbolInfo {
    pub name: String,
    pub kind: ResSymbolKind,
    pub module: Option<ModuleId>,
    #[allow(dead_code)] // Useful for diagnostics
    pub file: FileId,
    #[allow(dead_code)] // Useful for diagnostics
    pub span: Span,
}
