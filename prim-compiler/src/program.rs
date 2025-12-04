use prim_parse as ast;
use prim_parse::Span;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ModuleKey {
    Name(Vec<String>),
    Path(PathBuf),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModuleOrigin {
    User,
    Stdlib,
    Other,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub modules: Vec<Module>,
    pub root: ModuleId,
    pub module_index: HashMap<ModuleKey, ModuleId>,
    pub file_index: HashMap<PathBuf, FileId>,
    pub name_resolution: NameResolution,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: ModuleId,
    pub key: ModuleKey,
    pub name: Vec<String>,
    pub origin: ModuleOrigin,
    pub files: Vec<ModuleFile>,
    pub imports: Vec<ImportRequest>,
    pub exports: ExportTable,
    pub body_source: String,
}

#[derive(Clone, Debug)]
pub struct ModuleFile {
    pub file_id: FileId,
    pub path: PathBuf,
    pub source: Arc<str>,
    pub ast: ast::Program,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportRequest {
    pub module: Vec<String>,
    pub coverage: ImportCoverage,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportCoverage {
    All,
    Symbols(Vec<String>),
}

#[derive(Clone, Debug, Default)]
pub struct ExportTable {
    pub functions: Vec<String>,
    pub structs: Vec<String>,
    pub traits: Vec<String>,
    pub impls: Vec<String>,
}

impl ExportTable {
    pub fn contains(&self, name: &str) -> bool {
        self.functions.iter().any(|n| n == name)
            || self.structs.iter().any(|n| n == name)
            || self.traits.iter().any(|n| n == name)
            || self.impls.iter().any(|n| n == name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Module,
    Function,
    Struct,
    Trait,
    Impl,
    Global,
    Param,
    Local,
    Field,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NameRef {
    pub file: FileId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SymbolInfo {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub module: Option<ModuleId>,
    pub file: FileId,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct NameResolution {
    pub symbols: Vec<SymbolInfo>,
    pub uses: HashMap<NameRef, SymbolId>,
}
