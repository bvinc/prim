use prim_parse as ast;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

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
