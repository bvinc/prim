use crate::program::{
    ExportTable, FileId, ImportCoverage, ImportRequest, Module, ModuleFile, ModuleId, ModuleKey,
    ModuleOrigin, NameResolution, Program,
};
use prim_parse::{self, ImportDecl, ImportSelector};
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::path::{Path, PathBuf};

/// A fully loaded program.
pub struct LoadedProgram {
    pub program: Program,
}

/// Resolve the Prim root directory from `PRIM_ROOT` or the current executable location.
pub fn prim_root() -> Result<PathBuf, LoadError> {
    prim_root_from_env_or_exe()
}

#[derive(Debug)]
pub enum LoadError {
    Io(std::io::Error),
    Parse(prim_parse::ParseError),
    InvalidModule(String),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Io(err) => write!(f, "IO error: {}", err),
            LoadError::Parse(err) => write!(f, "Parse error: {}", err),
            LoadError::InvalidModule(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for LoadError {}

impl From<std::io::Error> for LoadError {
    fn from(err: std::io::Error) -> Self {
        LoadError::Io(err)
    }
}

impl From<prim_parse::ParseError> for LoadError {
    fn from(err: prim_parse::ParseError) -> Self {
        LoadError::Parse(err)
    }
}

/// Options that control how the loader resolves a program.
#[derive(Clone, Debug)]
pub struct LoadOptions {
    /// Automatically include the standard prelude when compiling a single file.
    pub include_prelude: bool,
    /// Optional override for the prim root directory. Defaults to `PRIM_ROOT` env var or exe-relative discovery.
    pub prim_root: Option<PathBuf>,
    /// Optional override for the std root. Defaults to `<prim_root>/src` (or `<prim_root>/prim-std/src` as a fallback).
    pub std_root: Option<PathBuf>,
}

impl Default for LoadOptions {
    fn default() -> Self {
        Self {
            include_prelude: true,
            prim_root: None,
            std_root: None,
        }
    }
}

fn inject_prelude_imports(
    imports: &mut Vec<ImportRequest>,
    import_index: &mut HashMap<String, usize>,
    module_segments: &[String],
    origin: ModuleOrigin,
) {
    let std_string = vec!["std".into(), "string".into()];
    if module_segments != std_string {
        merge_import_request(imports, import_index, std_string, ImportCoverage::All);
    }

    if matches!(origin, ModuleOrigin::User) {
        let std_io = vec!["std".into(), "io".into()];
        if module_segments != std_io {
            merge_import_request(imports, import_index, std_io, ImportCoverage::All);
        }
    }
}

pub fn load_program(
    entry_path: impl AsRef<Path>,
    options: LoadOptions,
) -> Result<LoadedProgram, LoadError> {
    let entry_path = entry_path.as_ref();
    let prim_root = match &options.prim_root {
        Some(root) => root.clone(),
        None => prim_root_from_env_or_exe()?,
    };
    let default_std_root = default_std_root(&prim_root);
    let std_root = options.std_root.clone().unwrap_or(default_std_root);

    if entry_path.is_dir() {
        let module_root = if entry_path.file_name().is_some_and(|n| n == "cmd") {
            entry_path
                .parent()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| entry_path.to_path_buf())
        } else {
            entry_path.to_path_buf()
        };
        let mut loader = Loader::new(module_root, std_root, options);
        loader.load_directory(entry_path)
    } else {
        let module_root = entry_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));
        let mut loader = Loader::new(module_root, std_root, options);
        loader.load_single_file(entry_path)
    }
}

struct Loader {
    module_root: PathBuf,
    std_root: PathBuf,
    options: LoadOptions,
    program: Program,
    module_cache: HashMap<String, ModuleCacheEntry>,
    next_file_id: u32,
}

#[derive(Clone, Debug)]
struct ModuleCacheEntry {
    imports: Vec<ImportRequest>,
    files: Vec<ModuleFile>,
}

impl Loader {
    fn new(module_root: PathBuf, std_root: PathBuf, options: LoadOptions) -> Self {
        Self {
            module_root,
            std_root,
            options,
            program: Program {
                modules: Vec::new(),
                root: ModuleId(0),
                module_index: HashMap::new(),
                name_resolution: NameResolution::default(),
            },
            module_cache: HashMap::new(),
            next_file_id: 0,
        }
    }

    fn alloc_file(&mut self, path: PathBuf, ast: prim_parse::Program) -> ModuleFile {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        ModuleFile { file_id, path, ast }
    }

    fn load_single_file(&mut self, path: &Path) -> Result<LoadedProgram, LoadError> {
        let source = std::fs::read_to_string(path)?;
        let (ast, _diagnostics) = prim_parse::parse(&source);
        let ast = ast?;

        let module_name = ast
            .module_name
            .as_ref()
            .map(|ident| ast.resolve(ident.sym).to_string())
            .unwrap_or_else(|| {
                path.file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("main")
                    .to_string()
            });

        let planned = plan_import_requests(
            &ast.imports,
            &ast.interner,
            &self.module_root,
            &self.std_root,
        )?;
        let mut imports = Vec::new();
        let mut import_index = HashMap::new();
        for ImportRequest { module, coverage } in planned {
            merge_import_request(&mut imports, &mut import_index, module, coverage);
        }
        if self.options.include_prelude {
            inject_prelude_imports(
                &mut imports,
                &mut import_index,
                &[module_name.clone()],
                ModuleOrigin::User,
            );
        }

        let module_files = vec![self.alloc_file(path.to_path_buf(), ast.clone())];

        let exports = collect_exports(&module_files);
        let module_id = ModuleId(self.program.modules.len() as u32);
        let module = Module {
            id: module_id,
            name: vec![module_name],
            files: module_files,
            imports: imports.clone(),
            exports,
        };

        let key = ModuleKey::Path(path.canonicalize().unwrap_or_else(|_| path.to_path_buf()));
        self.program.module_index.insert(key, module_id);
        self.program.root = module_id;
        self.program.modules.push(module);

        // Ensure dependencies are loaded.
        let mut stack = Vec::new();
        for ImportRequest { module, .. } in &imports {
            self.ensure_module(module, &mut stack)?;
        }
        self.validate_imports(&imports)?;

        Ok(LoadedProgram {
            program: self.program.clone(),
        })
    }

    fn load_directory(&mut self, dir: &Path) -> Result<LoadedProgram, LoadError> {
        let mut files: Vec<PathBuf> = std::fs::read_dir(dir)?
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().is_some_and(|ext| ext == "prim"))
            .collect();
        files.sort();

        if files.is_empty() {
            return Err(LoadError::InvalidModule(format!(
                "No .prim files found in {}",
                dir.display()
            )));
        }

        let mut module_name: Option<String> = None;
        let mut module_files = Vec::new();
        let mut imports = Vec::new();
        let mut import_index = HashMap::new();

        for file in &files {
            let source = std::fs::read_to_string(file)?;
            let (ast, _diagnostics) = prim_parse::parse(&source);
            let ast = ast
                .map_err(|err| LoadError::InvalidModule(format!("{}: {}", file.display(), err)))?;

            let this_name = ast
                .module_name
                .as_ref()
                .map(|ident| ast.resolve(ident.sym).to_string())
                .ok_or_else(|| {
                    LoadError::InvalidModule(format!(
                        "{}: missing 'mod <name>' declaration at top of file",
                        file.display()
                    ))
                })?;

            match &module_name {
                None => module_name = Some(this_name.clone()),
                Some(existing) if existing == &this_name => {}
                Some(existing) => {
                    return Err(LoadError::InvalidModule(format!(
                        "Module name mismatch: {} declares '{}', expected '{}'",
                        file.display(),
                        this_name,
                        existing
                    )));
                }
            }

            let planned = plan_import_requests(
                &ast.imports,
                &ast.interner,
                &self.module_root,
                &self.std_root,
            )?;
            for ImportRequest { module, coverage } in planned {
                merge_import_request(&mut imports, &mut import_index, module, coverage);
            }

            module_files.push(self.alloc_file(file.clone(), ast));
        }

        let module_name = module_name.unwrap();
        if module_name != "main" {
            return Err(LoadError::InvalidModule(format!(
                "Binary module must be named 'main', found '{}'",
                module_name
            )));
        }

        if self.options.include_prelude {
            inject_prelude_imports(
                &mut imports,
                &mut import_index,
                &[module_name.clone()],
                ModuleOrigin::User,
            );
        }

        let exports = collect_exports(&module_files);
        let module_segments = vec![module_name.clone()];
        let module_id = ModuleId(self.program.modules.len() as u32);
        let module = Module {
            id: module_id,
            name: module_segments.clone(),
            files: module_files,
            imports: imports.clone(),
            exports,
        };

        let key = ModuleKey::Name(module_segments.clone());
        self.program.module_index.insert(key, module_id);
        self.program.root = module_id;
        self.program.modules.push(module);

        let mut stack = Vec::new();
        for ImportRequest { module, .. } in &imports {
            self.ensure_module(module, &mut stack)?;
        }
        self.validate_imports(&imports)?;

        Ok(LoadedProgram {
            program: self.program.clone(),
        })
    }

    fn ensure_module(
        &mut self,
        module_segments: &[String],
        stack: &mut Vec<String>,
    ) -> Result<ModuleId, LoadError> {
        let module_key = module_segments.join(".");
        if stack.contains(&module_key) {
            let mut cycle = stack.clone();
            cycle.push(module_key.clone());
            return Err(LoadError::InvalidModule(format!(
                "Import cycle detected: {}",
                cycle.join(" -> ")
            )));
        }
        stack.push(module_key.clone());

        let key = ModuleKey::Name(module_segments.to_vec());
        if let Some(id) = self.program.module_index.get(&key) {
            stack.pop();
            return Ok(*id);
        }

        let entry = self.load_module_entry(module_segments)?.clone();
        let exports = collect_exports(&entry.files);
        let id = ModuleId(self.program.modules.len() as u32);
        let module = Module {
            id,
            name: module_segments.to_vec(),
            files: entry.files.clone(),
            imports: entry.imports.clone(),
            exports,
        };
        self.program.module_index.insert(key, id);
        self.program.modules.push(module);

        // Load dependencies of this module as well.
        for ImportRequest { module, .. } in &entry.imports {
            self.ensure_module(module, stack)?;
        }

        // Validate imports of this module (including selective symbol imports).
        // Validation needs the imported modules to have been loaded into `module_index`.
        self.validate_imports(&entry.imports)?;

        stack.pop();

        Ok(id)
    }

    fn load_module_entry(
        &mut self,
        module_segments: &[String],
    ) -> Result<&ModuleCacheEntry, LoadError> {
        let module_key = module_segments.join(".");
        if !self.module_cache.contains_key(&module_key) {
            let search_path =
                module_search_path(&self.module_root, &self.std_root, module_segments);
            let mut files: Vec<PathBuf> = if search_path.is_dir() {
                std::fs::read_dir(&search_path)?
                    .filter_map(|entry| entry.ok())
                    .map(|entry| entry.path())
                    .filter(|path| path.extension().is_some_and(|ext| ext == "prim"))
                    .collect()
            } else {
                let file_candidate = search_path.with_extension("prim");
                if file_candidate.exists() {
                    vec![file_candidate]
                } else {
                    Vec::new()
                }
            };
            files.sort();

            if files.is_empty() {
                return Err(LoadError::InvalidModule(format!(
                    "Imported module '{}' not found at {}",
                    module_key,
                    search_path.display()
                )));
            }

            let mut module_name: Option<String> = None;
            let mut imports = Vec::new();
            let mut import_index = HashMap::new();
            let mut module_files = Vec::new();

            for file in &files {
                let source = std::fs::read_to_string(file)?;
                let (program, _diagnostics) = prim_parse::parse(&source);
                let program = program.map_err(|err| {
                    LoadError::InvalidModule(format!("{}: {}", file.display(), err))
                })?;

                let this_name = program
                    .module_name
                    .as_ref()
                    .map(|ident| program.resolve(ident.sym).to_string())
                    .ok_or_else(|| {
                        LoadError::InvalidModule(format!(
                            "{}: missing 'mod <name>' declaration at top of file",
                            file.display()
                        ))
                    })?;

                match &module_name {
                    None => module_name = Some(this_name.clone()),
                    Some(existing) if existing == &this_name => {}
                    Some(existing) => {
                        return Err(LoadError::InvalidModule(format!(
                            "Module name mismatch: {} declares '{}', expected '{}'",
                            file.display(),
                            this_name,
                            existing
                        )));
                    }
                }

                let planned = plan_import_requests(
                    &program.imports,
                    &program.interner,
                    &self.module_root,
                    &self.std_root,
                )?;
                for ImportRequest { module, coverage } in planned {
                    merge_import_request(&mut imports, &mut import_index, module, coverage);
                }

                module_files.push(self.alloc_file(file.clone(), program));
            }

            if self.options.include_prelude {
                inject_prelude_imports(
                    &mut imports,
                    &mut import_index,
                    module_segments,
                    module_origin(module_segments),
                );
            }

            self.module_cache.insert(
                module_key.clone(),
                ModuleCacheEntry {
                    imports,
                    files: module_files,
                },
            );
        }

        Ok(self.module_cache.get(&module_key).unwrap())
    }
}

impl Loader {
    fn validate_imports(&self, imports: &[ImportRequest]) -> Result<(), LoadError> {
        for ImportRequest { module, coverage } in imports {
            if let ImportCoverage::Symbols(symbols) = coverage {
                let key = ModuleKey::Name(module.clone());
                let Some(&module_id) = self.program.module_index.get(&key) else {
                    return Err(LoadError::InvalidModule(format!(
                        "Imported module '{}' not found",
                        module.join(".")
                    )));
                };
                let module = &self.program.modules[module_id.0 as usize];
                for sym in symbols {
                    if !module.exports.contains(sym) {
                        return Err(LoadError::InvalidModule(format!(
                            "Module '{}' does not define '{}'",
                            module.name.join("."),
                            sym
                        )));
                    }
                }
            }
        }
        Ok(())
    }
}

fn module_origin(module_segments: &[String]) -> ModuleOrigin {
    if module_segments.first().is_some_and(|s| s == "std") {
        ModuleOrigin::Stdlib
    } else {
        ModuleOrigin::User
    }
}

fn collect_exports(files: &[ModuleFile]) -> ExportTable {
    let mut exports = ExportTable::default();
    for file in files {
        for s in &file.ast.structs {
            exports
                .structs
                .push(file.ast.resolve(s.name.sym).to_string());
        }
        for f in &file.ast.functions {
            let name = file.ast.resolve(f.name.sym);
            exports.functions.push(name.to_string());
        }
        for t in &file.ast.traits {
            exports
                .traits
                .push(file.ast.resolve(t.name.sym).to_string());
        }
        for im in &file.ast.impls {
            let trait_name = file.ast.resolve(im.trait_name.sym);
            let struct_name = file.ast.resolve(im.struct_name.sym);
            exports
                .impls
                .push(format!("impl {} for {}", trait_name, struct_name));
        }
    }
    exports
}

fn plan_import_requests(
    imports: &[ImportDecl],
    interner: &prim_parse::Interner,
    module_root: &Path,
    std_root: &Path,
) -> Result<Vec<ImportRequest>, LoadError> {
    let mut acc = Vec::new();
    let mut index = HashMap::new();
    for decl in imports {
        let (module, coverage) = convert_import_decl(decl, interner, module_root, std_root)?;
        merge_import_request(&mut acc, &mut index, module, coverage);
    }
    Ok(acc)
}

fn convert_import_decl(
    decl: &ImportDecl,
    interner: &prim_parse::Interner,
    module_root: &Path,
    std_root: &Path,
) -> Result<(Vec<String>, ImportCoverage), LoadError> {
    let mut module_segments = decl.module_segments(interner);
    if module_segments.is_empty() {
        return Err(LoadError::InvalidModule(
            "Import must specify at least one segment".into(),
        ));
    }

    match &decl.selector {
        ImportSelector::All => {
            if module_exists(module_root, std_root, &module_segments) {
                return Ok((module_segments, ImportCoverage::All));
            }
            if let Some(trailing_ident) = &decl.trailing_symbol {
                if module_segments.len() < 2 {
                    let module_display = module_segments.join(".");
                    let search_path = module_search_path(module_root, std_root, &module_segments);
                    return Err(LoadError::InvalidModule(format!(
                        "Imported module '{}' not found at {}",
                        module_display,
                        search_path.display()
                    )));
                }
                let symbol_name = interner
                    .resolve(trailing_ident.sym)
                    .expect("missing interned symbol")
                    .to_string();
                module_segments.pop();
                if !module_exists(module_root, std_root, &module_segments) {
                    let module_display = module_segments.join(".");
                    let search_path = module_search_path(module_root, std_root, &module_segments);
                    return Err(LoadError::InvalidModule(format!(
                        "Imported module '{}' not found at {}",
                        module_display,
                        search_path.display()
                    )));
                }
                Ok((module_segments, ImportCoverage::Symbols(vec![symbol_name])))
            } else {
                let module_display = module_segments.join(".");
                let search_path = module_search_path(module_root, std_root, &module_segments);
                Err(LoadError::InvalidModule(format!(
                    "Imported module '{}' not found at {}",
                    module_display,
                    search_path.display()
                )))
            }
        }
        ImportSelector::Named(_) => {
            if !module_exists(module_root, std_root, &module_segments) {
                let module_display = module_segments.join(".");
                let search_path = module_search_path(module_root, std_root, &module_segments);
                return Err(LoadError::InvalidModule(format!(
                    "Imported module '{}' not found at {}",
                    module_display,
                    search_path.display()
                )));
            }
            let mut symbols = Vec::new();
            if let Some(names) = decl.selector_names(interner) {
                symbols.extend(names);
            }
            Ok((module_segments, ImportCoverage::Symbols(symbols)))
        }
    }
}

fn merge_import_request(
    acc: &mut Vec<ImportRequest>,
    index: &mut HashMap<String, usize>,
    module: Vec<String>,
    coverage: ImportCoverage,
) {
    let key = module.join(".");
    if let Some(&slot) = index.get(&key) {
        match (&mut acc[slot].coverage, coverage) {
            (ImportCoverage::All, _) => {}
            (_, ImportCoverage::All) => {
                acc[slot].coverage = ImportCoverage::All;
            }
            (ImportCoverage::Symbols(existing), ImportCoverage::Symbols(mut symbols)) => {
                existing.append(&mut symbols);
                existing.sort();
                existing.dedup();
            }
        }
    } else {
        index.insert(key, acc.len());
        acc.push(ImportRequest { module, coverage });
    }
}

fn module_exists(module_root: &Path, std_root: &Path, segments: &[String]) -> bool {
    if segments.is_empty() {
        return false;
    }
    let search_path = module_search_path(module_root, std_root, segments);
    if search_path.is_dir() {
        std::fs::read_dir(&search_path)
            .ok()
            .into_iter()
            .flat_map(|iter| iter.filter_map(|e| e.ok()))
            .any(|entry| entry.path().extension().is_some_and(|ext| ext == "prim"))
    } else {
        search_path.with_extension("prim").exists()
    }
}

fn module_search_path(module_root: &Path, std_root: &Path, segments: &[String]) -> PathBuf {
    if segments.first().is_some_and(|s| s == "std") {
        let mut path = std_root.to_path_buf();
        for seg in segments {
            path.push(seg);
        }
        path
    } else {
        let mut path = module_root.to_path_buf();
        for seg in segments {
            path.push(seg);
        }
        path
    }
}

fn prim_root_from_env_or_exe() -> Result<PathBuf, LoadError> {
    if let Ok(path) = env::var("PRIM_ROOT") {
        return Ok(PathBuf::from(path));
    }

    let exe = env::current_exe()?;
    let mut cur = exe.parent().ok_or_else(|| {
        LoadError::Io(std::io::Error::other("current_exe has no parent directory"))
    })?;

    // Walk up looking for .../bin/, which would indicate PRIM_ROOT/bin/<binary>.
    for _ in 0..8 {
        if let Some(name) = cur.file_name().and_then(|n| n.to_str()) {
            if name == "bin" {
                if let Some(root) = cur.parent() {
                    return Ok(root.to_path_buf());
                }
            }
        }
        if cur.join("prim-std").exists() || cur.join("src").join("std").exists() {
            return Ok(cur.to_path_buf());
        }
        if let Some(parent) = cur.parent() {
            cur = parent;
        } else {
            break;
        }
    }

    Err(LoadError::Io(std::io::Error::other(
        "could not determine PRIM_ROOT; set PRIM_ROOT env var",
    )))
}

fn default_std_root(prim_root: &Path) -> PathBuf {
    let direct = prim_root.join("src");
    if direct.join("std").exists() {
        return direct;
    }
    let alt = prim_root.join("prim-std").join("src");
    if alt.exists() {
        return alt;
    }
    direct
}
