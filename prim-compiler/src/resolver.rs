use crate::program::{
    FileId, ImportCoverage, ImportRequest, ModuleFile, ModuleId, Program, SymbolId, SymbolInfo,
    SymbolKind,
};
use prim_parse::Span;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ResolveError {
    DuplicateSymbol {
        name: String,
        file: FileId,
        span: Span,
    },
}

impl ResolveError {
    pub fn span(&self) -> Span {
        match self {
            ResolveError::DuplicateSymbol { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::DuplicateSymbol { name, .. } => write!(f, "Duplicate symbol '{}'", name),
        }
    }
}

impl std::error::Error for ResolveError {}

/// Module-level scope mapping names to symbols.
pub type ModuleScope = HashMap<String, SymbolId>;

/// All module scopes indexed by module id.
pub type ModuleScopes = HashMap<ModuleId, ModuleScope>;

/// Collect top-level symbols and build module scopes.
///
/// This phase only handles structs, functions, and traits - items that can be
/// referenced by name across the program. Local variables and parameters are
/// resolved during HIR lowering.
pub fn collect_scopes(program: &mut Program) -> Result<ModuleScopes, Vec<ResolveError>> {
    let mut collector = ScopeCollector::new(program);
    collector.collect();
    if collector.errors.is_empty() {
        Ok(collector.module_scopes)
    } else {
        Err(collector.errors)
    }
}

struct ScopeCollector<'a> {
    program: &'a mut Program,
    next_symbol: u32,
    module_scopes: ModuleScopes,
    errors: Vec<ResolveError>,
}

impl<'a> ScopeCollector<'a> {
    fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            next_symbol: 0,
            module_scopes: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn add_symbol(
        &mut self,
        name: String,
        kind: SymbolKind,
        module: Option<ModuleId>,
        file: FileId,
        span: Span,
    ) -> SymbolId {
        let id = SymbolId(self.next_symbol);
        self.next_symbol += 1;
        self.program.name_resolution.symbols.push(SymbolInfo {
            id,
            name,
            kind,
            module,
            file,
            span,
        });
        id
    }

    fn collect(&mut self) {
        let modules = self.program.modules.clone();

        // First pass: collect symbols from each module (without imports)
        for module in &modules {
            let mut scope = HashMap::new();
            let file_id = module.files.first().map(|f| f.file_id).unwrap_or(FileId(0));

            // Register the module itself
            let module_symbol = self.add_symbol(
                module.name.join("."),
                SymbolKind::Module,
                Some(module.id),
                file_id,
                Span::empty_at(0),
            );
            scope.insert(
                module.name.last().cloned().unwrap_or_default(),
                module_symbol,
            );

            // Collect items from all files in this module
            for file in &module.files {
                self.collect_file_symbols(module.id, file, &mut scope);
            }

            self.module_scopes.insert(module.id, scope);
        }

        // Second pass: apply imports to each module's scope
        for module in &modules {
            let base_scope = self
                .module_scopes
                .get(&module.id)
                .cloned()
                .unwrap_or_default();
            let effective_scope = self.apply_imports(&base_scope, &module.imports);
            self.module_scopes.insert(module.id, effective_scope);
        }
    }

    fn collect_file_symbols(
        &mut self,
        module_id: ModuleId,
        file: &ModuleFile,
        scope: &mut ModuleScope,
    ) {
        let ast = &file.ast;

        for s in &ast.structs {
            let name = ast.resolve(s.name).to_string();
            if scope.contains_key(&name) {
                self.errors.push(ResolveError::DuplicateSymbol {
                    name,
                    file: file.file_id,
                    span: s.span,
                });
                continue;
            }
            let sym = self.add_symbol(
                name.clone(),
                SymbolKind::Struct,
                Some(module_id),
                file.file_id,
                s.span,
            );
            scope.insert(name, sym);
        }

        for func in &ast.functions {
            let name = ast.resolve(func.name).to_string();
            if scope.contains_key(&name) {
                self.errors.push(ResolveError::DuplicateSymbol {
                    name,
                    file: file.file_id,
                    span: func.span,
                });
                continue;
            }
            let sym = self.add_symbol(
                name.clone(),
                SymbolKind::Function,
                Some(module_id),
                file.file_id,
                func.span,
            );
            scope.insert(name, sym);
        }

        for tr in &ast.traits {
            let name = ast.resolve(tr.name).to_string();
            if scope.contains_key(&name) {
                self.errors.push(ResolveError::DuplicateSymbol {
                    name,
                    file: file.file_id,
                    span: tr.span,
                });
                continue;
            }
            let sym = self.add_symbol(
                name.clone(),
                SymbolKind::Trait,
                Some(module_id),
                file.file_id,
                tr.span,
            );
            scope.insert(name, sym);
        }
    }

    fn apply_imports(&self, module_scope: &ModuleScope, imports: &[ImportRequest]) -> ModuleScope {
        let mut scope = module_scope.clone();

        for ImportRequest { module, coverage } in imports {
            let key = crate::program::ModuleKey::Name(module.clone());
            let Some(module_id) = self.program.module_index.get(&key).copied() else {
                continue;
            };
            let Some(import_scope) = self.module_scopes.get(&module_id) else {
                continue;
            };

            match coverage {
                ImportCoverage::All => {
                    let skip = module.last().map(|s| s.as_str()).unwrap_or("");
                    for (name, sym) in import_scope {
                        if name != skip {
                            scope.insert(name.clone(), *sym);
                        }
                    }
                }
                ImportCoverage::Symbols(names) => {
                    for name in names {
                        if let Some(sym) = import_scope.get(name) {
                            scope.insert(name.clone(), *sym);
                        }
                    }
                }
            }
        }

        scope
    }
}
