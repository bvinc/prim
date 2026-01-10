use crate::program::{
    FileId, ImportCoverage, ImportRequest, ModuleFile, ModuleId, NameRef, Program, SymbolId,
    SymbolInfo, SymbolKind,
};
use prim_parse::{Expr, Span, Stmt};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ResolveError {
    DuplicateSymbol {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownSymbol {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownFunction {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownStruct {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownField {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownType {
        name: String,
        file: FileId,
        span: Span,
    },
}

impl ResolveError {
    pub fn span(&self) -> Span {
        match self {
            ResolveError::DuplicateSymbol { span, .. }
            | ResolveError::UnknownSymbol { span, .. }
            | ResolveError::UnknownFunction { span, .. }
            | ResolveError::UnknownStruct { span, .. }
            | ResolveError::UnknownField { span, .. }
            | ResolveError::UnknownType { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::DuplicateSymbol { name, .. } => {
                write!(f, "Duplicate symbol '{}'", name)
            }
            ResolveError::UnknownSymbol { name, .. } => write!(f, "Unknown symbol '{}'", name),
            ResolveError::UnknownFunction { name, .. } => write!(f, "Unknown function '{}'", name),
            ResolveError::UnknownStruct { name, .. } => write!(f, "Unknown struct '{}'", name),
            ResolveError::UnknownField { name, .. } => write!(f, "Unknown field '{}'", name),
            ResolveError::UnknownType { name, .. } => write!(f, "Unknown type '{}'", name),
        }
    }
}

impl std::error::Error for ResolveError {}

pub fn resolve_names(program: &mut Program) -> Result<(), Vec<ResolveError>> {
    let mut resolver = NameResolver::new(program);
    resolver.collect_symbols();
    resolver.resolve_uses();
    if resolver.errors.is_empty() {
        Ok(())
    } else {
        Err(resolver.errors)
    }
}

struct NameResolver<'a> {
    program: &'a mut Program,
    next_symbol: u32,
    module_scopes: HashMap<ModuleId, HashMap<String, SymbolId>>,
    errors: Vec<ResolveError>,
}

impl<'a> NameResolver<'a> {
    fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            next_symbol: 0,
            module_scopes: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn collect_symbols(&mut self) {
        let modules = self.program.modules.clone();
        for module in modules {
            let mut scope = HashMap::new();
            // Use the first file for the module symbol span if available.
            let file_id = module.files.first().map(|f| f.file_id).unwrap_or(FileId(0));
            let module_name = module.name.join(".");
            let module_symbol = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: module_name,
                kind: SymbolKind::Module,
                module: Some(module.id),
                file: file_id,
                span: Span::empty_at(0),
            });
            scope.insert(
                module.name.last().cloned().unwrap_or_default(),
                module_symbol,
            );

            for file in &module.files {
                self.collect_file_symbols(module.id, file, &mut scope);
            }
            self.module_scopes.insert(module.id, scope);
        }
    }

    fn collect_file_symbols(
        &mut self,
        module_id: ModuleId,
        file: &ModuleFile,
        scope: &mut HashMap<String, SymbolId>,
    ) {
        let source = &file.source;
        for s in &file.ast.structs {
            let name = s.name.text(source).to_string();
            if scope.contains_key(&name) {
                self.errors.push(ResolveError::DuplicateSymbol {
                    name,
                    file: file.file_id,
                    span: s.span,
                });
                continue;
            }
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Struct,
                module: Some(module_id),
                file: file.file_id,
                span: s.span,
            });
            scope.insert(name, sym);
            for f in &s.fields {
                let _ = self.insert_symbol(SymbolInfo {
                    id: SymbolId(0),
                    name: f.name.text(source).to_string(),
                    kind: SymbolKind::Field,
                    module: Some(module_id),
                    file: file.file_id,
                    span: f.name,
                });
            }
        }

        for func in &file.ast.functions {
            let name = file.ast.resolve(func.name).to_string();
            if scope.contains_key(&name) {
                self.errors.push(ResolveError::DuplicateSymbol {
                    name,
                    file: file.file_id,
                    span: func.span,
                });
                continue;
            }
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Function,
                module: Some(module_id),
                file: file.file_id,
                span: func.span,
            });
            scope.insert(name, sym);
            for param in &func.parameters {
                let _ = self.insert_symbol(SymbolInfo {
                    id: SymbolId(0),
                    name: param.name.text(source).to_string(),
                    kind: SymbolKind::Param,
                    module: Some(module_id),
                    file: file.file_id,
                    span: param.name,
                });
            }
        }

        for tr in &file.ast.traits {
            let name = tr.name.text(source).to_string();
            if scope.contains_key(&name) {
                self.errors.push(ResolveError::DuplicateSymbol {
                    name,
                    file: file.file_id,
                    span: tr.span,
                });
                continue;
            }
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Trait,
                module: Some(module_id),
                file: file.file_id,
                span: tr.span,
            });
            scope.insert(name, sym);
        }

        for im in &file.ast.impls {
            let _ = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: format!(
                    "impl {} for {}",
                    im.trait_name.text(source),
                    im.struct_name.text(source)
                ),
                kind: SymbolKind::Impl,
                module: Some(module_id),
                file: file.file_id,
                span: im.span,
            });
        }
    }

    fn insert_symbol(&mut self, mut info: SymbolInfo) -> SymbolId {
        let id = SymbolId(self.next_symbol);
        self.next_symbol += 1;
        info.id = id;
        self.program.name_resolution.symbols.push(info);
        id
    }

    fn resolve_uses(&mut self) {
        let modules = self.program.modules.clone();
        for module in modules {
            let module_scope = self
                .module_scopes
                .get(&module.id)
                .cloned()
                .unwrap_or_default();
            for file in &module.files {
                let mut local_scope = HashMap::new();
                let effective_scope = self.scope_with_imports(&module_scope, &module.imports);
                self.resolve_file_uses(file, &effective_scope, &mut local_scope);
            }
        }
    }

    fn scope_with_imports(
        &self,
        module_scope: &HashMap<String, SymbolId>,
        imports: &[ImportRequest],
    ) -> HashMap<String, SymbolId> {
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
                    // Import all top-level names unqualified (merged compilation unit behavior).
                    let skip = module.last().map(|s| s.as_str()).unwrap_or("");
                    for (name, sym) in import_scope {
                        if name == skip {
                            continue;
                        }
                        scope.insert(name.clone(), *sym);
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

    fn resolve_file_uses(
        &mut self,
        file: &ModuleFile,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, SymbolId>,
    ) {
        let source = &file.source;

        for func in &file.ast.functions {
            local_scope.clear();
            for param in &func.parameters {
                if let Some(sym) =
                    self.find_symbol_at_span(file.file_id, param.name, SymbolKind::Param)
                {
                    local_scope.insert(param.name.text(source).to_string(), sym);
                }
                self.resolve_type_use(&param.type_annotation, file.file_id, source, module_scope);
            }
            if let Some(ret) = &func.return_type {
                self.resolve_type_use(ret, file.file_id, source, module_scope);
            }
            for stmt in &func.body {
                self.resolve_stmt(stmt, file.file_id, source, module_scope, local_scope);
            }
        }

        for s in &file.ast.structs {
            for field in &s.fields {
                self.resolve_type_use(&field.field_type, file.file_id, source, module_scope);
            }
        }

        for tr in &file.ast.traits {
            for m in &tr.methods {
                for p in &m.parameters {
                    self.resolve_type_use(&p.type_annotation, file.file_id, source, module_scope);
                }
                if let Some(ret) = &m.return_type {
                    self.resolve_type_use(ret, file.file_id, source, module_scope);
                }
            }
        }

        for im in &file.ast.impls {
            self.resolve_type_use(
                &prim_parse::Type::Struct(im.struct_name),
                file.file_id,
                source,
                module_scope,
            );
            for m in &im.methods {
                for p in &m.parameters {
                    self.resolve_type_use(&p.type_annotation, file.file_id, source, module_scope);
                }
                if let Some(ret) = &m.return_type {
                    self.resolve_type_use(ret, file.file_id, source, module_scope);
                }
            }
        }
    }

    fn resolve_stmt(
        &mut self,
        stmt: &Stmt,
        file_id: FileId,
        source: &str,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, SymbolId>,
    ) {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                if let Some(ann) = type_annotation {
                    self.resolve_type_use(ann, file_id, source, module_scope);
                }
                self.resolve_expr(value, file_id, source, module_scope, local_scope);
                // Always create a fresh symbol for each `let` binding (locals are scope-based,
                // not file-based; reusing by name breaks `def_lookup` and shadowing).
                let sym = self.insert_symbol(SymbolInfo {
                    id: SymbolId(0),
                    name: name.text(source).to_string(),
                    kind: SymbolKind::Local,
                    module: None,
                    file: file_id,
                    span: *name,
                });
                local_scope.insert(name.text(source).to_string(), sym);
            }
            Stmt::Assign { target, value } => {
                self.resolve_expr(value, file_id, source, module_scope, local_scope);
                // Look up the target in local scope - it must already be defined
                let name = target.text(source);
                if let Some(&sym) = local_scope.get(name) {
                    // Record the use of the target symbol
                    let key = NameRef {
                        file: file_id,
                        span: *target,
                    };
                    self.program.name_resolution.uses.insert(key, sym);
                } else {
                    self.errors.push(ResolveError::UnknownSymbol {
                        name: name.to_string(),
                        file: file_id,
                        span: *target,
                    });
                }
            }
            Stmt::Expr(expr) => {
                self.resolve_expr(expr, file_id, source, module_scope, local_scope);
            }
            Stmt::If {
                condition,
                then_body,
                else_body,
                ..
            } => {
                self.resolve_expr(condition, file_id, source, module_scope, local_scope);
                for stmt in then_body {
                    self.resolve_stmt(stmt, file_id, source, module_scope, local_scope);
                }
                if let Some(body) = else_body {
                    for stmt in body {
                        self.resolve_stmt(stmt, file_id, source, module_scope, local_scope);
                    }
                }
            }
            Stmt::Loop { body, .. } => {
                for stmt in body {
                    self.resolve_stmt(stmt, file_id, source, module_scope, local_scope);
                }
            }
            Stmt::Break { .. } => {}
        }
    }

    fn resolve_expr(
        &mut self,
        expr: &Expr,
        file_id: FileId,
        source: &str,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, SymbolId>,
    ) {
        match expr {
            Expr::Identifier { span, .. } => {
                let name = span.text(source);
                if let Some(sym) = local_scope
                    .get(name)
                    .copied()
                    .or_else(|| module_scope.get(name).copied())
                {
                    let key = NameRef {
                        file: file_id,
                        span: *span,
                    };
                    self.program.name_resolution.uses.insert(key, sym);
                } else {
                    self.errors.push(ResolveError::UnknownSymbol {
                        name: name.to_string(),
                        file: file_id,
                        span: *span,
                    });
                }
            }
            Expr::FunctionCall { path, args, .. } => {
                let target = path.segments.last().map(|seg| seg.text(source).to_string());
                if let Some(name) = target {
                    let sym = if path.segments.len() == 1 {
                        local_scope
                            .get(&name)
                            .copied()
                            .or_else(|| module_scope.get(&name).copied())
                    } else {
                        let module_name: Vec<String> = path
                            .segments
                            .iter()
                            .take(path.segments.len() - 1)
                            .map(|s| s.text(source).to_string())
                            .collect();
                        self.resolve_module_symbol(&module_name, &name)
                    };
                    if let Some(sym) = sym {
                        let key = NameRef {
                            file: file_id,
                            span: *path.segments.last().unwrap(),
                        };
                        self.program.name_resolution.uses.insert(key, sym);
                    } else {
                        let full_name = if path.segments.len() == 1 {
                            name
                        } else {
                            let parts: Vec<String> = path
                                .segments
                                .iter()
                                .map(|s| s.text(source).to_string())
                                .collect();
                            parts.join(".")
                        };
                        self.errors.push(ResolveError::UnknownFunction {
                            name: full_name,
                            file: file_id,
                            span: *path.segments.last().unwrap(),
                        });
                    }
                }
                for arg in args {
                    self.resolve_expr(arg, file_id, source, module_scope, local_scope);
                }
            }
            Expr::StructLiteral { name, fields, .. } => {
                let struct_name = name.text(source);
                if let Some(sym) = module_scope.get(struct_name) {
                    let key = NameRef {
                        file: file_id,
                        span: *name,
                    };
                    self.program.name_resolution.uses.insert(key, *sym);
                } else {
                    self.errors.push(ResolveError::UnknownStruct {
                        name: struct_name.to_string(),
                        file: file_id,
                        span: *name,
                    });
                }
                for field in fields {
                    if let Some(sym) =
                        self.find_symbol(field.name.text(source), file_id, SymbolKind::Field)
                    {
                        let key = NameRef {
                            file: file_id,
                            span: field.name,
                        };
                        self.program.name_resolution.uses.insert(key, sym);
                    } else {
                        self.errors.push(ResolveError::UnknownField {
                            name: field.name.text(source).to_string(),
                            file: file_id,
                            span: field.name,
                        });
                    }
                    self.resolve_expr(&field.value, file_id, source, module_scope, local_scope);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left, file_id, source, module_scope, local_scope);
                self.resolve_expr(right, file_id, source, module_scope, local_scope);
            }
            Expr::FieldAccess { object, field, .. } => {
                self.resolve_expr(object, file_id, source, module_scope, local_scope);
                if let Some(sym) = self.find_symbol(field.text(source), file_id, SymbolKind::Field)
                {
                    let key = NameRef {
                        file: file_id,
                        span: *field,
                    };
                    self.program.name_resolution.uses.insert(key, sym);
                } else {
                    self.errors.push(ResolveError::UnknownField {
                        name: field.text(source).to_string(),
                        file: file_id,
                        span: *field,
                    });
                }
            }
            Expr::Dereference { operand, .. } => {
                self.resolve_expr(operand, file_id, source, module_scope, local_scope);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.resolve_expr(elem, file_id, source, module_scope, local_scope);
                }
            }
            Expr::IntLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::BoolLiteral { .. }
            | Expr::StringLiteral { .. } => {}
        }
    }

    fn resolve_type_use(
        &mut self,
        ty: &prim_parse::Type,
        file_id: FileId,
        source: &str,
        module_scope: &HashMap<String, SymbolId>,
    ) {
        match ty {
            prim_parse::Type::Struct(span) => {
                let name = span.text(source).to_string();
                if let Some(sym) = module_scope.get(&name).copied() {
                    let key = NameRef {
                        file: file_id,
                        span: *span,
                    };
                    self.program.name_resolution.uses.insert(key, sym);
                } else {
                    self.errors.push(ResolveError::UnknownType {
                        name,
                        file: file_id,
                        span: *span,
                    });
                }
            }
            prim_parse::Type::Array(inner) => {
                self.resolve_type_use(inner, file_id, source, module_scope)
            }
            prim_parse::Type::Pointer { pointee, .. } => {
                self.resolve_type_use(pointee, file_id, source, module_scope)
            }
            _ => {}
        }
    }

    fn resolve_module_symbol(&self, module_segments: &[String], name: &str) -> Option<SymbolId> {
        let key = crate::program::ModuleKey::Name(module_segments.to_vec());
        let module_id = self.program.module_index.get(&key)?;
        let scope = self.module_scopes.get(module_id)?;
        scope.get(name).copied()
    }

    fn find_symbol(&self, name: &str, file: FileId, kind: SymbolKind) -> Option<SymbolId> {
        self.program
            .name_resolution
            .symbols
            .iter()
            .find(|info| info.name == name && info.file == file && info.kind == kind)
            .map(|info| info.id)
    }

    fn find_symbol_at_span(&self, file: FileId, span: Span, kind: SymbolKind) -> Option<SymbolId> {
        self.program
            .name_resolution
            .symbols
            .iter()
            .find(|info| info.file == file && info.span == span && info.kind == kind)
            .map(|info| info.id)
    }
}
