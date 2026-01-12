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
    AssignToImmutable {
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
            | ResolveError::AssignToImmutable { span, .. }
            | ResolveError::UnknownSymbol { span, .. }
            | ResolveError::UnknownFunction { span, .. }
            | ResolveError::UnknownStruct { span, .. }
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
            ResolveError::AssignToImmutable { name, .. } => {
                write!(f, "Cannot assign to immutable variable '{}'", name)
            }
            ResolveError::UnknownSymbol { name, .. } => write!(f, "Unknown symbol '{}'", name),
            ResolveError::UnknownFunction { name, .. } => write!(f, "Unknown function '{}'", name),
            ResolveError::UnknownStruct { name, .. } => write!(f, "Unknown struct '{}'", name),
            ResolveError::UnknownType { name, .. } => write!(f, "Unknown type '{}'", name),
        }
    }
}

impl std::error::Error for ResolveError {}

#[derive(Clone, Copy)]
struct LocalBinding {
    symbol: SymbolId,
    mutable: bool,
}

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
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Struct,
                module: Some(module_id),
                file: file.file_id,
                span: s.span,
            });
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
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Function,
                module: Some(module_id),
                file: file.file_id,
                span: func.span,
            });
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

        for im in &ast.impls {
            let _ = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: format!(
                    "impl {} for {}",
                    ast.resolve(im.trait_name),
                    ast.resolve(im.struct_name)
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
                let mut local_scope: HashMap<String, LocalBinding> = HashMap::new();
                let effective_scope = self.scope_with_imports(&module_scope, &module.imports);
                self.resolve_file_uses(module.id, file, &effective_scope, &mut local_scope);
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
        module_id: ModuleId,
        file: &ModuleFile,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, LocalBinding>,
    ) {
        let ast = &file.ast;

        for func in &ast.functions {
            local_scope.clear();
            for param in &func.parameters {
                let name_str = ast.resolve(param.name).to_string();
                let symbol = self.insert_symbol(SymbolInfo {
                    id: SymbolId(0),
                    name: name_str.clone(),
                    kind: SymbolKind::Param,
                    module: Some(module_id),
                    file: file.file_id,
                    span: param.name_span,
                });
                local_scope.insert(
                    name_str,
                    LocalBinding {
                        symbol,
                        mutable: false,
                    },
                );
                self.resolve_type_use(&param.type_annotation, file.file_id, ast, module_scope);
            }
            if let Some(ret) = &func.return_type {
                self.resolve_type_use(ret, file.file_id, ast, module_scope);
            }
            self.resolve_block(&func.body, file.file_id, ast, module_scope, local_scope);
        }

        for s in &ast.structs {
            for field in &s.fields {
                self.resolve_type_use(&field.field_type, file.file_id, ast, module_scope);
            }
        }

        for tr in &ast.traits {
            for m in &tr.methods {
                for p in &m.parameters {
                    self.resolve_type_use(&p.type_annotation, file.file_id, ast, module_scope);
                }
                if let Some(ret) = &m.return_type {
                    self.resolve_type_use(ret, file.file_id, ast, module_scope);
                }
            }
        }

        for im in &ast.impls {
            self.resolve_type_use(
                &prim_parse::Type::Struct {
                    name: im.struct_name,
                    span: im.struct_name_span,
                },
                file.file_id,
                ast,
                module_scope,
            );
            for m in &im.methods {
                for p in &m.parameters {
                    self.resolve_type_use(&p.type_annotation, file.file_id, ast, module_scope);
                }
                if let Some(ret) = &m.return_type {
                    self.resolve_type_use(ret, file.file_id, ast, module_scope);
                }
            }
        }
    }

    fn resolve_stmt(
        &mut self,
        stmt: &Stmt,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, LocalBinding>,
    ) {
        match stmt {
            Stmt::Let {
                name,
                name_span,
                mutable,
                type_annotation,
                value,
            } => {
                if let Some(ann) = type_annotation {
                    self.resolve_type_use(ann, file_id, ast, module_scope);
                }
                self.resolve_expr(value, file_id, ast, module_scope, local_scope);
                // Always create a fresh symbol for each `let` binding (locals are scope-based,
                // not file-based; reusing by name breaks `def_lookup` and shadowing).
                let name_str = ast.resolve(*name).to_string();
                let symbol = self.insert_symbol(SymbolInfo {
                    id: SymbolId(0),
                    name: name_str.clone(),
                    kind: SymbolKind::Local,
                    module: None,
                    file: file_id,
                    span: *name_span,
                });
                local_scope.insert(
                    name_str,
                    LocalBinding {
                        symbol,
                        mutable: *mutable,
                    },
                );
            }
            Stmt::Assign {
                target,
                target_span,
                value,
            } => {
                self.resolve_expr(value, file_id, ast, module_scope, local_scope);
                // Look up the target in local scope - it must already be defined
                let name = ast.resolve(*target);
                if let Some(binding) = local_scope.get(name) {
                    if !binding.mutable {
                        self.errors.push(ResolveError::AssignToImmutable {
                            name: name.to_string(),
                            file: file_id,
                            span: *target_span,
                        });
                    }
                    // Record the use of the target symbol
                    let key = NameRef {
                        file: file_id,
                        span: *target_span,
                    };
                    self.program
                        .name_resolution
                        .uses
                        .insert(key, binding.symbol);
                } else {
                    self.errors.push(ResolveError::UnknownSymbol {
                        name: name.to_string(),
                        file: file_id,
                        span: *target_span,
                    });
                }
            }
            Stmt::Expr(expr) => {
                self.resolve_expr(expr, file_id, ast, module_scope, local_scope);
            }
            Stmt::Loop { body, .. } => {
                for stmt in body {
                    self.resolve_stmt(stmt, file_id, ast, module_scope, local_scope);
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.resolve_expr(condition, file_id, ast, module_scope, local_scope);
                for stmt in body {
                    self.resolve_stmt(stmt, file_id, ast, module_scope, local_scope);
                }
            }
            Stmt::Break { .. } => {}
        }
    }

    fn resolve_expr(
        &mut self,
        expr: &Expr,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, LocalBinding>,
    ) {
        match expr {
            Expr::Identifier { name, span, .. } => {
                let name_str = ast.resolve(*name);
                if let Some(sym) = local_scope
                    .get(name_str)
                    .map(|b| b.symbol)
                    .or_else(|| module_scope.get(name_str).copied())
                {
                    let key = NameRef {
                        file: file_id,
                        span: *span,
                    };
                    self.program.name_resolution.uses.insert(key, sym);
                } else {
                    self.errors.push(ResolveError::UnknownSymbol {
                        name: name_str.to_string(),
                        file: file_id,
                        span: *span,
                    });
                }
            }
            Expr::FunctionCall { path, args, .. } => {
                let target = path
                    .segments
                    .last()
                    .map(|(sym, _span)| ast.resolve(*sym).to_string());
                if let Some(name) = target {
                    let sym = if path.segments.len() == 1 {
                        local_scope
                            .get(&name)
                            .map(|b| b.symbol)
                            .or_else(|| module_scope.get(&name).copied())
                    } else {
                        let module_name: Vec<String> = path
                            .segments
                            .iter()
                            .take(path.segments.len() - 1)
                            .map(|(s, _span)| ast.resolve(*s).to_string())
                            .collect();
                        self.resolve_module_symbol(&module_name, &name)
                    };
                    if let Some(sym) = sym {
                        let key = NameRef {
                            file: file_id,
                            span: path.segments.last().unwrap().1,
                        };
                        self.program.name_resolution.uses.insert(key, sym);
                    } else {
                        let full_name = if path.segments.len() == 1 {
                            name
                        } else {
                            let parts: Vec<String> = path
                                .segments
                                .iter()
                                .map(|(s, _span)| ast.resolve(*s).to_string())
                                .collect();
                            parts.join(".")
                        };
                        self.errors.push(ResolveError::UnknownFunction {
                            name: full_name,
                            file: file_id,
                            span: path.segments.last().unwrap().1,
                        });
                    }
                }
                for arg in args {
                    self.resolve_expr(arg, file_id, ast, module_scope, local_scope);
                }
            }
            Expr::StructLiteral {
                name,
                name_span,
                fields,
                ..
            } => {
                let struct_name = ast.resolve(*name);
                if let Some(sym) = module_scope.get(struct_name) {
                    let key = NameRef {
                        file: file_id,
                        span: *name_span,
                    };
                    self.program.name_resolution.uses.insert(key, *sym);
                } else {
                    self.errors.push(ResolveError::UnknownStruct {
                        name: struct_name.to_string(),
                        file: file_id,
                        span: *name_span,
                    });
                }
                // Field names are resolved during type checking.
                for field in fields {
                    self.resolve_expr(&field.value, file_id, ast, module_scope, local_scope);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left, file_id, ast, module_scope, local_scope);
                self.resolve_expr(right, file_id, ast, module_scope, local_scope);
            }
            Expr::FieldAccess { object, .. } => {
                // Field names are resolved during type checking.
                self.resolve_expr(object, file_id, ast, module_scope, local_scope);
            }
            Expr::Dereference { operand, .. } => {
                self.resolve_expr(operand, file_id, ast, module_scope, local_scope);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.resolve_expr(elem, file_id, ast, module_scope, local_scope);
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.resolve_expr(condition, file_id, ast, module_scope, local_scope);
                self.resolve_block(then_branch, file_id, ast, module_scope, local_scope);
                if let Some(else_block) = else_branch {
                    self.resolve_block(else_block, file_id, ast, module_scope, local_scope);
                }
            }
            Expr::Block { block, .. } => {
                self.resolve_block(block, file_id, ast, module_scope, local_scope);
            }
            Expr::IntLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::BoolLiteral { .. }
            | Expr::StringLiteral { .. } => {}
        }
    }

    fn resolve_block(
        &mut self,
        block: &prim_parse::Block,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &HashMap<String, SymbolId>,
        local_scope: &mut HashMap<String, LocalBinding>,
    ) {
        for stmt in &block.stmts {
            self.resolve_stmt(stmt, file_id, ast, module_scope, local_scope);
        }
        if let Some(trailing_expr) = &block.expr {
            self.resolve_expr(trailing_expr, file_id, ast, module_scope, local_scope);
        }
    }

    fn resolve_type_use(
        &mut self,
        ty: &prim_parse::Type,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &HashMap<String, SymbolId>,
    ) {
        match ty {
            prim_parse::Type::Struct { name, span } => {
                let name_str = ast.resolve(*name).to_string();
                if let Some(sym) = module_scope.get(&name_str).copied() {
                    let key = NameRef {
                        file: file_id,
                        span: *span,
                    };
                    self.program.name_resolution.uses.insert(key, sym);
                } else {
                    self.errors.push(ResolveError::UnknownType {
                        name: name_str,
                        file: file_id,
                        span: *span,
                    });
                }
            }
            prim_parse::Type::Array(inner) => {
                self.resolve_type_use(inner, file_id, ast, module_scope)
            }
            prim_parse::Type::Pointer { pointee, .. } => {
                self.resolve_type_use(pointee, file_id, ast, module_scope)
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
}
