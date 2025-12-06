use crate::program::{
    FileId, ModuleFile, ModuleId, NameRef, Program, SymbolId, SymbolInfo, SymbolKind,
};
use prim_parse::{Expr, Span, Stmt};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ResolveError {
    DuplicateSymbol { name: String, module: ModuleId },
}

pub fn resolve_names(program: &mut Program) -> Result<(), ResolveError> {
    let mut resolver = NameResolver::new(program);
    resolver.collect_symbols()?;
    resolver.resolve_uses();
    Ok(())
}

struct NameResolver<'a> {
    program: &'a mut Program,
    next_symbol: u32,
    module_scopes: HashMap<ModuleId, HashMap<String, SymbolId>>,
}

impl<'a> NameResolver<'a> {
    fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            next_symbol: 0,
            module_scopes: HashMap::new(),
        }
    }

    fn collect_symbols(&mut self) -> Result<(), ResolveError> {
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
            })?;
            scope.insert(
                module.name.last().cloned().unwrap_or_default(),
                module_symbol,
            );

            for file in &module.files {
                self.collect_file_symbols(module.id, file, &mut scope)?;
            }
            self.module_scopes.insert(module.id, scope);
        }
        Ok(())
    }

    fn collect_file_symbols(
        &mut self,
        module_id: ModuleId,
        file: &ModuleFile,
        scope: &mut HashMap<String, SymbolId>,
    ) -> Result<(), ResolveError> {
        let source = &file.source;
        for s in &file.ast.structs {
            let name = s.name.text(source).to_string();
            if scope.contains_key(&name) {
                return Err(ResolveError::DuplicateSymbol {
                    name,
                    module: module_id,
                });
            }
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Struct,
                module: Some(module_id),
                file: file.file_id,
                span: s.span,
            })?;
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
            let name = func.name.text(source).to_string();
            if scope.contains_key(&name) {
                return Err(ResolveError::DuplicateSymbol {
                    name,
                    module: module_id,
                });
            }
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Function,
                module: Some(module_id),
                file: file.file_id,
                span: func.span,
            })?;
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
                return Err(ResolveError::DuplicateSymbol {
                    name,
                    module: module_id,
                });
            }
            let sym = self.insert_symbol(SymbolInfo {
                id: SymbolId(0),
                name: name.clone(),
                kind: SymbolKind::Trait,
                module: Some(module_id),
                file: file.file_id,
                span: tr.span,
            })?;
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

        Ok(())
    }

    fn insert_symbol(&mut self, mut info: SymbolInfo) -> Result<SymbolId, ResolveError> {
        let id = SymbolId(self.next_symbol);
        self.next_symbol += 1;
        info.id = id;
        self.program.name_resolution.symbols.push(info);
        Ok(id)
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
                self.resolve_file_uses(file, &module_scope, &mut local_scope);
            }
        }
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
                    self.find_symbol(param.name.text(source), file.file_id, SymbolKind::Param)
                {
                    local_scope.insert(param.name.text(source).to_string(), sym);
                }
            }
            for stmt in &func.body {
                self.resolve_stmt(stmt, file.file_id, source, module_scope, local_scope);
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
            Stmt::Let { name, value, .. } => {
                self.resolve_expr(value, file_id, source, module_scope, local_scope);
                if let Some(sym) = self.find_symbol(name.text(source), file_id, SymbolKind::Local) {
                    local_scope.insert(name.text(source).to_string(), sym);
                } else {
                    let sym = self
                        .insert_symbol(SymbolInfo {
                            id: SymbolId(0),
                            name: name.text(source).to_string(),
                            kind: SymbolKind::Local,
                            module: None,
                            file: file_id,
                            span: *name,
                        })
                        .ok();
                    if let Some(sym) = sym {
                        local_scope.insert(name.text(source).to_string(), sym);
                    }
                }
            }
            Stmt::Expr(expr) => {
                self.resolve_expr(expr, file_id, source, module_scope, local_scope);
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
                }
                for field in fields {
                    self.resolve_expr(&field.value, file_id, source, module_scope, local_scope);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left, file_id, source, module_scope, local_scope);
                self.resolve_expr(right, file_id, source, module_scope, local_scope);
            }
            Expr::FieldAccess { object, .. } => {
                self.resolve_expr(object, file_id, source, module_scope, local_scope);
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
}
