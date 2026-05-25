use crate::hir::*;
use crate::program::{Program, SymbolId as ResSymbolId, SymbolInfo, SymbolKind as ResSymbolKind};
use crate::resolver::{ModuleScope, ModuleScopes};
use prim_parse::{Expr, ExprKind, Span, Stmt, Type};
use prim_tok::{FileId, ModuleId};
use std::collections::HashMap;

#[derive(Debug)]
pub enum LoweringError {
    AssignToImmutable {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownName {
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
    UnknownModule {
        path: String,
        file: FileId,
        span: Span,
    },
}

impl std::fmt::Display for LoweringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoweringError::AssignToImmutable { name, .. } => {
                write!(f, "Cannot assign to immutable variable '{}'", name)
            }
            LoweringError::UnknownName { name, .. } => {
                write!(f, "Unknown name '{}'", name)
            }
            LoweringError::UnknownFunction { name, .. } => {
                write!(f, "Unknown function '{}'", name)
            }
            LoweringError::UnknownStruct { name, .. } => {
                write!(f, "Unknown struct '{}'", name)
            }
            LoweringError::UnknownModule { path, .. } => {
                write!(f, "Unknown module '{}'", path)
            }
        }
    }
}

impl std::error::Error for LoweringError {}

impl LoweringError {
    pub fn span(&self) -> Span {
        match self {
            LoweringError::AssignToImmutable { span, .. }
            | LoweringError::UnknownName { span, .. }
            | LoweringError::UnknownFunction { span, .. }
            | LoweringError::UnknownStruct { span, .. }
            | LoweringError::UnknownModule { span, .. } => *span,
        }
    }

    pub fn file(&self) -> FileId {
        match self {
            LoweringError::AssignToImmutable { file, .. }
            | LoweringError::UnknownName { file, .. }
            | LoweringError::UnknownFunction { file, .. }
            | LoweringError::UnknownStruct { file, .. }
            | LoweringError::UnknownModule { file, .. } => *file,
        }
    }
}

/// Lower a loaded [`Program`] into [`HirProgram`].
pub fn lower_to_hir(
    program: &Program,
    module_scopes: &ModuleScopes,
) -> Result<HirProgram, Vec<LoweringError>> {
    let mut ctx = LoweringContext::new(program, module_scopes);
    ctx.declare_modules_and_items();
    ctx.populate_items();
    if ctx.errors.is_empty() {
        Ok(ctx.finish())
    } else {
        Err(ctx.errors)
    }
}

/// Local variable binding (param or let).
#[derive(Clone, Copy)]
struct LocalBinding {
    symbol: SymbolId,
    mutable: bool,
}

/// Lexically scoped local variable bindings.
struct LocalScope {
    scopes: Vec<HashMap<String, LocalBinding>>,
}

impl LocalScope {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, binding: LocalBinding) {
        self.scopes.last_mut().unwrap().insert(name, binding);
    }

    fn get(&self, name: &str) -> Option<&LocalBinding> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.get(name) {
                return Some(b);
            }
        }
        None
    }

    fn clear(&mut self) {
        self.scopes.clear();
        self.scopes.push(HashMap::new());
    }
}

struct LoweringContext<'a> {
    program: &'a Program,
    module_scopes: &'a ModuleScopes,
    interner: Interner,
    spans: Vec<(FileId, Span)>,
    files: Vec<FileInfo>,
    symbols: SymbolTable,
    items: Items,
    modules: Vec<Module>,
    root_module: ModuleId,
    main: Option<SymbolId>,
    struct_ids: HashMap<ResSymbolId, StructId>,
    func_ids: HashMap<ResSymbolId, FuncId>,
    symbol_map: HashMap<ResSymbolId, SymbolId>,
    symbols_info: &'a [SymbolInfo],
    stdlib_str_struct: Option<StructId>,
    local_scope: LocalScope,
    errors: Vec<LoweringError>,
}

impl<'a> LoweringContext<'a> {
    fn new(program: &'a Program, module_scopes: &'a ModuleScopes) -> Self {
        let max_file = program
            .modules
            .iter()
            .flat_map(|m| m.files.iter().map(|f| f.file_id.0 as usize))
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        let mut files = vec![None; max_file];
        for module in &program.modules {
            for file in &module.files {
                let id = file.file_id;
                let slot = &mut files[id.0 as usize];
                *slot = Some(FileInfo {
                    id,
                    path: file.path.clone(),
                });
            }
        }
        let files: Vec<FileInfo> = files.into_iter().flatten().collect();

        let root_module = &program.modules[program.root.0 as usize];
        let interner = root_module
            .files
            .first()
            .map(|f| f.ast.interner.clone())
            .expect("root module should have at least one file");

        Self {
            program,
            module_scopes,
            interner,
            spans: Vec::new(),
            files,
            symbols: SymbolTable {
                entries: Vec::new(),
                by_name: HashMap::new(),
            },
            items: Items::default(),
            modules: Vec::new(),
            root_module: program.root,
            main: None,
            struct_ids: HashMap::new(),
            func_ids: HashMap::new(),
            symbol_map: HashMap::new(),
            symbols_info: &program.name_resolution.symbols,
            stdlib_str_struct: None,
            local_scope: LocalScope::new(),
            errors: Vec::new(),
        }
    }

    fn declare_modules_and_items(&mut self) {
        for module in &self.program.modules {
            let module_id = module.id;
            let files = module.files.iter().map(|f| f.file_id).collect();
            self.modules.push(Module {
                id: module_id,
                name: module.name.clone(),
                files,
                exports: Vec::new(),
                imports: Vec::new(),
            });

            for file in &module.files {
                for s in &file.ast.structs {
                    let name = file.ast.resolve(s.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let sym_id = self.ensure_symbol(res_id, Some(module_id));
                    let sid = *self
                        .struct_ids
                        .entry(res_id)
                        .or_insert_with(|| StructId(self.items.structs.len() as u32));
                    if self.stdlib_str_struct.is_none()
                        && module.name.len() == 2
                        && module.name[0] == "std"
                        && module.name[1] == "string"
                        && name == "Str"
                    {
                        self.stdlib_str_struct = Some(sid);
                    }
                    let span = self.span_id(s.span, file.file_id);
                    self.items.structs.push(HirStruct {
                        id: sid,
                        name: sym_id,
                        module: module_id,
                        file: file.file_id,
                        fields: Vec::new(),
                        span,
                    });
                }
                for f in &file.ast.functions {
                    let name = file.ast.resolve(f.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let sym_id = self.ensure_symbol(res_id, Some(module_id));
                    if self.main.is_none() && module_id == self.root_module && name == "main" {
                        self.main = Some(sym_id);
                    }
                    let fid = *self
                        .func_ids
                        .entry(res_id)
                        .or_insert_with(|| FuncId(self.items.functions.len() as u32));
                    let span = self.span_id(f.span, file.file_id);
                    self.items.functions.push(HirFunction {
                        id: fid,
                        name: sym_id,
                        module: module_id,
                        file: file.file_id,
                        params: Vec::new(),
                        ret: None,
                        body: HirBlock {
                            stmts: Vec::new(),
                            expr: None,
                        },
                        span,
                        runtime_binding: f.runtime_binding.clone(),
                    });
                }
            }
        }
    }

    fn populate_items(&mut self) {
        for module in &self.program.modules {
            let module_id = module.id;
            let module_scope = self
                .module_scopes
                .get(&module.id)
                .cloned()
                .unwrap_or_default();

            for file in &module.files {
                let ast = &file.ast;
                for s in &ast.structs {
                    let name = ast.resolve(s.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let sid = *self.struct_ids.get(&res_id).expect("missing struct id");
                    let fields = s
                        .fields
                        .iter()
                        .map(|f| HirField {
                            name: f.name.sym,
                            ty: self.lower_type(&f.field_type, ast, &module_scope),
                            span: self.span_id(f.name.span, file.file_id),
                        })
                        .collect();
                    if let Some(hir_struct) = self.items.structs.get_mut(sid.0 as usize) {
                        hir_struct.fields = fields;
                    }
                }

                for f in &ast.functions {
                    let name = ast.resolve(f.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let fid = *self.func_ids.get(&res_id).expect("missing function id");

                    self.local_scope.clear();
                    let params = f
                        .parameters
                        .iter()
                        .map(|p| {
                            let sym = self.insert_symbol(module_id, p.name.sym, SymbolKind::Param);
                            self.local_scope.insert(
                                ast.resolve(p.name.sym).to_string(),
                                LocalBinding {
                                    symbol: sym,
                                    mutable: false,
                                },
                            );
                            HirParam {
                                name: sym,
                                ty: self.lower_type(&p.type_annotation, ast, &module_scope),
                                span: self.span_id(p.name.span, file.file_id),
                            }
                        })
                        .collect();
                    let ret = f
                        .return_type
                        .as_ref()
                        .map(|t| self.lower_type(t, ast, &module_scope));
                    let body =
                        self.lower_block(&f.body, module_id, file.file_id, ast, &module_scope);
                    let span = self.span_id(f.span, file.file_id);
                    if let Some(hir_func) = self.items.functions.get_mut(fid.0 as usize) {
                        hir_func.params = params;
                        hir_func.ret = ret;
                        hir_func.body = body;
                        hir_func.span = span;
                    }
                }
            }
        }

        for module in &mut self.modules {
            for ((mid, _name), sym) in &self.symbols.by_name {
                if *mid == module.id {
                    module.exports.push(*sym);
                }
            }
        }
    }

    fn finish(self) -> HirProgram {
        HirProgram {
            modules: self.modules,
            items: self.items,
            symbols: self.symbols,
            interner: self.interner,
            main: self.main,
            files: self.files,
            spans: self.spans,
        }
    }

    fn find_top_level_symbol(&self, name: &str, module_id: ModuleId) -> ResSymbolId {
        self.module_scopes
            .get(&module_id)
            .and_then(|scope| scope.get(name).copied())
            .expect("missing top-level symbol")
    }

    fn insert_symbol(
        &mut self,
        module: ModuleId,
        name: InternSymbol,
        kind: SymbolKind,
    ) -> SymbolId {
        let id = SymbolId(self.symbols.entries.len() as u32);
        match kind {
            SymbolKind::Function(_)
            | SymbolKind::Struct(_)
            | SymbolKind::Global(_)
            | SymbolKind::Module => {
                self.symbols.by_name.insert((module, name), id);
            }
            _ => {}
        }
        self.symbols.entries.push(Symbol {
            id,
            module,
            name,
            kind,
        });
        id
    }

    fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> HirStmt {
        match stmt {
            Stmt::Let {
                name,
                mutable,
                type_annotation,
                value,
            } => {
                let value_hir = self.lower_expr(value, module, file_id, ast, module_scope);
                let sym = self.insert_symbol(module, name.sym, SymbolKind::Local);
                self.local_scope.insert(
                    ast.resolve(name.sym).to_string(),
                    LocalBinding {
                        symbol: sym,
                        mutable: *mutable,
                    },
                );
                HirStmt::Let {
                    name: sym,
                    mutable: *mutable,
                    ty: self.lower_type(
                        type_annotation.as_ref().unwrap_or(&Type::Undetermined),
                        ast,
                        module_scope,
                    ),
                    value: value_hir,
                    span: self.span_id(name.span, file_id),
                }
            }
            Stmt::Assign { target, value } => {
                let target_name = ast.resolve(target.sym);
                let binding = self.local_scope.get(target_name).copied();
                match binding {
                    Some(binding) => {
                        if !binding.mutable {
                            self.errors.push(LoweringError::AssignToImmutable {
                                name: target_name.to_string(),
                                file: file_id,
                                span: target.span,
                            });
                        }
                        HirStmt::Assign {
                            target: binding.symbol,
                            value: self.lower_expr(value, module, file_id, ast, module_scope),
                            span: self.span_id(target.span, file_id),
                        }
                    }
                    None => {
                        self.errors.push(LoweringError::UnknownName {
                            name: target_name.to_string(),
                            file: file_id,
                            span: target.span,
                        });
                        let span = self.span_id(target.span, file_id);
                        HirStmt::Expr(HirExpr::Error { span })
                    }
                }
            }
            Stmt::Expr(expr) => {
                HirStmt::Expr(self.lower_expr(expr, module, file_id, ast, module_scope))
            }
            Stmt::Loop { body, span } => HirStmt::Loop {
                body: self.lower_stmt_list(body, module, file_id, ast, module_scope),
                span: self.span_id(*span, file_id),
            },
            Stmt::While {
                condition,
                body,
                span,
            } => HirStmt::While {
                condition: self.lower_expr(condition, module, file_id, ast, module_scope),
                body: self.lower_stmt_list(body, module, file_id, ast, module_scope),
                span: self.span_id(*span, file_id),
            },
            Stmt::Break { span } => HirStmt::Break {
                span: self.span_id(*span, file_id),
            },
        }
    }

    fn lower_stmt_list(
        &mut self,
        stmts: &[Stmt],
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> HirBlock {
        self.local_scope.push();
        let hir_stmts = stmts
            .iter()
            .map(|s| self.lower_stmt(s, module, file_id, ast, module_scope))
            .collect();
        self.local_scope.pop();
        HirBlock {
            stmts: hir_stmts,
            expr: None,
        }
    }

    fn lower_block(
        &mut self,
        block: &prim_parse::Block,
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> HirBlock {
        self.local_scope.push();
        let stmts = block
            .stmts
            .iter()
            .map(|s| self.lower_stmt(s, module, file_id, ast, module_scope))
            .collect();
        let expr = block
            .expr
            .as_ref()
            .map(|e| Box::new(self.lower_expr(e, module, file_id, ast, module_scope)));
        self.local_scope.pop();
        HirBlock { stmts, expr }
    }

    fn lower_expr(
        &mut self,
        expr: &Expr,
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> HirExpr {
        let span = self.span_id(expr.span, file_id);
        match &expr.kind {
            ExprKind::Int(value) => HirExpr::Int {
                value: *value,
                ty: self.lower_int_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::Float(value) => HirExpr::Float {
                value: *value,
                ty: self.lower_float_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::Bool(value) => HirExpr::Bool {
                value: *value,
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::String(value) => HirExpr::Str {
                value: value.clone(),
                ty: self
                    .stdlib_str_struct
                    .map(crate::hir::Type::Struct)
                    .unwrap_or(crate::hir::Type::Undetermined),
                span,
            },
            ExprKind::Ident(ident) => {
                let name_str = ast.resolve(ident.sym);
                match self.resolve_name(name_str, module, file_id, ident.span, module_scope) {
                    Some(sym) => HirExpr::Ident {
                        symbol: sym,
                        ty: self.lower_type(&expr.ty, ast, module_scope),
                        span,
                    },
                    None => HirExpr::Error { span },
                }
            }
            ExprKind::Binary { left, op, right } => HirExpr::Binary {
                op: *op,
                left: Box::new(self.lower_expr(left, module, file_id, ast, module_scope)),
                right: Box::new(self.lower_expr(right, module, file_id, ast, module_scope)),
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::FunctionCall { path, args } => {
                let call_span = path.segments.last().expect("empty path").span;
                let fid = self
                    .resolve_function_path(path, file_id, ast, module_scope)
                    .and_then(|id| self.func_ids.get(&id).copied());
                match fid {
                    Some(fid) => HirExpr::Call {
                        func: fid,
                        args: args
                            .iter()
                            .map(|a| self.lower_expr(a, module, file_id, ast, module_scope))
                            .collect(),
                        ty: self.lower_type(&expr.ty, ast, module_scope),
                        span: self.span_id(call_span, file_id),
                    },
                    None => HirExpr::Error { span },
                }
            }
            ExprKind::StructLiteral { name, fields } => {
                let struct_name = ast.resolve(name.sym);
                let struct_id = module_scope
                    .get(struct_name)
                    .and_then(|res_id| self.struct_ids.get(res_id).copied());
                match struct_id {
                    Some(struct_id) => HirExpr::StructLit {
                        struct_id,
                        fields: fields
                            .iter()
                            .map(|f| {
                                (
                                    f.name.sym,
                                    self.lower_expr(&f.value, module, file_id, ast, module_scope),
                                )
                            })
                            .collect(),
                        ty: self.lower_type(&expr.ty, ast, module_scope),
                        span,
                    },
                    None => {
                        self.errors.push(LoweringError::UnknownStruct {
                            name: struct_name.to_string(),
                            file: file_id,
                            span: name.span,
                        });
                        HirExpr::Error { span }
                    }
                }
            }
            ExprKind::FieldAccess { object, field } => HirExpr::Field {
                base: Box::new(self.lower_expr(object, module, file_id, ast, module_scope)),
                field: field.sym,
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::Dereference(operand) => HirExpr::Deref {
                base: Box::new(self.lower_expr(operand, module, file_id, ast, module_scope)),
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::Array(elements) => HirExpr::ArrayLit {
                elements: elements
                    .iter()
                    .map(|e| self.lower_expr(e, module, file_id, ast, module_scope))
                    .collect(),
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => HirExpr::If {
                condition: Box::new(self.lower_expr(condition, module, file_id, ast, module_scope)),
                then_branch: self.lower_block(then_branch, module, file_id, ast, module_scope),
                else_branch: else_branch
                    .as_ref()
                    .map(|b| self.lower_block(b, module, file_id, ast, module_scope)),
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
            ExprKind::Block(block) => HirExpr::Block {
                block: self.lower_block(block, module, file_id, ast, module_scope),
                ty: self.lower_type(&expr.ty, ast, module_scope),
                span,
            },
        }
    }

    fn resolve_name(
        &mut self,
        name: &str,
        module: ModuleId,
        file: FileId,
        span: Span,
        module_scope: &ModuleScope,
    ) -> Option<SymbolId> {
        // Check local scope first
        if let Some(binding) = self.local_scope.get(name) {
            return Some(binding.symbol);
        }
        // Then check module scope
        if let Some(&res_id) = module_scope.get(name) {
            return Some(self.ensure_symbol(res_id, Some(module)));
        }
        self.errors.push(LoweringError::UnknownName {
            name: name.to_string(),
            file,
            span,
        });
        None
    }

    fn resolve_function_path(
        &mut self,
        path: &prim_parse::NamePath,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> Option<ResSymbolId> {
        let name_ident = path.segments.last().expect("empty path");
        let name = ast.resolve(name_ident.sym);

        if path.segments.len() == 1 {
            // Simple name - look up in module scope
            if let Some(&id) = module_scope.get(name) {
                Some(id)
            } else {
                self.errors.push(LoweringError::UnknownFunction {
                    name: name.to_string(),
                    file: file_id,
                    span: name_ident.span,
                });
                None
            }
        } else {
            // Qualified path - look up in target module
            let module_path: Vec<String> = path
                .segments
                .iter()
                .take(path.segments.len() - 1)
                .map(|ident| ast.resolve(ident.sym).to_string())
                .collect();
            let key = crate::program::ModuleKey::Name(module_path.clone());
            let first_seg = &path.segments[0];
            let Some(target_module_id) = self.program.module_index.get(&key) else {
                self.errors.push(LoweringError::UnknownModule {
                    path: module_path.join("::"),
                    file: file_id,
                    span: first_seg.span,
                });
                return None;
            };
            let target_scope = self
                .module_scopes
                .get(target_module_id)
                .expect("missing scope for known module");
            if let Some(&id) = target_scope.get(name) {
                Some(id)
            } else {
                self.errors.push(LoweringError::UnknownFunction {
                    name: name.to_string(),
                    file: file_id,
                    span: name_ident.span,
                });
                None
            }
        }
    }

    fn lower_type(
        &self,
        ty: &Type,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> crate::hir::Type {
        match ty {
            Type::Struct(name) => {
                let name_str = ast.resolve(*name);
                // The resolver validates all type names; this expect indicates a resolver bug.
                let res_id = *module_scope.get(name_str).unwrap_or_else(|| {
                    panic!("resolver should have caught unknown type '{name_str}'")
                });
                let sid = *self
                    .struct_ids
                    .get(&res_id)
                    .unwrap_or_else(|| panic!("missing struct id for resolved type '{name_str}'"));
                crate::hir::Type::Struct(sid)
            }
            Type::Array(inner) => {
                crate::hir::Type::Array(Box::new(self.lower_type(inner, ast, module_scope)))
            }
            Type::Pointer {
                mutability,
                pointee,
            } => crate::hir::Type::Pointer {
                mutable: *mutability == prim_parse::PointerMutability::Mutable,
                pointee: Box::new(self.lower_type(pointee, ast, module_scope)),
            },
            Type::Undetermined => crate::hir::Type::Undetermined,
            Type::U8 => crate::hir::Type::U8,
            Type::I8 => crate::hir::Type::I8,
            Type::U16 => crate::hir::Type::U16,
            Type::I16 => crate::hir::Type::I16,
            Type::U32 => crate::hir::Type::U32,
            Type::I32 => crate::hir::Type::I32,
            Type::U64 => crate::hir::Type::U64,
            Type::I64 => crate::hir::Type::I64,
            Type::Usize => crate::hir::Type::Usize,
            Type::Isize => crate::hir::Type::Isize,
            Type::F32 => crate::hir::Type::F32,
            Type::F64 => crate::hir::Type::F64,
            Type::Bool => crate::hir::Type::Bool,
        }
    }

    fn lower_int_type(
        &self,
        ty: &Type,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> crate::hir::Type {
        match ty {
            Type::Undetermined => crate::hir::Type::IntVar,
            _ => self.lower_type(ty, ast, module_scope),
        }
    }

    fn lower_float_type(
        &self,
        ty: &Type,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> crate::hir::Type {
        match ty {
            Type::Undetermined => crate::hir::Type::FloatVar,
            _ => self.lower_type(ty, ast, module_scope),
        }
    }

    fn span_id(&mut self, span: Span, file: FileId) -> SpanId {
        let id = SpanId(self.spans.len() as u32);
        self.spans.push((file, span));
        id
    }

    fn ensure_symbol(&mut self, res_id: ResSymbolId, module_hint: Option<ModuleId>) -> SymbolId {
        if let Some(sym) = self.symbol_map.get(&res_id) {
            return *sym;
        }
        let info = &self.symbols_info[res_id.0 as usize];
        let module = info
            .module
            .or(module_hint)
            .expect("missing module for symbol");
        let kind = self.convert_kind(info.kind, res_id);
        let name_sym = self.interner.get_or_intern(&info.name);
        let sym = self.insert_symbol(module, name_sym, kind);
        self.symbol_map.insert(res_id, sym);
        sym
    }

    fn convert_kind(&mut self, kind: ResSymbolKind, res_id: ResSymbolId) -> SymbolKind {
        match kind {
            ResSymbolKind::Function => {
                let fid = *self
                    .func_ids
                    .entry(res_id)
                    .or_insert_with(|| FuncId(self.items.functions.len() as u32));
                SymbolKind::Function(fid)
            }
            ResSymbolKind::Struct => {
                let sid = *self
                    .struct_ids
                    .entry(res_id)
                    .or_insert_with(|| StructId(self.items.structs.len() as u32));
                SymbolKind::Struct(sid)
            }
            ResSymbolKind::Trait => SymbolKind::Trait,
            ResSymbolKind::Impl => SymbolKind::Unknown,
            ResSymbolKind::Module => SymbolKind::Module,
        }
    }
}
