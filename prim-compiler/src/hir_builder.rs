use crate::program::{
    FileId as ProgFileId, NameRef, Program, SymbolId as ResSymbolId, SymbolInfo,
    SymbolKind as ResSymbolKind,
};
use prim_hir::*;
use prim_parse::{BinaryOp as AstBinaryOp, Expr, Span, Stmt, Type};
use std::collections::HashMap;

/// Lower a loaded and type-checked [`Program`] into [`HirProgram`].
pub fn lower_to_hir(program: &Program) -> HirProgram {
    let mut ctx = LoweringContext::new(program);
    ctx.declare_modules_and_items();
    ctx.populate_items();
    ctx.finish()
}

struct LoweringContext<'a> {
    program: &'a Program,
    spans: Vec<Span>,
    files: Vec<FileInfo>,
    symbols: SymbolTable,
    items: Items,
    modules: Vec<Module>,
    struct_ids: HashMap<ResSymbolId, StructId>,
    func_ids: HashMap<ResSymbolId, FuncId>,
    symbol_map: HashMap<ResSymbolId, SymbolId>,
    def_lookup: HashMap<(ProgFileId, Span), ResSymbolId>,
    uses: &'a HashMap<NameRef, ResSymbolId>,
    symbols_info: &'a [SymbolInfo],
}

impl<'a> LoweringContext<'a> {
    fn new(program: &'a Program) -> Self {
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
                let id = FileId(file.file_id.0);
                let slot = &mut files[id.0 as usize];
                *slot = Some(FileInfo {
                    id,
                    path: file.path.clone(),
                    source: file.source.to_string(),
                });
            }
        }
        let files: Vec<FileInfo> = files.into_iter().flatten().collect();

        let def_lookup = program
            .name_resolution
            .symbols
            .iter()
            .map(|info| ((info.file, info.span), info.id))
            .collect();

        Self {
            program,
            spans: Vec::new(),
            files,
            symbols: SymbolTable {
                entries: Vec::new(),
                by_name: HashMap::new(),
            },
            items: Items::default(),
            modules: Vec::new(),
            struct_ids: HashMap::new(),
            func_ids: HashMap::new(),
            symbol_map: HashMap::new(),
            def_lookup,
            uses: &program.name_resolution.uses,
            symbols_info: &program.name_resolution.symbols,
        }
    }

    fn declare_modules_and_items(&mut self) {
        for module in &self.program.modules {
            let module_id = ModuleId(module.id.0);
            let files = module.files.iter().map(|f| FileId(f.file_id.0)).collect();
            self.modules.push(Module {
                id: module_id,
                name: module.name.clone(),
                files,
                exports: Vec::new(),
                imports: Vec::new(),
            });

            for file in &module.files {
                for s in &file.ast.structs {
                    let res_id = self.def_lookup.get(&(file.file_id, s.span)).copied();
                    let sym_id = res_id
                        .and_then(|sid| self.ensure_symbol(sid, Some(module_id)))
                        .unwrap_or_else(SymbolId::dummy);
                    let sid = if let Some(rid) = res_id {
                        *self
                            .struct_ids
                            .entry(rid)
                            .or_insert_with(|| StructId(self.items.structs.len() as u32))
                    } else {
                        StructId(self.items.structs.len() as u32)
                    };
                    let span = self.span_id(s.span);
                    self.items.structs.push(HirStruct {
                        id: sid,
                        name: sym_id,
                        module: module_id,
                        fields: Vec::new(),
                        span,
                    });
                }
                for f in &file.ast.functions {
                    let res_id = self.def_lookup.get(&(file.file_id, f.span)).copied();
                    let sym_id = res_id
                        .and_then(|sid| self.ensure_symbol(sid, Some(module_id)))
                        .unwrap_or_else(SymbolId::dummy);
                    let fid = if let Some(rid) = res_id {
                        *self
                            .func_ids
                            .entry(rid)
                            .or_insert_with(|| FuncId(self.items.functions.len() as u32))
                    } else {
                        FuncId(self.items.functions.len() as u32)
                    };
                    let span = self.span_id(f.span);
                    self.items.functions.push(HirFunction {
                        id: fid,
                        name: sym_id,
                        module: module_id,
                        params: Vec::new(),
                        ret: None,
                        body: HirBlock { stmts: Vec::new() },
                        span,
                        runtime_binding: f.runtime_binding.clone(),
                    });
                }
            }
        }
    }

    fn populate_items(&mut self) {
        for module in &self.program.modules {
            let module_id = ModuleId(module.id.0);
            for file in &module.files {
                let source = file.source.as_ref();
                for s in &file.ast.structs {
                    let res_id = self.def_lookup.get(&(file.file_id, s.span)).copied();
                    let sid = res_id
                        .and_then(|rid| self.struct_ids.get(&rid).copied())
                        .unwrap_or(StructId(u32::MAX));
                    let fields = s
                        .fields
                        .iter()
                        .map(|f| {
                            let sym_id = if let Some(rid) =
                                self.def_lookup.get(&(file.file_id, f.name)).copied()
                            {
                                self.ensure_symbol(rid, Some(module_id))
                                    .unwrap_or_else(SymbolId::dummy)
                            } else {
                                SymbolId::dummy()
                            };
                            HirField {
                                name: sym_id,
                                ty: self.lower_type(&f.field_type, file.file_id, source),
                                span: self.span_id(f.name),
                            }
                        })
                        .collect();
                    if let Some(hir_struct) = self.items.structs.get_mut(sid.0 as usize) {
                        hir_struct.fields = fields;
                    }
                }

                for f in &file.ast.functions {
                    let res_id = self.def_lookup.get(&(file.file_id, f.span)).copied();
                    let fid = res_id
                        .and_then(|rid| self.func_ids.get(&rid).copied())
                        .unwrap_or(FuncId(u32::MAX));
                    let params = f
                        .parameters
                        .iter()
                        .map(|p| HirParam {
                            name: if let Some(rid) =
                                self.def_lookup.get(&(file.file_id, p.name)).copied()
                            {
                                self.ensure_symbol(rid, Some(module_id))
                                    .unwrap_or_else(SymbolId::dummy)
                            } else {
                                SymbolId::dummy()
                            },
                            ty: self.lower_type(&p.type_annotation, file.file_id, source),
                            span: self.span_id(p.name),
                        })
                        .collect();
                    let ret = f
                        .return_type
                        .as_ref()
                        .map(|t| self.lower_type(t, file.file_id, source));
                    let body = HirBlock {
                        stmts: f
                            .body
                            .iter()
                            .map(|s| self.lower_stmt(s, source, module_id, file.file_id))
                            .collect(),
                    };
                    if let Some(hir_func) = self.items.functions.get_mut(fid.0 as usize) {
                        hir_func.params = params;
                        hir_func.ret = ret;
                        hir_func.body = body;
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
            files: self.files,
            spans: self.spans,
        }
    }

    fn insert_symbol(&mut self, module: ModuleId, name: String, kind: SymbolKind) -> SymbolId {
        let id = SymbolId(self.symbols.entries.len() as u32);
        match kind {
            SymbolKind::Function(_)
            | SymbolKind::Struct(_)
            | SymbolKind::Global(_)
            | SymbolKind::Module => {
                self.symbols.by_name.insert((module, name.clone()), id);
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
        source: &str,
        module: ModuleId,
        file_id: ProgFileId,
    ) -> HirStmt {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => HirStmt::Let {
                name: if let Some(rid) = self.def_lookup.get(&(file_id, *name)).copied() {
                    self.ensure_symbol(rid, Some(module))
                        .unwrap_or_else(SymbolId::dummy)
                } else {
                    SymbolId::dummy()
                },
                ty: self.lower_type(
                    type_annotation.as_ref().unwrap_or(&Type::Undetermined),
                    file_id,
                    source,
                ),
                value: self.lower_expr(value, source, module, file_id),
                span: self.span_id(*name),
            },
            Stmt::Expr(expr) => HirStmt::Expr(self.lower_expr(expr, source, module, file_id)),
            Stmt::Loop { body, span } => HirStmt::Loop {
                body: HirBlock {
                    stmts: body
                        .iter()
                        .map(|s| self.lower_stmt(s, source, module, file_id))
                        .collect(),
                },
                span: self.span_id(*span),
            },
            Stmt::Break { span } => HirStmt::Break {
                span: self.span_id(*span),
            },
        }
    }

    fn lower_expr(
        &mut self,
        expr: &Expr,
        source: &str,
        module: ModuleId,
        file_id: ProgFileId,
    ) -> HirExpr {
        match expr {
            Expr::IntLiteral { span, ty, value } => HirExpr::Int {
                value: *value,
                ty: self.lower_type(ty, file_id, source),
                span: self.span_id(*span),
            },
            Expr::FloatLiteral { span, ty } => {
                let parsed = span
                    .checked_text(source)
                    .and_then(|s| s.parse::<f64>().ok())
                    .unwrap_or_default();
                HirExpr::Float {
                    value: parsed,
                    ty: self.lower_type(ty, file_id, source),
                    span: self.span_id(*span),
                }
            }
            Expr::BoolLiteral { value, ty } => HirExpr::Bool {
                value: *value,
                ty: self.lower_type(ty, file_id, source),
                span: self.dummy_span(),
            },
            Expr::StringLiteral { span, ty } => HirExpr::Str {
                value: span.checked_text(source).unwrap_or_default().to_string(),
                ty: self.lower_type(ty, file_id, source),
                span: self.span_id(*span),
            },
            Expr::Identifier { span, ty } => HirExpr::Ident {
                symbol: self
                    .symbol_for_use(file_id, *span, Some(module))
                    .unwrap_or_else(SymbolId::dummy),
                ty: self.lower_type(ty, file_id, source),
                span: self.span_id(*span),
            },
            Expr::Binary {
                left,
                op,
                right,
                ty,
            } => HirExpr::Binary {
                op: match op {
                    AstBinaryOp::Add => BinaryOp::Add,
                    AstBinaryOp::Subtract => BinaryOp::Subtract,
                    AstBinaryOp::Multiply => BinaryOp::Multiply,
                    AstBinaryOp::Divide => BinaryOp::Divide,
                    AstBinaryOp::Equals => BinaryOp::Equals,
                },
                left: Box::new(self.lower_expr(left, source, module, file_id)),
                right: Box::new(self.lower_expr(right, source, module, file_id)),
                ty: self.lower_type(ty, file_id, source),
                span: self.dummy_span(),
            },
            Expr::FunctionCall { path, args, ty } => {
                let call_span = *path.segments.last().unwrap_or(&Span::empty_at(0));
                let res_id = self.res_use(file_id, call_span);
                let fid = res_id
                    .and_then(|rid| self.func_ids.get(&rid).copied())
                    .unwrap_or(FuncId(u32::MAX));
                let _ = res_id.and_then(|rid| self.ensure_symbol(rid, Some(module)));
                HirExpr::Call {
                    func: fid,
                    args: args
                        .iter()
                        .map(|a| self.lower_expr(a, source, module, file_id))
                        .collect(),
                    ty: self.lower_type(ty, file_id, source),
                    span: self.dummy_span(),
                }
            }
            Expr::StructLiteral { name, fields, ty } => {
                let res_id = self.res_use(file_id, *name);
                let struct_id = res_id
                    .and_then(|rid| self.struct_ids.get(&rid).copied())
                    .unwrap_or(StructId(u32::MAX));
                let _ = res_id.and_then(|rid| self.ensure_symbol(rid, Some(module)));
                HirExpr::StructLit {
                    struct_id,
                    fields: fields
                        .iter()
                        .map(|f| {
                            (
                                self.symbol_for_use(file_id, f.name, Some(module))
                                    .unwrap_or_else(SymbolId::dummy),
                                self.lower_expr(&f.value, source, module, file_id),
                            )
                        })
                        .collect(),
                    ty: self.lower_type(ty, file_id, source),
                    span: self.span_id(*name),
                }
            }
            Expr::FieldAccess { object, field, ty } => HirExpr::Field {
                base: Box::new(self.lower_expr(object, source, module, file_id)),
                field: self
                    .symbol_for_use(file_id, *field, Some(module))
                    .unwrap_or_else(SymbolId::dummy),
                ty: self.lower_type(ty, file_id, source),
                span: self.span_id(*field),
            },
            Expr::Dereference { operand, ty } => HirExpr::Deref {
                base: Box::new(self.lower_expr(operand, source, module, file_id)),
                ty: self.lower_type(ty, file_id, source),
                span: self.dummy_span(),
            },
            Expr::ArrayLiteral { elements, ty } => HirExpr::ArrayLit {
                elements: elements
                    .iter()
                    .map(|e| self.lower_expr(e, source, module, file_id))
                    .collect(),
                ty: self.lower_type(ty, file_id, source),
                span: self.dummy_span(),
            },
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn lower_type(&self, ty: &Type, file_id: ProgFileId, source: &str) -> prim_hir::Type {
        match ty {
            Type::Struct(span) => {
                let sid = self
                    .res_use(file_id, *span)
                    .and_then(|rid| self.struct_ids.get(&rid).copied())
                    .unwrap_or(StructId(u32::MAX));
                prim_hir::Type::Struct(sid)
            }
            Type::Array(inner) => {
                prim_hir::Type::Array(Box::new(self.lower_type(inner, file_id, source)))
            }
            Type::Pointer {
                mutability,
                pointee,
            } => prim_hir::Type::Pointer {
                mutable: *mutability == prim_parse::PointerMutability::Mutable,
                pointee: Box::new(self.lower_type(pointee, file_id, source)),
            },
            Type::Undetermined => prim_hir::Type::Undetermined,
            Type::U8 => prim_hir::Type::U8,
            Type::I8 => prim_hir::Type::I8,
            Type::U16 => prim_hir::Type::U16,
            Type::I16 => prim_hir::Type::I16,
            Type::U32 => prim_hir::Type::U32,
            Type::I32 => prim_hir::Type::I32,
            Type::U64 => prim_hir::Type::U64,
            Type::I64 => prim_hir::Type::I64,
            Type::Usize => prim_hir::Type::Usize,
            Type::Isize => prim_hir::Type::Isize,
            Type::F32 => prim_hir::Type::F32,
            Type::F64 => prim_hir::Type::F64,
            Type::Bool => prim_hir::Type::Bool,
            Type::StrSlice => prim_hir::Type::StrSlice,
        }
    }

    fn span_id(&mut self, span: Span) -> SpanId {
        let id = SpanId(self.spans.len() as u32);
        self.spans.push(span);
        id
    }

    fn dummy_span(&mut self) -> SpanId {
        self.span_id(Span::empty_at(0))
    }

    fn res_use(&self, file_id: ProgFileId, span: Span) -> Option<ResSymbolId> {
        self.uses
            .get(&NameRef {
                file: file_id,
                span,
            })
            .copied()
    }

    fn symbol_for_use(
        &mut self,
        file_id: ProgFileId,
        span: Span,
        module_hint: Option<ModuleId>,
    ) -> Option<SymbolId> {
        let res_id = self.res_use(file_id, span)?;
        self.ensure_symbol(res_id, module_hint)
    }

    fn ensure_symbol(
        &mut self,
        res_id: ResSymbolId,
        module_hint: Option<ModuleId>,
    ) -> Option<SymbolId> {
        if let Some(sym) = self.symbol_map.get(&res_id) {
            return Some(*sym);
        }
        let info = self.symbols_info.iter().find(|i| i.id == res_id)?;
        let module = info
            .module
            .map(|m| ModuleId(m.0))
            .or(module_hint)
            .unwrap_or(ModuleId(u32::MAX));
        let kind = self.convert_kind(info.kind, res_id);
        let sym = self.insert_symbol(module, info.name.clone(), kind);
        self.symbol_map.insert(res_id, sym);
        Some(sym)
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
            ResSymbolKind::Global => SymbolKind::Global(GlobalId(u32::MAX)),
            ResSymbolKind::Param => SymbolKind::Param,
            ResSymbolKind::Local => SymbolKind::Local,
            ResSymbolKind::Field => SymbolKind::Field,
            ResSymbolKind::Trait => SymbolKind::Trait,
            ResSymbolKind::Impl => SymbolKind::Impl,
            ResSymbolKind::Module => SymbolKind::Module,
        }
    }
}

trait DummySymbol {
    fn dummy() -> Self;
}

impl DummySymbol for SymbolId {
    fn dummy() -> Self {
        SymbolId(u32::MAX)
    }
}
