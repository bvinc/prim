use crate::program::Program;
use prim_hir::*;
use prim_parse::{BinaryOp as AstBinaryOp, Expr, Span, Stmt, Type};
use std::collections::HashMap;
use std::path::PathBuf;

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
    file_ids: HashMap<PathBuf, FileId>,
    symbols: SymbolTable,
    items: Items,
    modules: Vec<Module>,
    struct_ids: HashMap<(ModuleId, String), StructId>,
    func_ids: HashMap<(ModuleId, String), FuncId>,
}

impl<'a> LoweringContext<'a> {
    fn new(program: &'a Program) -> Self {
        let mut files = Vec::new();
        let mut file_ids = HashMap::new();

        for module in &program.modules {
            for file in &module.files {
                let id = FileId(files.len() as u32);
                file_ids.insert(file.path.clone(), id);
                files.push(FileInfo {
                    id,
                    path: file.path.clone(),
                    source: file.source.to_string(),
                });
            }
        }

        Self {
            program,
            spans: Vec::new(),
            files,
            file_ids,
            symbols: SymbolTable {
                entries: Vec::new(),
                by_name: HashMap::new(),
            },
            items: Items::default(),
            modules: Vec::new(),
            struct_ids: HashMap::new(),
            func_ids: HashMap::new(),
        }
    }

    fn declare_modules_and_items(&mut self) {
        for (idx, module) in self.program.modules.iter().enumerate() {
            let module_id = ModuleId(idx as u32);
            let files = module
                .files
                .iter()
                .filter_map(|f| self.file_ids.get(&f.path).copied())
                .collect();
            self.modules.push(Module {
                id: module_id,
                name: module.name.clone(),
                files,
                exports: Vec::new(),
                imports: Vec::new(),
            });

            for file in &module.files {
                let source = file.source.as_ref();
                for s in &file.ast.structs {
                    let name = s.name.text(source).to_string();
                    let sid = StructId(self.items.structs.len() as u32);
                    self.struct_ids.insert((module_id, name.clone()), sid);
                    let sym = self.insert_symbol(module_id, name, SymbolKind::Struct(sid));
                    let span = self.span_id(s.span);
                    self.items.structs.push(HirStruct {
                        id: sid,
                        name: sym,
                        module: module_id,
                        fields: Vec::new(),
                        span,
                    });
                }
                for f in &file.ast.functions {
                    let name = f.name.text(source).to_string();
                    let fid = FuncId(self.items.functions.len() as u32);
                    self.func_ids.insert((module_id, name.clone()), fid);
                    let sym = self.insert_symbol(module_id, name, SymbolKind::Function(fid));
                    let span = self.span_id(f.span);
                    self.items.functions.push(HirFunction {
                        id: fid,
                        name: sym,
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
        for (midx, module) in self.program.modules.iter().enumerate() {
            let module_id = ModuleId(midx as u32);
            for file in &module.files {
                let source = file.source.as_ref();
                for s in &file.ast.structs {
                    let sid = self.struct_ids[&(module_id, s.name.text(source).to_string())];
                    let fields = s
                        .fields
                        .iter()
                        .map(|f| HirField {
                            name: SymbolId::dummy(),
                            ty: self.lower_type(&f.field_type, source, module_id),
                            span: self.span_id(f.name),
                        })
                        .collect();
                    let hir_struct = &mut self.items.structs[sid.0 as usize];
                    hir_struct.fields = fields;
                }

                for f in &file.ast.functions {
                    let fid = self.func_ids[&(module_id, f.name.text(source).to_string())];
                    let params = f
                        .parameters
                        .iter()
                        .map(|p| HirParam {
                            name: SymbolId::dummy(),
                            ty: self.lower_type(&p.type_annotation, source, module_id),
                            span: self.span_id(p.name),
                        })
                        .collect();
                    let ret = f
                        .return_type
                        .as_ref()
                        .map(|t| self.lower_type(t, source, module_id));
                    let body = HirBlock {
                        stmts: f
                            .body
                            .iter()
                            .map(|s| self.lower_stmt(s, source, module_id))
                            .collect(),
                    };
                    let hir_func = &mut self.items.functions[fid.0 as usize];
                    hir_func.params = params;
                    hir_func.ret = ret;
                    hir_func.body = body;
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
        self.symbols.by_name.insert((module, name.clone()), id);
        self.symbols.entries.push(Symbol {
            id,
            module,
            name,
            kind,
        });
        id
    }

    fn lower_stmt(&mut self, stmt: &Stmt, source: &str, module: ModuleId) -> HirStmt {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => HirStmt::Let {
                name: SymbolId::dummy(),
                ty: self.lower_type(
                    type_annotation.as_ref().unwrap_or(&Type::Undetermined),
                    source,
                    module,
                ),
                value: self.lower_expr(value, source, module),
                span: self.span_id(*name),
            },
            Stmt::Expr(expr) => HirStmt::Expr(self.lower_expr(expr, source, module)),
            Stmt::Loop { body, span } => HirStmt::Loop {
                body: HirBlock {
                    stmts: body
                        .iter()
                        .map(|s| self.lower_stmt(s, source, module))
                        .collect(),
                },
                span: self.span_id(*span),
            },
            Stmt::Break { span } => HirStmt::Break {
                span: self.span_id(*span),
            },
        }
    }

    fn lower_expr(&mut self, expr: &Expr, source: &str, module: ModuleId) -> HirExpr {
        match expr {
            Expr::IntLiteral { span, ty, value } => HirExpr::Int {
                value: *value,
                ty: self.lower_type(ty, source, module),
                span: self.span_id(*span),
            },
            Expr::FloatLiteral { span, ty } => {
                let parsed = span
                    .checked_text(source)
                    .and_then(|s| s.parse::<f64>().ok())
                    .unwrap_or_default();
                HirExpr::Float {
                    value: parsed,
                    ty: self.lower_type(ty, source, module),
                    span: self.span_id(*span),
                }
            }
            Expr::BoolLiteral { value, ty } => HirExpr::Bool {
                value: *value,
                ty: self.lower_type(ty, source, module),
                span: self.dummy_span(),
            },
            Expr::StringLiteral { span, ty } => HirExpr::Str {
                value: span.checked_text(source).unwrap_or_default().to_string(),
                ty: self.lower_type(ty, source, module),
                span: self.span_id(*span),
            },
            Expr::Identifier { span, ty } => HirExpr::Ident {
                symbol: SymbolId::dummy(),
                ty: self.lower_type(ty, source, module),
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
                left: Box::new(self.lower_expr(left, source, module)),
                right: Box::new(self.lower_expr(right, source, module)),
                ty: self.lower_type(ty, source, module),
                span: self.dummy_span(),
            },
            Expr::FunctionCall { path, args, ty } => {
                let name = path
                    .segments
                    .last()
                    .and_then(|s| s.checked_text(source))
                    .unwrap_or("");
                let fid = self
                    .func_ids
                    .get(&(module, name.to_string()))
                    .copied()
                    .unwrap_or(FuncId(u32::MAX));
                HirExpr::Call {
                    func: fid,
                    args: args
                        .iter()
                        .map(|a| self.lower_expr(a, source, module))
                        .collect(),
                    ty: self.lower_type(ty, source, module),
                    span: self.dummy_span(),
                }
            }
            Expr::StructLiteral { name, fields, ty } => {
                let struct_id = self
                    .struct_ids
                    .get(&(
                        module,
                        name.checked_text(source).unwrap_or_default().to_string(),
                    ))
                    .copied()
                    .unwrap_or(StructId(u32::MAX));
                HirExpr::StructLit {
                    struct_id,
                    fields: fields
                        .iter()
                        .map(|f| (SymbolId::dummy(), self.lower_expr(&f.value, source, module)))
                        .collect(),
                    ty: self.lower_type(ty, source, module),
                    span: self.span_id(*name),
                }
            }
            Expr::FieldAccess { object, field, ty } => HirExpr::Field {
                base: Box::new(self.lower_expr(object, source, module)),
                field: SymbolId::dummy(),
                ty: self.lower_type(ty, source, module),
                span: self.span_id(*field),
            },
            Expr::Dereference { operand, ty } => HirExpr::Deref {
                base: Box::new(self.lower_expr(operand, source, module)),
                ty: self.lower_type(ty, source, module),
                span: self.dummy_span(),
            },
            Expr::ArrayLiteral { elements, ty } => HirExpr::ArrayLit {
                elements: elements
                    .iter()
                    .map(|e| self.lower_expr(e, source, module))
                    .collect(),
                ty: self.lower_type(ty, source, module),
                span: self.dummy_span(),
            },
        }
    }

    fn lower_type(&self, ty: &Type, source: &str, module: ModuleId) -> prim_hir::Type {
        match ty {
            Type::Struct(span) => {
                let name = span.checked_text(source).unwrap_or_default().to_string();
                let sid = self
                    .struct_ids
                    .get(&(module, name))
                    .copied()
                    .unwrap_or(StructId(u32::MAX));
                prim_hir::Type::Struct(sid)
            }
            Type::Array(inner) => {
                prim_hir::Type::Array(Box::new(self.lower_type(inner, source, module)))
            }
            Type::Pointer {
                mutability,
                pointee,
            } => prim_hir::Type::Pointer {
                mutable: *mutability == prim_parse::PointerMutability::Mutable,
                pointee: Box::new(self.lower_type(pointee, source, module)),
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
}

trait DummySymbol {
    fn dummy() -> Self;
}

impl DummySymbol for SymbolId {
    fn dummy() -> Self {
        SymbolId(u32::MAX)
    }
}
