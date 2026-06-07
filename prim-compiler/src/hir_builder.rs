use crate::hir::{
    self, Field, FuncId, Function, InternSymbol, Interner, Module, Param, SpanId, Struct, StructId,
    Symbol, SymbolId, SymbolKind,
};
use crate::program::{Program, ResSymbolId, ResSymbolKind};
use crate::resolver::{ModuleScope, ModuleScopes};
use prim_parse::{Expr, ExprKind, Span, Stmt, Type};
use prim_tok::{FileId, ModuleId};
use std::collections::HashMap;
use std::sync::Arc;

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
    NonConstantGlobalInit {
        file: FileId,
        span: Span,
    },
    DuplicateImplMethod {
        name: String,
        file: FileId,
        span: Span,
    },
    UnknownMethod {
        name: String,
        receiver_type: String,
        file: FileId,
        span: Span,
    },
    UnknownRuntimeAbi {
        name: String,
        file: FileId,
        span: Span,
    },
    NotAnEnum {
        name: String,
        file: FileId,
        span: Span,
    },
    InvalidImplTarget {
        name: String,
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
            LoweringError::NonConstantGlobalInit { .. } => {
                write!(
                    f,
                    "Module-level let initializer must be a literal (int, float, or bool)"
                )
            }
            LoweringError::DuplicateImplMethod { name, .. } => {
                write!(f, "Duplicate impl method '{}' for the same type", name)
            }
            LoweringError::UnknownMethod {
                name,
                receiver_type,
                ..
            } => {
                write!(f, "No method '{}' on type {}", name, receiver_type)
            }
            LoweringError::UnknownRuntimeAbi { name, .. } => {
                write!(f, "Unknown runtime ABI '{}'", name)
            }
            LoweringError::NotAnEnum { name, .. } => {
                write!(f, "'{}' is not an enum", name)
            }
            LoweringError::InvalidImplTarget { name, .. } => {
                write!(
                    f,
                    "cannot implement methods for '{}': not a struct or enum",
                    name
                )
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
            | LoweringError::UnknownModule { span, .. }
            | LoweringError::NonConstantGlobalInit { span, .. }
            | LoweringError::DuplicateImplMethod { span, .. }
            | LoweringError::UnknownMethod { span, .. }
            | LoweringError::UnknownRuntimeAbi { span, .. }
            | LoweringError::NotAnEnum { span, .. }
            | LoweringError::InvalidImplTarget { span, .. } => *span,
        }
    }

    pub fn file(&self) -> FileId {
        match self {
            LoweringError::AssignToImmutable { file, .. }
            | LoweringError::UnknownName { file, .. }
            | LoweringError::UnknownFunction { file, .. }
            | LoweringError::UnknownStruct { file, .. }
            | LoweringError::UnknownModule { file, .. }
            | LoweringError::NonConstantGlobalInit { file, .. }
            | LoweringError::DuplicateImplMethod { file, .. }
            | LoweringError::UnknownMethod { file, .. }
            | LoweringError::UnknownRuntimeAbi { file, .. }
            | LoweringError::NotAnEnum { file, .. }
            | LoweringError::InvalidImplTarget { file, .. } => *file,
        }
    }
}

/// Lower a loaded [`Program`] into [`hir::Program`].
///
/// `source_map` is consulted only by `@dbg` lowering (to build the
/// `[path:line:col] expr_text = ` prefix); other call sites don't need it.
pub fn lower_to_hir(
    program: &Program,
    module_scopes: &ModuleScopes,
    source_map: Arc<crate::SourceMap>,
) -> Result<hir::Program, Vec<LoweringError>> {
    let mut ctx = LoweringContext::new(program, module_scopes, source_map);
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
    interner: Arc<Interner>,
    /// File paths + lazily-cached source bytes. Used by `@dbg` lowering to
    /// build the `[path:line:col] expr_text = ` prefix; otherwise unused.
    source_map: Arc<crate::SourceMap>,
    spans: Vec<(FileId, Span)>,
    /// HIR symbols. For every resolver-side `ResSymbolId(k)` (`k < N`),
    /// `symbols[k]` is the corresponding HIR symbol — built eagerly during
    /// `declare_modules_and_items`. Local/param symbols added during body
    /// lowering append at indices `>= N`.
    symbols: Vec<Symbol>,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    globals: Vec<hir::Global>,
    modules: Vec<Module>,
    root_module: ModuleId,
    main: Option<SymbolId>,
    traits: Vec<hir::Trait>,
    enums: Vec<hir::Enum>,
    struct_ids: HashMap<ResSymbolId, StructId>,
    func_ids: HashMap<ResSymbolId, FuncId>,
    global_ids: HashMap<ResSymbolId, hir::GlobalId>,
    trait_ids: HashMap<ResSymbolId, hir::TraitId>,
    enum_ids: HashMap<ResSymbolId, hir::EnumId>,
    /// Type-param scope of the function currently being populated.
    /// Resolved name → `TypeParamId`. Cleared between functions.
    current_type_params: HashMap<String, hir::TypeParamId>,
    /// `(receiver type, method name)` → impl method FuncId. Built when
    /// lowering each `impl ... for Type { fn ... }` block; the receiver is a
    /// struct or an enum.
    impl_methods: HashMap<(hir::MethodOwner, hir::InternSymbol), FuncId>,
    /// `(trait, struct)` → vec of FuncIds in trait method declaration order.
    /// Vtables back dynamic dispatch, which is struct-only for now.
    impls: HashMap<(hir::TraitId, StructId), Vec<FuncId>>,
    stdlib_string_struct: Option<StructId>,
    local_scope: LocalScope,
    errors: Vec<LoweringError>,
}

impl<'a> LoweringContext<'a> {
    fn new(
        program: &'a Program,
        module_scopes: &'a ModuleScopes,
        source_map: Arc<crate::SourceMap>,
    ) -> Self {
        Self {
            program,
            module_scopes,
            interner: program.interner.clone(),
            source_map,
            spans: Vec::new(),
            symbols: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            globals: Vec::new(),
            modules: Vec::new(),
            root_module: program.root,
            main: None,
            traits: Vec::new(),
            enums: Vec::new(),
            struct_ids: HashMap::new(),
            func_ids: HashMap::new(),
            global_ids: HashMap::new(),
            trait_ids: HashMap::new(),
            enum_ids: HashMap::new(),
            current_type_params: HashMap::new(),
            impl_methods: HashMap::new(),
            impls: HashMap::new(),
            stdlib_string_struct: None,
            local_scope: LocalScope::new(),
            errors: Vec::new(),
        }
    }

    fn declare_modules_and_items(&mut self) {
        // Pass 1: walk the program assigning FuncId / StructId to each
        // top-level function and struct, and pushing their shells (with
        // empty fields/body) into self.structs / self.functions. Symbol
        // creation is deferred to pass 2 so it can use the IDs assigned
        // here.
        for module in &self.program.modules {
            let module_id = module.id;
            self.modules.push(Module {
                id: module_id,
                name: module.name.clone(),
            });

            for file in &module.files {
                for s in &file.ast.structs {
                    let name = self.interner.resolve(&s.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let sid = *self
                        .struct_ids
                        .entry(res_id)
                        .or_insert_with(|| StructId(self.structs.len() as u32));
                    if self.stdlib_string_struct.is_none()
                        && module.name.len() == 2
                        && module.name[0] == "std"
                        && module.name[1] == "string"
                        && name == "String"
                    {
                        self.stdlib_string_struct = Some(sid);
                    }
                    let span = self.span_id(s.span, file.file_id);
                    self.structs.push(Struct {
                        id: sid,
                        // SymbolId(res_id.0) — see pass 2 below for why this
                        // identity holds.
                        name: SymbolId(res_id.0),
                        type_params: Vec::new(),
                        fields: Vec::new(),
                        span,
                    });
                }
                for f in &file.ast.functions {
                    let name = self.interner.resolve(&f.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    if self.main.is_none() && module_id == self.root_module && name == "main" {
                        self.main = Some(SymbolId(res_id.0));
                    }
                    let fid = *self
                        .func_ids
                        .entry(res_id)
                        .or_insert_with(|| FuncId(self.functions.len() as u32));
                    let span = self.span_id(f.span, file.file_id);
                    let runtime = f.runtime_binding.as_deref().and_then(|binding| {
                        let runtime = hir::RuntimeAbi::from_symbol(binding);
                        if runtime.is_none() {
                            self.errors.push(LoweringError::UnknownRuntimeAbi {
                                name: binding.to_string(),
                                file: file.file_id,
                                span: f.span,
                            });
                        }
                        runtime
                    });
                    self.functions.push(Function {
                        id: fid,
                        name: SymbolId(res_id.0),
                        type_params: Vec::new(),
                        params: Vec::new(),
                        ret: None,
                        body: hir::Block {
                            stmts: Vec::new(),
                            expr: None,
                        },
                        span,
                        runtime,
                    });
                }
                for g in &file.ast.globals {
                    let name = self.interner.resolve(&g.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let gid = *self
                        .global_ids
                        .entry(res_id)
                        .or_insert_with(|| hir::GlobalId(self.globals.len() as u32));
                    let span = self.span_id(g.span, file.file_id);
                    // Shell: ty and init filled in pass 2 with the rest of the
                    // module's resolved types.
                    self.globals.push(hir::Global {
                        id: gid,
                        name: SymbolId(res_id.0),
                        mutable: g.mutable,
                        ty: hir::Type::Undetermined,
                        init: hir::GlobalInit::I32(0),
                        span,
                    });
                }
                for t in &file.ast.traits {
                    let name = self.interner.resolve(&t.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let tid = *self
                        .trait_ids
                        .entry(res_id)
                        .or_insert_with(|| hir::TraitId(self.traits.len() as u32));
                    let span = self.span_id(t.span, file.file_id);
                    self.traits.push(hir::Trait {
                        id: tid,
                        name: SymbolId(res_id.0),
                        methods: Vec::new(),
                        method_idx: HashMap::new(),
                        span,
                    });
                }
                for e in &file.ast.enums {
                    let name = self.interner.resolve(&e.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let eid = *self
                        .enum_ids
                        .entry(res_id)
                        .or_insert_with(|| hir::EnumId(self.enums.len() as u32));
                    let span = self.span_id(e.span, file.file_id);
                    self.enums.push(hir::Enum {
                        id: eid,
                        name: SymbolId(res_id.0),
                        type_params: Vec::new(),
                        variants: Vec::new(),
                        variant_idx: HashMap::new(),
                        span,
                    });
                }
            }
        }

        // Pass 2: build one hir::Symbol per ResSymbol, in resolver order, so
        // `SymbolId(k) == ResSymbolId(k)` for every top-level symbol. Local
        // and param symbols added later append at indices `>= N`.
        for (idx, info) in self.program.symbols.iter().enumerate() {
            let module = info.module.unwrap_or(self.root_module);
            let kind = self.convert_kind(info.kind, ResSymbolId(idx as u32));
            let name_sym = self.interner.get_or_intern(&info.name);
            self.symbols.push(Symbol {
                id: SymbolId(idx as u32),
                module,
                name: name_sym,
                kind,
            });
        }

        // Pass 3: declare impl methods. Each impl method becomes a regular
        // Function with a synthesized symbol (indices `>= N`). The method's
        // (receiver-struct, method-name) is recorded in `impl_methods` so
        // typecheck can resolve `receiver.method()` calls.
        for module in &self.program.modules {
            let module_id = module.id;
            for file in &module.files {
                for im in &file.ast.impls {
                    let struct_name = self.interner.resolve(&im.struct_name.sym).to_string();
                    let Some(struct_res_id) = self
                        .module_scopes
                        .get(&module_id)
                        .and_then(|scope| scope.get(&struct_name).copied())
                    else {
                        self.errors.push(LoweringError::UnknownName {
                            name: struct_name,
                            file: file.file_id,
                            span: im.struct_name.span,
                        });
                        continue;
                    };
                    let Some(owner) = self.method_owner(struct_res_id) else {
                        self.errors.push(LoweringError::InvalidImplTarget {
                            name: struct_name,
                            file: file.file_id,
                            span: im.struct_name.span,
                        });
                        continue;
                    };
                    for m in &im.methods {
                        let fid = FuncId(self.functions.len() as u32);
                        let symbol_id =
                            self.insert_symbol(module_id, m.name.sym, SymbolKind::Function(fid));
                        let span = self.span_id(m.name.span, file.file_id);
                        self.functions.push(Function {
                            id: fid,
                            name: symbol_id,
                            type_params: Vec::new(),
                            params: Vec::new(),
                            ret: None,
                            body: hir::Block {
                                stmts: Vec::new(),
                                expr: None,
                            },
                            span,
                            runtime: None,
                        });
                        // Record (owner, method-name) → FuncId. Duplicate
                        // impls for the same (owner, name) are a hard error.
                        if self.impl_methods.insert((owner, m.name.sym), fid).is_some() {
                            let method_name = self.interner.resolve(&m.name.sym).to_string();
                            self.errors.push(LoweringError::DuplicateImplMethod {
                                name: method_name,
                                file: file.file_id,
                                span: m.name.span,
                            });
                        }
                    }
                }
            }
        }
    }

    /// The struct or enum a resolved symbol names, if any. Used to key
    /// `impl` blocks by their receiver type.
    fn method_owner(&self, res_id: ResSymbolId) -> Option<hir::MethodOwner> {
        if let Some(&sid) = self.struct_ids.get(&res_id) {
            return Some(hir::MethodOwner::Struct(sid));
        }
        self.enum_ids
            .get(&res_id)
            .map(|&eid| hir::MethodOwner::Enum(eid))
    }

    fn populate_items(&mut self) {
        for module in &self.program.modules {
            let module_id = module.id;
            let empty = ModuleScope::default();
            let module_scope = self.module_scopes.get(&module.id).unwrap_or(&empty);

            for file in &module.files {
                let ast = &file.ast;
                for s in &ast.structs {
                    let name = self.interner.resolve(&s.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let sid = *self.struct_ids.get(&res_id).expect("missing struct id");
                    // The struct's type params shadow module-scope names
                    // for the duration of field-type lowering, the same
                    // way function type params do.
                    let type_params = self.lower_type_params(
                        &s.type_params,
                        module_id,
                        file.file_id,
                        module_scope,
                    );
                    let fields = s
                        .fields
                        .iter()
                        .map(|f| Field {
                            name: f.name.sym,
                            ty: self.lower_type(&f.field_type, module_scope),
                            span: self.span_id(f.name.span, file.file_id),
                        })
                        .collect();
                    if let Some(hir_struct) = self.structs.get_mut(sid.0 as usize) {
                        hir_struct.type_params = type_params;
                        hir_struct.fields = fields;
                    }
                    self.current_type_params.clear();
                }

                for e in &ast.enums {
                    let name = self.interner.resolve(&e.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let eid = *self.enum_ids.get(&res_id).expect("missing enum id");
                    let type_params = self.lower_type_params(
                        &e.type_params,
                        module_id,
                        file.file_id,
                        module_scope,
                    );
                    let mut variants = Vec::with_capacity(e.variants.len());
                    let mut variant_idx = HashMap::with_capacity(e.variants.len());
                    for (i, v) in e.variants.iter().enumerate() {
                        variant_idx.insert(v.name.sym, i as u32);
                        let fields: Vec<hir::Field> = v
                            .fields
                            .iter()
                            .map(|f| hir::Field {
                                name: f.name.sym,
                                ty: self.lower_type(&f.field_type, module_scope),
                                span: self.span_id(f.name.span, file.file_id),
                            })
                            .collect();
                        variants.push(hir::Variant {
                            name: v.name.sym,
                            fields,
                            span: self.span_id(v.span, file.file_id),
                        });
                    }
                    if let Some(hir_enum) = self.enums.get_mut(eid.0 as usize) {
                        hir_enum.type_params = type_params;
                        hir_enum.variants = variants;
                        hir_enum.variant_idx = variant_idx;
                    }
                    self.current_type_params.clear();
                }

                for f in &ast.functions {
                    let name = self.interner.resolve(&f.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let fid = *self.func_ids.get(&res_id).expect("missing function id");

                    self.local_scope.clear();
                    // Set up the type-param scope so `lower_type` can map
                    // references like `T` to `Type::Param`. Bounds resolve
                    // against the module's trait names.
                    let type_params = self.lower_type_params(
                        &f.type_params,
                        module_id,
                        file.file_id,
                        module_scope,
                    );
                    let params = f
                        .parameters
                        .iter()
                        .map(|p| {
                            let sym = self.insert_symbol(module_id, p.name.sym, SymbolKind::Param);
                            self.local_scope.insert(
                                self.interner.resolve(&p.name.sym).to_string(),
                                LocalBinding {
                                    symbol: sym,
                                    mutable: false,
                                },
                            );
                            Param {
                                name: sym,
                                ty: self.lower_type(&p.type_annotation, module_scope),
                                span: self.span_id(p.name.span, file.file_id),
                            }
                        })
                        .collect();
                    let ret = f
                        .return_type
                        .as_ref()
                        .map(|t| self.lower_type(t, module_scope));
                    let body =
                        self.lower_block(&f.body, module_id, file.file_id, ast, module_scope);
                    let span = self.span_id(f.span, file.file_id);
                    if let Some(hir_func) = self.functions.get_mut(fid.0 as usize) {
                        hir_func.type_params = type_params;
                        hir_func.params = params;
                        hir_func.ret = ret;
                        hir_func.body = body;
                        hir_func.span = span;
                    }
                    self.current_type_params.clear();
                }

                for g in &ast.globals {
                    let name = self.interner.resolve(&g.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let gid = *self.global_ids.get(&res_id).expect("missing global id");
                    let ty = self.lower_type(&g.type_annotation, module_scope);
                    let init = match self.lower_global_init(&g.value, &ty, file.file_id) {
                        Some(init) => init,
                        None => continue,
                    };
                    if let Some(hir_global) = self.globals.get_mut(gid.0 as usize) {
                        hir_global.ty = ty;
                        hir_global.init = init;
                    }
                }

                // Populate trait method signatures.
                for t in &ast.traits {
                    let name = self.interner.resolve(&t.name.sym).to_string();
                    let res_id = self.find_top_level_symbol(&name, module.id);
                    let tid = *self.trait_ids.get(&res_id).expect("missing trait id");
                    let methods: Vec<hir::TraitMethodSig> = t
                        .methods
                        .iter()
                        .map(|m| {
                            let params = m
                                .parameters
                                .iter()
                                .map(|p| self.lower_type(&p.type_annotation, module_scope))
                                .collect();
                            let ret = m
                                .return_type
                                .as_ref()
                                .map(|ty| self.lower_type(ty, module_scope));
                            hir::TraitMethodSig {
                                name: m.name.sym,
                                params,
                                ret,
                                span: self.span_id(m.name.span, file.file_id),
                            }
                        })
                        .collect();
                    if let Some(hir_trait) = self.traits.get_mut(tid.0 as usize) {
                        let mut method_idx = HashMap::with_capacity(methods.len());
                        for (i, m) in methods.iter().enumerate() {
                            method_idx.insert(m.name, i as u32);
                        }
                        hir_trait.methods = methods;
                        hir_trait.method_idx = method_idx;
                    }
                }

                // Populate impl method bodies. The (struct, method) → FuncId
                // map was built in pass 3; here we fill in params/ret/body
                // mirroring how regular functions are populated above.
                for im in &ast.impls {
                    let struct_name = self.interner.resolve(&im.struct_name.sym).to_string();
                    let Some(struct_res_id) = self
                        .module_scopes
                        .get(&module_id)
                        .and_then(|scope| scope.get(&struct_name).copied())
                    else {
                        continue;
                    };
                    let Some(owner) = self.method_owner(struct_res_id) else {
                        continue;
                    };
                    // Resolve the trait so we can index into its method order
                    // for the vtable. Vtables (dynamic dispatch) are built for
                    // struct receivers only; enums use static dispatch.
                    let trait_name = self.interner.resolve(&im.trait_name.sym).to_string();
                    let trait_res_id = self
                        .module_scopes
                        .get(&module_id)
                        .and_then(|scope| scope.get(&trait_name).copied());
                    let tid = trait_res_id.and_then(|id| self.trait_ids.get(&id).copied());
                    if let (Some(tid), hir::MethodOwner::Struct(sid)) = (tid, owner) {
                        let trait_def = self.traits.get(tid.0 as usize).cloned();
                        if let Some(trait_def) = trait_def {
                            let mut method_fids: Vec<FuncId> =
                                Vec::with_capacity(trait_def.methods.len());
                            for trait_m in &trait_def.methods {
                                if let Some(&fid) = self.impl_methods.get(&(owner, trait_m.name)) {
                                    method_fids.push(fid);
                                } else {
                                    // Missing impl for this method — leave
                                    // the slot with a sentinel; typecheck
                                    // surfaces this as an unknown method on
                                    // dispatch.
                                    method_fids.push(FuncId(u32::MAX));
                                }
                            }
                            self.impls.insert((tid, sid), method_fids);
                        }
                    }
                    for m in &im.methods {
                        let Some(&fid) = self.impl_methods.get(&(owner, m.name.sym)) else {
                            continue;
                        };
                        self.local_scope.clear();
                        let params: Vec<Param> = m
                            .parameters
                            .iter()
                            .map(|p| {
                                let sym =
                                    self.insert_symbol(module_id, p.name.sym, SymbolKind::Param);
                                self.local_scope.insert(
                                    self.interner.resolve(&p.name.sym).to_string(),
                                    LocalBinding {
                                        symbol: sym,
                                        mutable: false,
                                    },
                                );
                                Param {
                                    name: sym,
                                    ty: self.lower_type(&p.type_annotation, module_scope),
                                    span: self.span_id(p.name.span, file.file_id),
                                }
                            })
                            .collect();
                        let ret = m
                            .return_type
                            .as_ref()
                            .map(|t| self.lower_type(t, module_scope));
                        let body =
                            self.lower_block(&m.body, module_id, file.file_id, ast, module_scope);
                        let span = self.span_id(m.name.span, file.file_id);
                        if let Some(hir_func) = self.functions.get_mut(fid.0 as usize) {
                            hir_func.params = params;
                            hir_func.ret = ret;
                            hir_func.body = body;
                            hir_func.span = span;
                        }
                    }
                }
            }
        }
    }

    /// Validate a global's initializer expression. Wasm globals can only
    /// be initialized with a constant, so we accept literal numbers and
    /// booleans only. Anything else is a lowering error.
    fn lower_global_init(
        &mut self,
        expr: &prim_parse::Expr,
        ty: &hir::Type,
        file_id: FileId,
    ) -> Option<hir::GlobalInit> {
        use prim_parse::ExprKind;
        match (&expr.kind, ty) {
            (ExprKind::Int(n), hir::Type::I64 | hir::Type::U64) => Some(hir::GlobalInit::I64(*n)),
            (ExprKind::Int(n), _) => Some(hir::GlobalInit::I32(*n as i32)),
            (ExprKind::Float(v), hir::Type::F32) => Some(hir::GlobalInit::F32(*v as f32)),
            (ExprKind::Float(v), _) => Some(hir::GlobalInit::F64(*v)),
            (ExprKind::Bool(b), _) => Some(hir::GlobalInit::I32(if *b { 1 } else { 0 })),
            _ => {
                self.errors.push(LoweringError::NonConstantGlobalInit {
                    file: file_id,
                    span: expr.span,
                });
                None
            }
        }
    }

    fn finish(self) -> hir::Program {
        hir::Program {
            modules: self.modules,
            functions: self.functions,
            structs: self.structs,
            enums: self.enums,
            globals: self.globals,
            traits: self.traits,
            impl_methods: self.impl_methods,
            impls: self.impls,
            symbols: self.symbols,
            interner: self.interner,
            main: self.main,
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
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(Symbol {
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
    ) -> hir::Stmt {
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
                    self.interner.resolve(&name.sym).to_string(),
                    LocalBinding {
                        symbol: sym,
                        mutable: *mutable,
                    },
                );
                hir::Stmt::Let {
                    name: sym,
                    mutable: *mutable,
                    ty: self.lower_type(
                        type_annotation.as_ref().unwrap_or(&Type::Undetermined),
                        module_scope,
                    ),
                    value: value_hir,
                    span: self.span_id(name.span, file_id),
                }
            }
            Stmt::Assign { target, value } => {
                let target_name = self.interner.resolve(&target.sym).to_string();
                let binding = self.local_scope.get(&target_name).copied();
                if let Some(binding) = binding {
                    if !binding.mutable {
                        self.errors.push(LoweringError::AssignToImmutable {
                            name: target_name,
                            file: file_id,
                            span: target.span,
                        });
                    }
                    hir::Stmt::Assign {
                        target: binding.symbol,
                        value: self.lower_expr(value, module, file_id, ast, module_scope),
                        span: self.span_id(target.span, file_id),
                    }
                } else if let Some(&res_id) = module_scope.get(&target_name) {
                    // Module-level binding (e.g. a global). Mutability is
                    // checked again in typecheck; lowering just routes the
                    // target symbol through.
                    let sym = self.hir_symbol(res_id);
                    let info = &self.symbols[sym.0 as usize];
                    if !matches!(info.kind, hir::SymbolKind::Global(_)) {
                        self.errors.push(LoweringError::AssignToImmutable {
                            name: target_name,
                            file: file_id,
                            span: target.span,
                        });
                    }
                    hir::Stmt::Assign {
                        target: sym,
                        value: self.lower_expr(value, module, file_id, ast, module_scope),
                        span: self.span_id(target.span, file_id),
                    }
                } else {
                    self.errors.push(LoweringError::UnknownName {
                        name: target_name,
                        file: file_id,
                        span: target.span,
                    });
                    let span = self.span_id(target.span, file_id);
                    hir::Stmt::Expr(hir::Expr {
                        kind: hir::ExprKind::Error,
                        ty: hir::Type::Undetermined,
                        span,
                    })
                }
            }
            Stmt::DerefAssign { ptr, value } => {
                let span = self.span_id(ptr.span, file_id);
                let ptr_hir = self.lower_expr(ptr, module, file_id, ast, module_scope);
                let value_hir = self.lower_expr(value, module, file_id, ast, module_scope);
                hir::Stmt::DerefAssign {
                    ptr: ptr_hir,
                    value: value_hir,
                    span,
                }
            }
            Stmt::Expr(expr) => {
                hir::Stmt::Expr(self.lower_expr(expr, module, file_id, ast, module_scope))
            }
            Stmt::Loop { body, span } => hir::Stmt::Loop {
                body: self.lower_stmt_list(body, module, file_id, ast, module_scope),
                span: self.span_id(*span, file_id),
            },
            Stmt::While {
                condition,
                body,
                span,
            } => hir::Stmt::While {
                condition: self.lower_expr(condition, module, file_id, ast, module_scope),
                body: self.lower_stmt_list(body, module, file_id, ast, module_scope),
                span: self.span_id(*span, file_id),
            },
            Stmt::Break { span } => hir::Stmt::Break {
                span: self.span_id(*span, file_id),
            },
            Stmt::Return { value, span } => hir::Stmt::Return {
                value: value
                    .as_ref()
                    .map(|e| self.lower_expr(e, module, file_id, ast, module_scope)),
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
    ) -> hir::Block {
        self.local_scope.push();
        let hir_stmts = stmts
            .iter()
            .map(|s| self.lower_stmt(s, module, file_id, ast, module_scope))
            .collect();
        self.local_scope.pop();
        hir::Block {
            stmts: hir_stmts,
            expr: None,
        }
    }

    fn lower_match_arm(
        &mut self,
        arm: &prim_parse::MatchArm,
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> hir::MatchArm {
        // Pattern bindings introduce locals scoped to the arm body, so
        // we open and close a fresh local scope around it.
        self.local_scope.push();
        let pattern = self.lower_pattern(&arm.pattern, module, file_id, module_scope);
        let body = self.lower_expr(&arm.body, module, file_id, ast, module_scope);
        self.local_scope.pop();
        hir::MatchArm {
            pattern,
            body,
            span: self.span_id(arm.span, file_id),
        }
    }

    fn lower_pattern(
        &mut self,
        pat: &prim_parse::Pattern,
        module: ModuleId,
        file_id: FileId,
        module_scope: &ModuleScope,
    ) -> hir::Pattern {
        match pat {
            prim_parse::Pattern::Wildcard { span } => hir::Pattern::Wildcard {
                span: self.span_id(*span, file_id),
            },
            prim_parse::Pattern::Variant {
                enum_path,
                variant_name,
                bindings,
                span,
            } => {
                let name_str = self.path_name(enum_path);
                let res_id = self.resolve_symbol_path(enum_path, file_id, module_scope);
                let eid = res_id.and_then(|r| self.enum_ids.get(&r).copied());
                let (enum_id, variant_idx) = match eid {
                    Some(eid) => match self
                        .enums
                        .get(eid.0 as usize)
                        .and_then(|e| e.variant_idx.get(&variant_name.sym).copied())
                    {
                        Some(idx) => (eid, idx),
                        None => {
                            let vname = self.interner.resolve(&variant_name.sym).to_string();
                            self.errors.push(LoweringError::UnknownName {
                                name: format!("variant '{}' in enum '{}'", vname, name_str),
                                file: file_id,
                                span: variant_name.span,
                            });
                            (eid, 0)
                        }
                    },
                    None => {
                        // `res_id` resolved but isn't an enum; if it didn't
                        // resolve at all, `resolve_symbol_path` already erred.
                        if res_id.is_some() {
                            self.errors.push(LoweringError::NotAnEnum {
                                name: name_str,
                                file: file_id,
                                span: enum_path.segments[0].span,
                            });
                        }
                        (hir::EnumId(0), 0)
                    }
                };
                // Each binding introduces a fresh Local in the arm scope.
                let binding_pairs: Vec<(hir::InternSymbol, SymbolId, hir::Type)> = bindings
                    .iter()
                    .map(|b| {
                        let sym = self.insert_symbol(module, b.binding.sym, SymbolKind::Local);
                        self.local_scope.insert(
                            self.interner.resolve(&b.binding.sym).to_string(),
                            LocalBinding {
                                symbol: sym,
                                mutable: false,
                            },
                        );
                        (b.field.sym, sym, hir::Type::Undetermined)
                    })
                    .collect();
                hir::Pattern::Variant {
                    enum_id,
                    variant_idx,
                    bindings: binding_pairs,
                    span: self.span_id(*span, file_id),
                }
            }
        }
    }

    fn lower_block(
        &mut self,
        block: &prim_parse::Block,
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> hir::Block {
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
        hir::Block { stmts, expr }
    }

    fn lower_expr(
        &mut self,
        expr: &Expr,
        module: ModuleId,
        file_id: FileId,
        ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> hir::Expr {
        let span = self.span_id(expr.span, file_id);
        let error = || hir::Expr {
            kind: hir::ExprKind::Error,
            ty: hir::Type::Undetermined,
            span,
        };
        let (kind, ty) = match &expr.kind {
            ExprKind::Int(value) => (
                hir::ExprKind::Int(*value),
                self.lower_int_type(&expr.ty, module_scope),
            ),
            ExprKind::Float(value) => (
                hir::ExprKind::Float(*value),
                self.lower_float_type(&expr.ty, module_scope),
            ),
            ExprKind::Bool(value) => (
                hir::ExprKind::Bool(*value),
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::String(value) => (
                hir::ExprKind::Str(value.clone()),
                self.stdlib_string_struct
                    .map(|sid| hir::Type::Struct(sid, Vec::new()))
                    .unwrap_or(hir::Type::Undetermined),
            ),
            ExprKind::Ident(ident) => {
                let name_str = self.interner.resolve(&ident.sym).to_string();
                match self.resolve_name(&name_str, module, file_id, ident.span, module_scope) {
                    Some(sym) => (
                        hir::ExprKind::Ident(sym),
                        self.lower_type(&expr.ty, module_scope),
                    ),
                    None => return error(),
                }
            }
            ExprKind::Path(path) => {
                return self.lower_path_expr(path, expr.span, module, file_id, module_scope);
            }
            ExprKind::Binary { left, op, right } => (
                hir::ExprKind::Binary {
                    op: *op,
                    left: Box::new(self.lower_expr(left, module, file_id, ast, module_scope)),
                    right: Box::new(self.lower_expr(right, module, file_id, ast, module_scope)),
                },
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::FunctionCall { path, args } => {
                if let Some(receiver) = self.lower_path_call_receiver(path, file_id) {
                    let method = *path.segments.last().expect("method segment");
                    return hir::Expr {
                        kind: hir::ExprKind::MethodCall {
                            receiver: Box::new(receiver),
                            method: method.sym,
                            args: args
                                .iter()
                                .map(|a| self.lower_expr(a, module, file_id, ast, module_scope))
                                .collect(),
                        },
                        ty: self.lower_type(&expr.ty, module_scope),
                        span,
                    };
                }
                let call_span = path.segments.last().expect("empty path").span;
                let fid = self
                    .resolve_function_path(path, file_id, ast, module_scope)
                    .and_then(|id| self.func_ids.get(&id).copied());
                match fid {
                    Some(fid) => {
                        return hir::Expr {
                            kind: hir::ExprKind::Call {
                                func: fid,
                                type_args: Vec::new(),
                                args: args
                                    .iter()
                                    .map(|a| self.lower_expr(a, module, file_id, ast, module_scope))
                                    .collect(),
                            },
                            ty: self.lower_type(&expr.ty, module_scope),
                            span: self.span_id(call_span, file_id),
                        };
                    }
                    None => return error(),
                }
            }
            ExprKind::StructLiteral { name, fields } => {
                let struct_name = self.interner.resolve(&name.sym);
                let struct_id = module_scope
                    .get(struct_name)
                    .and_then(|res_id| self.struct_ids.get(res_id).copied());
                match struct_id {
                    Some(struct_id) => (
                        hir::ExprKind::StructLit {
                            struct_id,
                            type_args: Vec::new(),
                            fields: fields
                                .iter()
                                .map(|f| {
                                    (
                                        f.name.sym,
                                        self.lower_expr(
                                            &f.value,
                                            module,
                                            file_id,
                                            ast,
                                            module_scope,
                                        ),
                                    )
                                })
                                .collect(),
                        },
                        self.lower_type(&expr.ty, module_scope),
                    ),
                    None => {
                        self.errors.push(LoweringError::UnknownStruct {
                            name: struct_name.to_string(),
                            file: file_id,
                            span: name.span,
                        });
                        return error();
                    }
                }
            }
            ExprKind::VariantLiteral {
                enum_path,
                variant_name,
                fields,
            } => {
                let name_str = self.path_name(enum_path);
                let res_id = self.resolve_symbol_path(enum_path, file_id, module_scope);
                let eid = res_id.and_then(|r| self.enum_ids.get(&r).copied());
                match eid {
                    Some(eid) => {
                        let variant_idx = self
                            .enums
                            .get(eid.0 as usize)
                            .and_then(|e| e.variant_idx.get(&variant_name.sym).copied());
                        match variant_idx {
                            Some(variant_idx) => (
                                hir::ExprKind::VariantLit {
                                    enum_id: eid,
                                    variant_idx,
                                    type_args: Vec::new(),
                                    fields: fields
                                        .iter()
                                        .map(|f| {
                                            (
                                                f.name.sym,
                                                self.lower_expr(
                                                    &f.value,
                                                    module,
                                                    file_id,
                                                    ast,
                                                    module_scope,
                                                ),
                                            )
                                        })
                                        .collect(),
                                },
                                hir::Type::Enum(eid, Vec::new()),
                            ),
                            None => {
                                let vname = self.interner.resolve(&variant_name.sym).to_string();
                                self.errors.push(LoweringError::UnknownName {
                                    name: format!("variant '{}' in enum '{}'", vname, name_str),
                                    file: file_id,
                                    span: variant_name.span,
                                });
                                return error();
                            }
                        }
                    }
                    None => {
                        // `res_id` resolved but isn't an enum; if it didn't
                        // resolve at all, `resolve_symbol_path` already erred.
                        if res_id.is_some() {
                            self.errors.push(LoweringError::NotAnEnum {
                                name: name_str,
                                file: file_id,
                                span: enum_path.segments[0].span,
                            });
                        }
                        return error();
                    }
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                let scrut_hir = self.lower_expr(scrutinee, module, file_id, ast, module_scope);
                let arms_hir = arms
                    .iter()
                    .map(|arm| self.lower_match_arm(arm, module, file_id, ast, module_scope))
                    .collect();
                (
                    hir::ExprKind::Match {
                        scrutinee: Box::new(scrut_hir),
                        arms: arms_hir,
                    },
                    self.lower_type(&expr.ty, module_scope),
                )
            }
            ExprKind::FieldAccess { object, field } => (
                hir::ExprKind::Field {
                    base: Box::new(self.lower_expr(object, module, file_id, ast, module_scope)),
                    field: field.sym,
                },
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::MethodCall {
                receiver,
                method,
                args,
            } => (
                hir::ExprKind::MethodCall {
                    receiver: Box::new(self.lower_expr(
                        receiver,
                        module,
                        file_id,
                        ast,
                        module_scope,
                    )),
                    method: method.sym,
                    args: args
                        .iter()
                        .map(|a| self.lower_expr(a, module, file_id, ast, module_scope))
                        .collect(),
                },
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::Dereference(operand) => (
                hir::ExprKind::Deref(Box::new(self.lower_expr(
                    operand,
                    module,
                    file_id,
                    ast,
                    module_scope,
                ))),
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::BitNot(operand) => (
                hir::ExprKind::BitNot(Box::new(self.lower_expr(
                    operand,
                    module,
                    file_id,
                    ast,
                    module_scope,
                ))),
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::Array(elements) => (
                hir::ExprKind::ArrayLit(
                    elements
                        .iter()
                        .map(|e| self.lower_expr(e, module, file_id, ast, module_scope))
                        .collect(),
                ),
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => (
                hir::ExprKind::If {
                    condition: Box::new(self.lower_expr(
                        condition,
                        module,
                        file_id,
                        ast,
                        module_scope,
                    )),
                    then_branch: self.lower_block(then_branch, module, file_id, ast, module_scope),
                    else_branch: else_branch
                        .as_ref()
                        .map(|b| self.lower_block(b, module, file_id, ast, module_scope)),
                },
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::Block(block) => (
                hir::ExprKind::Block(self.lower_block(block, module, file_id, ast, module_scope)),
                self.lower_type(&expr.ty, module_scope),
            ),
            ExprKind::Dbg(inner) => {
                let inner_span = inner.span;
                // Use only the basename so output is portable across machines.
                let path_str = self
                    .source_map
                    .get_path(file_id)
                    .and_then(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()))
                    .unwrap_or_default();
                let source = self
                    .source_map
                    .read_source(file_id)
                    .unwrap_or(Arc::from(""));
                let (line, col) = inner_span.line_col(&source);
                let expr_text = inner_span.text(&source);
                let prefix = format!("[{path_str}:{line}:{col}] {expr_text} = ");
                let lowered_inner = self.lower_expr(inner, module, file_id, ast, module_scope);
                let dbg_ty = lowered_inner.ty.clone();
                (
                    hir::ExprKind::Dbg {
                        prefix,
                        inner: Box::new(lowered_inner),
                    },
                    dbg_ty,
                )
            }
        };
        hir::Expr { kind, ty, span }
    }

    fn resolve_name(
        &mut self,
        name: &str,
        _module: ModuleId,
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
            return Some(self.hir_symbol(res_id));
        }
        self.errors.push(LoweringError::UnknownName {
            name: name.to_string(),
            file,
            span,
        });
        None
    }

    fn path_name(&self, path: &prim_parse::NamePath) -> String {
        path.segments
            .iter()
            .map(|segment| self.interner.resolve(&segment.sym).to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn lookup_symbol_path(
        &self,
        path: &prim_parse::NamePath,
        module_scope: &ModuleScope,
    ) -> Option<ResSymbolId> {
        let name_ident = path.segments.last()?;
        let name = self.interner.resolve(&name_ident.sym);
        if path.segments.len() == 1 {
            return module_scope.get(name).copied();
        }

        let module_path: Vec<String> = path
            .segments
            .iter()
            .take(path.segments.len() - 1)
            .map(|ident| self.interner.resolve(&ident.sym).to_string())
            .collect();
        let key = crate::program::ModuleKey::Name(module_path);
        let target_module_id = self.program.module_index.get(&key)?;
        self.module_scopes
            .get(target_module_id)
            .and_then(|scope| scope.get(name).copied())
    }

    fn resolve_symbol_path(
        &mut self,
        path: &prim_parse::NamePath,
        file_id: FileId,
        module_scope: &ModuleScope,
    ) -> Option<ResSymbolId> {
        if let Some(id) = self.lookup_symbol_path(path, module_scope) {
            return Some(id);
        }

        let name_ident = path.segments.last().expect("empty path");
        let name = self.interner.resolve(&name_ident.sym);
        if path.segments.len() == 1 {
            self.errors.push(LoweringError::UnknownName {
                name: name.to_string(),
                file: file_id,
                span: name_ident.span,
            });
            return None;
        }

        let module_path: Vec<String> = path
            .segments
            .iter()
            .take(path.segments.len() - 1)
            .map(|ident| self.interner.resolve(&ident.sym).to_string())
            .collect();
        let key = crate::program::ModuleKey::Name(module_path.clone());
        if !self.program.module_index.contains_key(&key) {
            self.errors.push(LoweringError::UnknownModule {
                path: module_path.join("::"),
                file: file_id,
                span: path.segments[0].span,
            });
            return None;
        }

        self.errors.push(LoweringError::UnknownName {
            name: name.to_string(),
            file: file_id,
            span: name_ident.span,
        });
        None
    }

    fn lower_local_path(
        &mut self,
        path: &prim_parse::NamePath,
        span: Span,
        file_id: FileId,
    ) -> Option<hir::Expr> {
        let first = path.segments.first()?;
        let name = self.interner.resolve(&first.sym);
        let binding = *self.local_scope.get(name)?;
        let mut expr = hir::Expr {
            kind: hir::ExprKind::Ident(binding.symbol),
            ty: hir::Type::Undetermined,
            span: self.span_id(first.span, file_id),
        };
        for segment in path.segments.iter().skip(1) {
            expr = hir::Expr {
                kind: hir::ExprKind::Field {
                    base: Box::new(expr),
                    field: segment.sym,
                },
                ty: hir::Type::Undetermined,
                span: self.span_id(span, file_id),
            };
        }
        Some(expr)
    }

    fn lower_path_call_receiver(
        &mut self,
        path: &prim_parse::NamePath,
        file_id: FileId,
    ) -> Option<hir::Expr> {
        if path.segments.len() < 2 {
            return None;
        }
        let first_name = self.interner.resolve(&path.segments[0].sym);
        self.local_scope.get(first_name)?;
        let receiver_path = prim_parse::NamePath {
            segments: path.segments[..path.segments.len() - 1].to_vec(),
        };
        let span = receiver_path
            .segments
            .first()
            .expect("receiver path")
            .span
            .cover(receiver_path.segments.last().expect("receiver path").span);
        self.lower_local_path(&receiver_path, span, file_id)
    }

    fn lower_path_expr(
        &mut self,
        path: &prim_parse::NamePath,
        span: Span,
        _module: ModuleId,
        file_id: FileId,
        module_scope: &ModuleScope,
    ) -> hir::Expr {
        let span_id = self.span_id(span, file_id);
        let error = || hir::Expr {
            kind: hir::ExprKind::Error,
            ty: hir::Type::Undetermined,
            span: span_id,
        };

        if let Some(expr) = self.lower_local_path(path, span, file_id) {
            return expr;
        }

        if path.segments.len() >= 2 {
            let enum_path = prim_parse::NamePath {
                segments: path.segments[..path.segments.len() - 1].to_vec(),
            };
            let variant_name = path.segments.last().expect("variant segment");
            if let Some(enum_res_id) = self.lookup_symbol_path(&enum_path, module_scope) {
                if let Some(&eid) = self.enum_ids.get(&enum_res_id) {
                    let variant_idx = self
                        .enums
                        .get(eid.0 as usize)
                        .and_then(|e| e.variant_idx.get(&variant_name.sym).copied());
                    return match variant_idx {
                        Some(variant_idx) => hir::Expr {
                            kind: hir::ExprKind::VariantLit {
                                enum_id: eid,
                                variant_idx,
                                type_args: Vec::new(),
                                fields: Vec::new(),
                            },
                            ty: hir::Type::Enum(eid, Vec::new()),
                            span: span_id,
                        },
                        None => {
                            self.errors.push(LoweringError::UnknownName {
                                name: format!(
                                    "variant '{}' in enum '{}'",
                                    self.interner.resolve(&variant_name.sym),
                                    self.path_name(&enum_path)
                                ),
                                file: file_id,
                                span: variant_name.span,
                            });
                            error()
                        }
                    };
                }
                self.errors.push(LoweringError::NotAnEnum {
                    name: self.path_name(&enum_path),
                    file: file_id,
                    span: enum_path.segments[0].span,
                });
                return error();
            }
        }

        match self.resolve_symbol_path(path, file_id, module_scope) {
            Some(res_id) => hir::Expr {
                kind: hir::ExprKind::Ident(self.hir_symbol(res_id)),
                ty: hir::Type::Undetermined,
                span: span_id,
            },
            None => error(),
        }
    }

    fn resolve_function_path(
        &mut self,
        path: &prim_parse::NamePath,
        file_id: FileId,
        _ast: &prim_parse::Program,
        module_scope: &ModuleScope,
    ) -> Option<ResSymbolId> {
        let name_ident = path.segments.last().expect("empty path");
        let name = self.interner.resolve(&name_ident.sym);

        if let Some(id) = self.lookup_symbol_path(path, module_scope) {
            if self.func_ids.contains_key(&id) {
                return Some(id);
            }
        }

        if path.segments.len() > 1 {
            let module_path: Vec<String> = path
                .segments
                .iter()
                .take(path.segments.len() - 1)
                .map(|ident| self.interner.resolve(&ident.sym).to_string())
                .collect();
            let key = crate::program::ModuleKey::Name(module_path.clone());
            if !self.program.module_index.contains_key(&key) {
                self.errors.push(LoweringError::UnknownModule {
                    path: module_path.join("::"),
                    file: file_id,
                    span: path.segments[0].span,
                });
                return None;
            }
        }

        self.errors.push(LoweringError::UnknownFunction {
            name: name.to_string(),
            file: file_id,
            span: name_ident.span,
        });
        None
    }

    /// Lower a function's AST type-param list and populate
    /// `current_type_params` so subsequent `lower_type` calls in this
    /// function's signature and body resolve `T`, `U`, etc. The bound
    /// (if any) resolves through the module scope as a trait.
    fn lower_type_params(
        &mut self,
        params: &[prim_parse::TypeParam],
        module_id: ModuleId,
        file_id: FileId,
        module_scope: &ModuleScope,
    ) -> Vec<hir::TypeParam> {
        let mut out = Vec::with_capacity(params.len());
        for (idx, p) in params.iter().enumerate() {
            let tp_id = hir::TypeParamId(idx as u32);
            let name_str = self.interner.resolve(&p.name.sym).to_string();
            self.current_type_params.insert(name_str, tp_id);
            let bound = p.bound.as_ref().and_then(|b| {
                let bname = self.interner.resolve(&b.sym).to_string();
                let res_id = module_scope.get(&bname).copied()?;
                let tid = self.trait_ids.get(&res_id).copied();
                if tid.is_none() {
                    self.errors.push(LoweringError::UnknownName {
                        name: bname,
                        file: file_id,
                        span: b.span,
                    });
                }
                tid
            });
            let sym = self.insert_symbol(module_id, p.name.sym, SymbolKind::Unknown);
            out.push(hir::TypeParam {
                name: sym,
                bound,
                span: self.span_id(p.name.span, file_id),
            });
        }
        out
    }

    fn lower_type(&self, ty: &Type, module_scope: &ModuleScope) -> hir::Type {
        match ty {
            Type::Struct(name, type_args) => {
                let name_str = self.interner.resolve(name);
                // A named type can resolve to a type parameter of the
                // enclosing generic function, a struct, or a trait. Type
                // params shadow module-scope names.
                if let Some(&tp_id) = self.current_type_params.get(name_str) {
                    return hir::Type::Param(tp_id);
                }
                // The resolver validates all type names; this expect indicates a resolver bug.
                let res_id = *module_scope.get(name_str).unwrap_or_else(|| {
                    panic!("resolver should have caught unknown type '{name_str}'")
                });
                let lowered_args: Vec<hir::Type> = type_args
                    .iter()
                    .map(|t| self.lower_type(t, module_scope))
                    .collect();
                if let Some(&sid) = self.struct_ids.get(&res_id) {
                    hir::Type::Struct(sid, lowered_args)
                } else if let Some(&eid) = self.enum_ids.get(&res_id) {
                    hir::Type::Enum(eid, lowered_args)
                } else if let Some(&tid) = self.trait_ids.get(&res_id) {
                    hir::Type::Trait(tid)
                } else {
                    panic!("missing struct/trait/enum id for resolved type '{name_str}'")
                }
            }
            Type::Array(inner) => hir::Type::Array(Box::new(self.lower_type(inner, module_scope))),
            Type::Pointer { mutable, pointee } => hir::Type::Pointer {
                mutable: *mutable,
                pointee: Box::new(self.lower_type(pointee, module_scope)),
            },
            Type::Undetermined => hir::Type::Undetermined,
            Type::U8 => hir::Type::U8,
            Type::I8 => hir::Type::I8,
            Type::U16 => hir::Type::U16,
            Type::I16 => hir::Type::I16,
            Type::U32 => hir::Type::U32,
            Type::I32 => hir::Type::I32,
            Type::U64 => hir::Type::U64,
            Type::I64 => hir::Type::I64,
            Type::Usize => hir::Type::Usize,
            Type::Isize => hir::Type::Isize,
            Type::F32 => hir::Type::F32,
            Type::F64 => hir::Type::F64,
            Type::Bool => hir::Type::Bool,
        }
    }

    fn lower_int_type(&self, ty: &Type, module_scope: &ModuleScope) -> hir::Type {
        match ty {
            Type::Undetermined => hir::Type::IntVar,
            _ => self.lower_type(ty, module_scope),
        }
    }

    fn lower_float_type(&self, ty: &Type, module_scope: &ModuleScope) -> hir::Type {
        match ty {
            Type::Undetermined => hir::Type::FloatVar,
            _ => self.lower_type(ty, module_scope),
        }
    }

    fn span_id(&mut self, span: Span, file: FileId) -> SpanId {
        let id = SpanId(self.spans.len() as u32);
        self.spans.push((file, span));
        id
    }

    /// Convert a resolver-side symbol id to its HIR counterpart. The numbering
    /// is shared (`SymbolId(k) == ResSymbolId(k)` for all top-level symbols)
    /// because `declare_modules_and_items` eagerly creates the `Symbol`
    /// entries in resolver order.
    fn hir_symbol(&self, res_id: ResSymbolId) -> SymbolId {
        SymbolId(res_id.0)
    }

    fn convert_kind(&mut self, kind: ResSymbolKind, res_id: ResSymbolId) -> SymbolKind {
        match kind {
            ResSymbolKind::Function => {
                let fid = *self
                    .func_ids
                    .entry(res_id)
                    .or_insert_with(|| FuncId(self.functions.len() as u32));
                SymbolKind::Function(fid)
            }
            ResSymbolKind::Struct => {
                let sid = *self
                    .struct_ids
                    .entry(res_id)
                    .or_insert_with(|| StructId(self.structs.len() as u32));
                SymbolKind::Struct(sid)
            }
            ResSymbolKind::Global => {
                let gid = *self
                    .global_ids
                    .entry(res_id)
                    .or_insert_with(|| hir::GlobalId(self.globals.len() as u32));
                SymbolKind::Global(gid)
            }
            ResSymbolKind::Enum => SymbolKind::Unknown,
            ResSymbolKind::Trait => SymbolKind::Trait,
            ResSymbolKind::Impl => SymbolKind::Unknown,
            ResSymbolKind::Module => SymbolKind::Module,
        }
    }
}
