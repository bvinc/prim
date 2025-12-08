//! HIR-based Cranelift code generator (work in progress).
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use prim_hir::HirProgram;
use std::collections::HashMap;

pub mod error;
pub use error::CodegenError;

pub struct CraneliftCodeGenerator {
    module: ObjectModule,
    ctx: codegen::Context,
    builder_context: FunctionBuilderContext,
    struct_layouts: HashMap<prim_hir::StructId, StructLayout>,
    func_ids: HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_param_counts: HashMap<prim_hir::FuncId, usize>,
}

#[derive(Clone, Debug)]
struct StructLayout {
    fields: HashMap<prim_hir::SymbolId, FieldLayout>,
    order: Vec<prim_hir::SymbolId>,
    size: u32,
    align: u8,
}

#[derive(Clone, Debug)]
struct FieldLayout {
    offset: u32,
    size: u32,
    ty: cranelift::prelude::Type,
}

impl CraneliftCodeGenerator {
    pub fn new() -> Result<Self, CodegenError> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;
        let isa_builder =
            cranelift_native::builder().map_err(|msg| CodegenError::UnsupportedTarget {
                message: msg.to_string(),
            })?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let builder = ObjectBuilder::new(
            isa,
            "prim_program",
            cranelift_module::default_libcall_names(),
        )?;
        let module = ObjectModule::new(builder);
        let ctx = module.make_context();

        Ok(Self {
            module,
            ctx,
            builder_context: FunctionBuilderContext::new(),
            struct_layouts: HashMap::new(),
            func_ids: HashMap::new(),
            func_param_counts: HashMap::new(),
        })
    }

    pub fn generate(mut self, program: &HirProgram) -> Result<Vec<u8>, CodegenError> {
        self.compute_struct_layouts(program)?;
        // Declare all functions first to populate func_ids.
        for func in &program.items.functions {
            let mut sig = self.module.make_signature();
            for param in &func.params {
                append_abi_params(&mut sig, &param.ty, &self.struct_layouts)?;
            }
            let param_count = sig.params.len();
            if func.name == main_symbol(program) {
                sig.returns.push(AbiParam::new(types::I32));
            } else if let Some(ret) = &func.ret {
                append_return(&mut sig, ret, &self.struct_layouts)?;
            }

            let sym = export_symbol(func, program)?;
            let linkage = if func.name == main_symbol(program) {
                Linkage::Export
            } else if func.runtime_binding.is_some() {
                Linkage::Import
            } else {
                Linkage::Local
            };

            let func_id = self.module.declare_function(&sym, linkage, &sig)?;
            self.func_ids.insert(func.id, func_id);
            self.func_param_counts.insert(func.id, param_count);
        }

        for func in &program.items.functions {
            if func.runtime_binding.is_some() {
                continue;
            }
            self.generate_function(func, program)?;
        }
        let product = self.module.finish();
        Ok(product.emit()?)
    }

    fn compute_struct_layouts(&mut self, program: &HirProgram) -> Result<(), CodegenError> {
        for st in &program.items.structs {
            let mut offset = 0u32;
            let mut fields = HashMap::new();
            let mut order = Vec::new();
            for f in &st.fields {
                let (cl_ty, size) = scalar_lane(&f.ty)?;
                fields.insert(
                    f.name,
                    FieldLayout {
                        offset,
                        size,
                        ty: cl_ty,
                    },
                );
                order.push(f.name);
                offset += size;
            }
            let size = offset.max(1);
            self.struct_layouts.insert(
                st.id,
                StructLayout {
                    fields,
                    order,
                    size,
                    align: 8,
                },
            );
        }
        Ok(())
    }

    fn generate_function(
        &mut self,
        func: &prim_hir::HirFunction,
        program: &HirProgram,
    ) -> Result<(), CodegenError> {
        let mut sig = self.module.make_signature();
        for param in &func.params {
            append_abi_params(&mut sig, &param.ty, &self.struct_layouts)?;
        }
        if func.name == main_symbol(program) {
            sig.returns.push(AbiParam::new(types::I32));
        } else if let Some(ret) = &func.ret {
            append_return(&mut sig, ret, &self.struct_layouts)?;
        }

        let func_id = *self
            .func_ids
            .get(&func.id)
            .ok_or(CodegenError::Unimplemented)?;
        self.ctx.func.signature = sig.clone();

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let abi_params = builder.block_params(entry).to_vec();
        let mut locals = VarEnv::new();
        bind_params_static(&mut locals, &abi_params, func, &self.struct_layouts)?;

        let mut last_val: Option<Vec<Value>> = None;
        for stmt in &func.body.stmts {
            last_val = lower_stmt_static(
                stmt,
                &mut builder,
                &mut locals,
                &self.struct_layouts,
                &self.func_ids,
                &self.func_param_counts,
                &mut self.module,
            )?;
        }

        let rets = if func.name == main_symbol(program) {
            vec![builder.ins().iconst(types::I32, 0)]
        } else if let Some(ret_ty) = &func.ret {
            let out = match last_val {
                Some(vs) if !vs.is_empty() => vs,
                _ => vec![zero_value_static(
                    &mut builder,
                    ret_ty,
                    &self.struct_layouts,
                )?],
            };
            out
        } else {
            Vec::new()
        };
        builder.ins().return_(&rets);
        builder.finalize();

        self.module.define_function(func_id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);
        Ok(())
    }
}

fn scalar_lane(ty: &prim_hir::Type) -> Result<(cranelift::prelude::Type, u32), CodegenError> {
    use prim_hir::Type::*;
    let lane = match ty {
        Bool | I8 | U8 => types::I8,
        I16 | U16 => types::I16,
        I32 | U32 => types::I32,
        I64 | U64 | Isize | Usize | Pointer { .. } | Struct(_) | Array(_) => types::I64,
        F32 => types::F32,
        F64 => types::F64,
        StrSlice => types::I64,
        Undetermined => return Ok((types::I64, 8)),
    };
    let size = lane.bytes();
    Ok((lane, size))
}

fn append_abi_params(
    sig: &mut Signature,
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> Result<(), CodegenError> {
    match ty {
        prim_hir::Type::StrSlice => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        prim_hir::Type::Struct(id) => {
            if let Some(layout) = layouts.get(id) {
                for fld in &layout.order {
                    let l = &layout.fields[fld];
                    sig.params.push(AbiParam::new(l.ty));
                }
            } else {
                sig.params.push(AbiParam::new(types::I64));
            }
        }
        prim_hir::Type::Array(_) => sig.params.push(AbiParam::new(types::I64)),
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.params.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

fn append_return(
    sig: &mut Signature,
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> Result<(), CodegenError> {
    match ty {
        prim_hir::Type::Struct(id) => {
            if let Some(layout) = layouts.get(id) {
                for fld in &layout.order {
                    let l = &layout.fields[fld];
                    sig.returns.push(AbiParam::new(l.ty));
                }
            } else {
                let (lane, _) = scalar_lane(ty)?;
                sig.returns.push(AbiParam::new(lane));
            }
        }
        prim_hir::Type::StrSlice => {
            sig.returns.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.returns.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

fn main_symbol(program: &HirProgram) -> prim_hir::SymbolId {
    program
        .items
        .functions
        .iter()
        .find(|f| {
            program
                .symbols
                .entries
                .iter()
                .find(|e| e.id == f.name)
                .map(|e| e.name.as_str() == "main")
                .unwrap_or(false)
        })
        .map(|f| f.name)
        .unwrap_or(prim_hir::SymbolId(u32::MAX))
}

fn export_symbol(
    func: &prim_hir::HirFunction,
    program: &HirProgram,
) -> Result<String, CodegenError> {
    if let Some(binding) = &func.runtime_binding {
        return Ok(binding.clone());
    }
    if func.name == main_symbol(program) {
        return Ok("prim_main".to_string());
    }
    Ok(symbol_name(func.name, program))
}

fn symbol_name(sym: prim_hir::SymbolId, program: &HirProgram) -> String {
    program
        .symbols
        .entries
        .iter()
        .find(|e| e.id == sym)
        .map(|e| e.name.clone())
        .unwrap_or_else(|| format!("sym_{}", sym.0))
}

fn abi_slot_count(
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> usize {
    match ty {
        prim_hir::Type::StrSlice => 2,
        prim_hir::Type::Struct(id) => layouts.get(id).map(|l| l.order.len()).unwrap_or(1),
        _ => 1,
    }
}

fn bind_params_static(
    locals: &mut VarEnv,
    abi_params: &[Value],
    func: &prim_hir::HirFunction,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> Result<(), CodegenError> {
    let mut idx = 0usize;
    for param in &func.params {
        let slots = abi_slot_count(&param.ty, layouts);
        let slice = abi_params
            .get(idx..idx + slots)
            .ok_or_else(|| {
                eprintln!("param slot missing for {:?}", param.name);
                CodegenError::Unimplemented
            })?;
        locals.bind_local(param.name, slice.to_vec());
        idx += slots;
    }
    Ok(())
}

fn lower_stmt_static(
    stmt: &prim_hir::HirStmt,
    builder: &mut FunctionBuilder,
    locals: &mut VarEnv,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    func_ids: &HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_param_counts: &HashMap<prim_hir::FuncId, usize>,
    module: &mut ObjectModule,
) -> Result<Option<Vec<Value>>, CodegenError> {
    match stmt {
        prim_hir::HirStmt::Let { name, value, .. } => {
            let vals = lower_expr_static(
                value,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                module,
            )?;
            locals.bind_local(*name, vals.clone());
            Ok(None)
        }
        prim_hir::HirStmt::Expr(expr) => {
            let vals = lower_expr_static(
                expr,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                module,
            )?;
            Ok(Some(vals))
        }
        prim_hir::HirStmt::Loop { body, .. } => {
            let header = builder.create_block();
            let body_block = builder.create_block();
            let exit = builder.create_block();
            builder.ins().jump(header, &[]);
            builder.switch_to_block(header);
            builder.ins().jump(body_block, &[]);
            builder.seal_block(header);

            builder.switch_to_block(body_block);
            for s in &body.stmts {
                lower_stmt_static(
                    s,
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    module,
                )?;
            }
            builder.ins().jump(header, &[]);
            builder.seal_block(body_block);

            builder.switch_to_block(exit);
            builder.seal_block(exit);
            Ok(None)
        }
        prim_hir::HirStmt::Break { .. } => {
            builder.ins().return_(&[]);
            Ok(Some(Vec::new()))
        }
    }
}

fn lower_expr_static(
    expr: &prim_hir::HirExpr,
    builder: &mut FunctionBuilder,
    locals: &mut VarEnv,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    func_ids: &HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_param_counts: &HashMap<prim_hir::FuncId, usize>,
    module: &mut ObjectModule,
) -> Result<Vec<Value>, CodegenError> {
    Ok(match expr {
        prim_hir::HirExpr::Int { value, ty, .. } => {
            let (lane, _) = scalar_lane(ty)?;
            vec![builder.ins().iconst(lane, *value)]
        }
        prim_hir::HirExpr::Float { value, ty, .. } => {
            let v = if matches!(ty, prim_hir::Type::F32) {
                builder.ins().f32const(Ieee32::with_float(*value as f32))
            } else {
                builder.ins().f64const(Ieee64::with_float(*value))
            };
            vec![v]
        }
        prim_hir::HirExpr::Bool { value, .. } => {
            let v = builder.ins().iconst(types::I8, if *value { 1 } else { 0 });
            vec![v]
        }
        prim_hir::HirExpr::Str { value, ty, .. } => {
            let (ptr, len) = make_string_data_static(builder, module, value.as_bytes())?;
            if matches!(ty, prim_hir::Type::StrSlice) {
                vec![ptr, len]
            } else {
                vec![ptr]
            }
        }
        prim_hir::HirExpr::Ident { symbol, ty, .. } => locals
            .use_symbol(*symbol)
            .or_else(|_| zero_value_static(builder, ty, layouts).map(|v| vec![v]))?,
        prim_hir::HirExpr::Binary {
            op, left, right, ..
        } => {
            let l = lower_expr_static(
                left,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                module,
            )?;
            let r = lower_expr_static(
                right,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                module,
            )?;
            let (l, r) = (l[0], r[0]);
            let res = match op {
                prim_hir::BinaryOp::Add => builder.ins().iadd(l, r),
                prim_hir::BinaryOp::Subtract => builder.ins().isub(l, r),
                prim_hir::BinaryOp::Multiply => builder.ins().imul(l, r),
                prim_hir::BinaryOp::Divide => builder.ins().sdiv(l, r),
                prim_hir::BinaryOp::Equals => {
                    let cmp = builder.ins().icmp(IntCC::Equal, l, r);
                    let one = builder.ins().iconst(types::I8, 1);
                    let zero = builder.ins().iconst(types::I8, 0);
                    builder.ins().select(cmp, one, zero)
                }
            };
            vec![res]
        }
        prim_hir::HirExpr::Call { func, args, ty, .. } => {
            if func.0 == u32::MAX {
                return Ok(vec![zero_value_static(builder, ty, layouts)?]);
            }
            let target = match func_ids.get(func) {
                Some(id) => *id,
                None => {
                    return Ok(vec![zero_value_static(builder, ty, layouts)?]);
                }
            };
            let callee = module.declare_func_in_func(target, builder.func);
            let mut lowered_args = Vec::new();
            for a in args {
                lowered_args.extend(lower_expr_static(
                    a,
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    module,
                )?);
            }
            if let Some(expected) = func_param_counts.get(func) {
                if lowered_args.len() > *expected {
                    lowered_args.truncate(*expected);
                } else if lowered_args.len() < *expected {
                    while lowered_args.len() < *expected {
                        lowered_args
                            .push(builder.ins().iconst(types::I64, 0));
                    }
                }
            }
            let call = builder.ins().call(callee, &lowered_args);
            builder.inst_results(call).to_vec()
        }
        prim_hir::HirExpr::StructLit {
            struct_id, fields, ..
        } => {
            let layout = layouts.get(struct_id).cloned().unwrap_or(StructLayout {
                fields: HashMap::new(),
                order: Vec::new(),
                size: 0,
                align: 8,
            });
            let mut values = vec![builder.ins().iconst(types::I64, 0); layout.order.len()];
            for (field_sym, expr) in fields {
                if let Some(pos) = layout.order.iter().position(|s| s == field_sym) {
                    let vals = lower_expr_static(
                        expr,
                        builder,
                        locals,
                        layouts,
                        func_ids,
                        func_param_counts,
                        module,
                    )?;
                    if let Some(v) = vals.first() {
                        values[pos] = *v;
                    }
                }
            }
            values
        }
        prim_hir::HirExpr::Field { base, field, .. } => {
            let base_vals = lower_expr_static(
                base,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                module,
            )?;
            let idx = field.0 as usize;
            vec![
                base_vals
                    .get(idx)
                    .copied()
                    .unwrap_or_else(|| builder.ins().iconst(types::I64, 0)),
            ]
        }
        prim_hir::HirExpr::Deref { base, .. } => {
            let ptr = lower_expr_static(
                base,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                module,
            )?[0];
            let loaded = builder.ins().load(types::I64, MemFlags::new(), ptr, 0);
            vec![loaded]
        }
        prim_hir::HirExpr::ArrayLit { elements, .. } => {
            if elements.is_empty() {
                vec![builder.ins().iconst(types::I64, 0)]
            } else {
                // For now, return the first element value as a stand-in.
                lower_expr_static(
                    &elements[0],
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    module,
                )?
            }
        }
    })
}

fn make_string_data_static(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    bytes: &[u8],
) -> Result<(Value, Value), CodegenError> {
    let sym_name = format!("strlit_{}", bytes.len());
    let data_id = module.declare_data(&sym_name, Linkage::Local, true, false)?;
    let mut desc = cranelift_module::DataDescription::new();
    desc.define(bytes.to_vec().into_boxed_slice());
    module.define_data(data_id, &desc)?;
    let gv = module.declare_data_in_func(data_id, builder.func);
    let ptr = builder.ins().global_value(types::I64, gv);
    let len = builder.ins().iconst(types::I64, bytes.len() as i64);
    Ok((ptr, len))
}

fn zero_value_static(
    builder: &mut FunctionBuilder,
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> Result<Value, CodegenError> {
    match ty {
        prim_hir::Type::Struct(id) => {
            if let Some(layout) = layouts.get(id) {
                let lane = layout
                    .order
                    .first()
                    .and_then(|f| layout.fields.get(f))
                    .map(|f| f.ty)
                    .unwrap_or(types::I64);
                Ok(builder.ins().iconst(lane, 0))
            } else {
                Ok(builder.ins().iconst(types::I64, 0))
            }
        }
        prim_hir::Type::StrSlice => Ok(builder.ins().iconst(types::I64, 0)),
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            if lane == types::F32 {
                Ok(builder.ins().f32const(Ieee32::with_bits(0)))
            } else if lane == types::F64 {
                Ok(builder.ins().f64const(Ieee64::with_bits(0)))
            } else {
                Ok(builder.ins().iconst(lane, 0))
            }
        }
    }
}

#[derive(Default)]
struct VarEnv {
    locals: HashMap<prim_hir::SymbolId, Vec<Value>>,
}

impl VarEnv {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    fn bind_local(&mut self, sym: prim_hir::SymbolId, values: Vec<Value>) {
        self.locals.insert(sym, values);
    }

    fn use_symbol(&self, sym: prim_hir::SymbolId) -> Result<Vec<Value>, CodegenError> {
        self.locals
            .get(&sym)
            .cloned()
            .ok_or(CodegenError::Unimplemented)
    }
}

/// Entry point used by prim-cli.
pub fn generate_object_code(
    _program: &prim_compiler::Program,
    _hir: &HirProgram,
) -> Result<Vec<u8>, CodegenError> {
    let codegen = CraneliftCodeGenerator::new()?;
    codegen.generate(_hir)
}
