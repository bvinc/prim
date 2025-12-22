//! HIR-based Cranelift code generator (work in progress).
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use mangle::{string_literal_symbol, symbol_name};
use prim_hir::HirProgram;
use std::collections::HashMap;

pub mod error;
mod mangle;
pub use error::CodegenError;

pub struct CraneliftCodeGenerator {
    module: ObjectModule,
    ctx: codegen::Context,
    builder_context: FunctionBuilderContext,
    struct_layouts: HashMap<prim_hir::StructId, StructLayout>,
    func_ids: HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_param_counts: HashMap<prim_hir::FuncId, usize>,
    func_param_types: HashMap<prim_hir::FuncId, Vec<cranelift::prelude::Type>>,
}

fn coerce_value(
    builder: &mut FunctionBuilder,
    val: Value,
    expected: cranelift::prelude::Type,
) -> Value {
    // TODO: remove this once lowering emits the correct lane types directly.
    let got = builder.func.dfg.value_type(val);
    if got == expected {
        return val;
    }
    if got.is_int() && expected.is_int() {
        if got.bytes() < expected.bytes() {
            return builder.ins().uextend(expected, val);
        }
        return builder.ins().ireduce(expected, val);
    }
    if got.is_float() && expected.is_float() {
        if got == types::F32 && expected == types::F64 {
            return builder.ins().fpromote(expected, val);
        }
        if got == types::F64 && expected == types::F32 {
            return builder.ins().fdemote(expected, val);
        }
    }
    val
}

fn expected_return_lanes(
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> Result<Vec<cranelift::prelude::Type>, CodegenError> {
    match ty {
        prim_hir::Type::Struct(id) => {
            let layout = layouts
                .get(id)
                .ok_or(CodegenError::MissingStructLayout(*id))?;
            let mut lanes = Vec::with_capacity(layout.order.len());
            for field_sym in &layout.order {
                let field =
                    layout
                        .fields
                        .get(field_sym)
                        .ok_or(CodegenError::MissingStructField {
                            struct_id: *id,
                            field: *field_sym,
                        })?;
                lanes.push(field.ty);
            }
            Ok(lanes)
        }
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            Ok(vec![lane])
        }
    }
}

fn validate_return_values(
    builder: &mut FunctionBuilder,
    vals: Vec<Value>,
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> Result<Vec<Value>, CodegenError> {
    let expected = expected_return_lanes(ty, layouts)?;
    if vals.len() != expected.len() {
        return Err(CodegenError::ReturnArityMismatch {
            expected: expected.len(),
            found: vals.len(),
        });
    }
    for (val, expected_ty) in vals.iter().copied().zip(expected.into_iter()) {
        let got = builder.func.dfg.value_type(val);
        if got != expected_ty {
            return Err(CodegenError::ReturnTypeMismatch {
                expected: expected_ty,
                found: got,
            });
        }
    }
    Ok(vals)
}

fn expr_type(expr: &prim_hir::HirExpr) -> &prim_hir::Type {
    match expr {
        prim_hir::HirExpr::Int { ty, .. }
        | prim_hir::HirExpr::Float { ty, .. }
        | prim_hir::HirExpr::Bool { ty, .. }
        | prim_hir::HirExpr::Str { ty, .. }
        | prim_hir::HirExpr::Ident { ty, .. }
        | prim_hir::HirExpr::Binary { ty, .. }
        | prim_hir::HirExpr::Call { ty, .. }
        | prim_hir::HirExpr::StructLit { ty, .. }
        | prim_hir::HirExpr::Field { ty, .. }
        | prim_hir::HirExpr::Deref { ty, .. }
        | prim_hir::HirExpr::ArrayLit { ty, .. } => ty,
    }
}

trait StructTypeExt {
    fn as_struct(&self) -> Option<prim_hir::StructId>;
}

impl StructTypeExt for prim_hir::Type {
    fn as_struct(&self) -> Option<prim_hir::StructId> {
        match self {
            prim_hir::Type::Struct(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
struct StructLayout {
    fields: HashMap<prim_hir::SymbolId, FieldLayout>,
    order: Vec<prim_hir::SymbolId>,
}

#[derive(Clone, Debug)]
struct FieldLayout {
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
            func_param_types: HashMap::new(),
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
            let param_types: Vec<cranelift::prelude::Type> =
                sig.params.iter().map(|p| p.value_type).collect();
            if main_symbol(program) == Some(func.name) {
                sig.returns.push(AbiParam::new(types::I32));
            } else if let Some(ret) = &func.ret {
                append_return(&mut sig, ret, &self.struct_layouts)?;
            }

            let sym = export_symbol(func, program)?;
            let linkage = if main_symbol(program) == Some(func.name) {
                Linkage::Export
            } else if func.runtime_binding.is_some() {
                Linkage::Import
            } else {
                Linkage::Local
            };

            let func_id = self.module.declare_function(&sym, linkage, &sig)?;
            self.func_ids.insert(func.id, func_id);
            self.func_param_counts.insert(func.id, param_count);
            self.func_param_types.insert(func.id, param_types);
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
            let mut fields = HashMap::new();
            let mut order = Vec::new();
            for f in &st.fields {
                let (cl_ty, _) = scalar_lane(&f.ty)?;
                fields.insert(f.name, FieldLayout { ty: cl_ty });
                order.push(f.name);
            }
            self.struct_layouts
                .insert(st.id, StructLayout { fields, order });
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
        if main_symbol(program) == Some(func.name) {
            sig.returns.push(AbiParam::new(types::I32));
        } else if let Some(ret) = &func.ret {
            append_return(&mut sig, ret, &self.struct_layouts)?;
        }

        let func_id = *self.func_ids.get(&func.id).expect("missing function id");
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
        let mut loop_exits: Vec<Block> = Vec::new();
        for stmt in &func.body.stmts {
            let flow = lower_stmt_static(
                stmt,
                program,
                func.module,
                &mut builder,
                &mut locals,
                &self.struct_layouts,
                &self.func_ids,
                &self.func_param_counts,
                &self.func_param_types,
                &mut self.module,
                &mut loop_exits,
            )?;
            match flow {
                StmtFlow::Continue => {}
                StmtFlow::Value(vs) => last_val = Some(vs),
                StmtFlow::Terminated => break,
            }
        }

        let rets = if main_symbol(program) == Some(func.name) {
            vec![builder.ins().iconst(types::I32, 0)]
        } else if let Some(ret_ty) = &func.ret {
            let out = match last_val {
                Some(vs) if !vs.is_empty() => vs,
                _ => return Err(CodegenError::MissingReturnValue),
            };
            validate_return_values(&mut builder, out, ret_ty, &self.struct_layouts)?
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
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.returns.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

fn main_symbol(program: &HirProgram) -> Option<prim_hir::SymbolId> {
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
}

fn export_symbol(
    func: &prim_hir::HirFunction,
    program: &HirProgram,
) -> Result<String, CodegenError> {
    if let Some(binding) = &func.runtime_binding {
        return Ok(binding.clone());
    }
    if main_symbol(program) == Some(func.name) {
        return Ok("prim_main".to_string());
    }
    Ok(symbol_name(func.name, program))
}

fn abi_slot_count(
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
) -> usize {
    match ty {
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
            .expect("missing ABI parameter slots");
        locals.bind_local(param.name, slice.to_vec());
        idx += slots;
    }
    Ok(())
}

enum StmtFlow {
    Continue,
    Value(Vec<Value>),
    Terminated,
}

fn lower_stmt_static(
    stmt: &prim_hir::HirStmt,
    program: &HirProgram,
    module_id: prim_hir::ModuleId,
    builder: &mut FunctionBuilder,
    locals: &mut VarEnv,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    func_ids: &HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_param_counts: &HashMap<prim_hir::FuncId, usize>,
    func_param_types: &HashMap<prim_hir::FuncId, Vec<cranelift::prelude::Type>>,
    module: &mut ObjectModule,
    loop_exits: &mut Vec<Block>,
) -> Result<StmtFlow, CodegenError> {
    match stmt {
        prim_hir::HirStmt::Let { name, value, .. } => {
            let vals = lower_expr_static(
                value,
                program,
                module_id,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                func_param_types,
                module,
            )?;
            locals.bind_local(*name, vals.clone());
            Ok(StmtFlow::Continue)
        }
        prim_hir::HirStmt::Expr(expr) => {
            let vals = lower_expr_static(
                expr,
                program,
                module_id,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                func_param_types,
                module,
            )?;
            Ok(StmtFlow::Value(vals))
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
            loop_exits.push(exit);
            let mut terminated = false;
            for s in &body.stmts {
                let flow = lower_stmt_static(
                    s,
                    program,
                    module_id,
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    func_param_types,
                    module,
                    loop_exits,
                )?;
                match flow {
                    StmtFlow::Terminated => {
                        terminated = true;
                        break;
                    }
                    StmtFlow::Continue | StmtFlow::Value(_) => {}
                }
            }
            loop_exits.pop();
            if !terminated {
                builder.ins().jump(header, &[]);
            }
            builder.seal_block(body_block);

            builder.switch_to_block(exit);
            builder.seal_block(exit);
            Ok(StmtFlow::Continue)
        }
        prim_hir::HirStmt::Break { .. } => {
            let exit = loop_exits
                .last()
                .copied()
                .ok_or(CodegenError::InvalidBreak)?;
            builder.ins().jump(exit, &[]);
            Ok(StmtFlow::Terminated)
        }
    }
}

fn lower_expr_static(
    expr: &prim_hir::HirExpr,
    program: &HirProgram,
    module_id: prim_hir::ModuleId,
    builder: &mut FunctionBuilder,
    locals: &mut VarEnv,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    func_ids: &HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_param_counts: &HashMap<prim_hir::FuncId, usize>,
    func_param_types: &HashMap<prim_hir::FuncId, Vec<cranelift::prelude::Type>>,
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
        prim_hir::HirExpr::Str { value, ty, span } => {
            let (ptr, len) = make_string_data_static(
                builder,
                module,
                program,
                module_id,
                *span,
                value.as_bytes(),
            )?;
            let _ = ty;
            vec![ptr, len]
        }
        prim_hir::HirExpr::Ident { symbol, .. } => locals.use_symbol(*symbol)?,
        prim_hir::HirExpr::Binary {
            op, left, right, ..
        } => {
            let l = lower_expr_static(
                left,
                program,
                module_id,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                func_param_types,
                module,
            )?;
            let r = lower_expr_static(
                right,
                program,
                module_id,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                func_param_types,
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
        prim_hir::HirExpr::Call { func, args, .. } => {
            let target = *func_ids
                .get(func)
                .ok_or(CodegenError::MissingFunction(*func))?;
            let callee = module.declare_func_in_func(target, builder.func);
            let mut lowered_args = Vec::new();
            for a in args {
                lowered_args.extend(lower_expr_static(
                    a,
                    program,
                    module_id,
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    func_param_types,
                    module,
                )?);
            }
            if let Some(expected) = func_param_counts.get(func) {
                if lowered_args.len() != *expected {
                    return Err(CodegenError::ArityMismatch {
                        expected: *expected,
                        found: lowered_args.len(),
                    });
                }
            }
            if let Some(param_types) = func_param_types.get(func) {
                for (arg, expected_ty) in lowered_args.iter_mut().zip(param_types.iter().copied()) {
                    *arg = coerce_value(builder, *arg, expected_ty);
                }
            }
            let call = builder.ins().call(callee, &lowered_args);
            builder.inst_results(call).to_vec()
        }
        prim_hir::HirExpr::StructLit {
            struct_id, fields, ..
        } => {
            let layout = layouts
                .get(struct_id)
                .cloned()
                .ok_or(CodegenError::MissingStructLayout(*struct_id))?;
            let mut provided: HashMap<prim_hir::SymbolId, Value> = HashMap::new();
            for (field_sym, expr) in fields {
                let vals = lower_expr_static(
                    expr,
                    program,
                    module_id,
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    func_param_types,
                    module,
                )?;
                let v = *vals.first().ok_or(CodegenError::MissingStructValue {
                    struct_id: *struct_id,
                    field: *field_sym,
                })?;
                if !layout.fields.contains_key(field_sym) {
                    return Err(CodegenError::MissingStructField {
                        struct_id: *struct_id,
                        field: *field_sym,
                    });
                }
                provided.insert(*field_sym, v);
            }
            let mut values = Vec::with_capacity(layout.order.len());
            for field_sym in &layout.order {
                let v =
                    provided
                        .get(field_sym)
                        .copied()
                        .ok_or(CodegenError::MissingStructValue {
                            struct_id: *struct_id,
                            field: *field_sym,
                        })?;
                let target_ty = layout
                    .fields
                    .get(field_sym)
                    .map(|f| f.ty)
                    .unwrap_or(builder.func.dfg.value_type(v));
                values.push(coerce_value(builder, v, target_ty));
            }
            values
        }
        prim_hir::HirExpr::Field { base, field, .. } => {
            let base_vals = lower_expr_static(
                base,
                program,
                module_id,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                func_param_types,
                module,
            )?;
            let struct_id = expr_type(base)
                .as_struct()
                .ok_or(CodegenError::InvalidFieldAccess)?;
            let layout = layouts
                .get(&struct_id)
                .ok_or(CodegenError::MissingStructLayout(struct_id))?;
            let pos = layout.order.iter().position(|s| s == field).ok_or(
                CodegenError::MissingStructField {
                    struct_id,
                    field: *field,
                },
            )?;
            let val = base_vals
                .get(pos)
                .copied()
                .ok_or(CodegenError::MissingStructValue {
                    struct_id,
                    field: *field,
                })?;
            vec![val]
        }
        prim_hir::HirExpr::Deref { base, .. } => {
            let ptr = lower_expr_static(
                base,
                program,
                module_id,
                builder,
                locals,
                layouts,
                func_ids,
                func_param_counts,
                func_param_types,
                module,
            )?[0];
            let pointee = match base.ty() {
                prim_hir::Type::Pointer { pointee, .. } => pointee.as_ref(),
                _ => return Err(CodegenError::InvalidDereference),
            };
            let (lane, _) = scalar_lane(pointee)?;
            let loaded = builder.ins().load(lane, MemFlags::new(), ptr, 0);
            vec![loaded]
        }
        prim_hir::HirExpr::ArrayLit { elements, .. } => {
            if elements.is_empty() {
                vec![builder.ins().iconst(types::I64, 0)]
            } else {
                // For now, return the first element value as a stand-in.
                lower_expr_static(
                    &elements[0],
                    program,
                    module_id,
                    builder,
                    locals,
                    layouts,
                    func_ids,
                    func_param_counts,
                    func_param_types,
                    module,
                )?
            }
        }
    })
}

fn make_string_data_static(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    program: &HirProgram,
    module_id: prim_hir::ModuleId,
    span_id: prim_hir::SpanId,
    bytes: &[u8],
) -> Result<(Value, Value), CodegenError> {
    let sym_name = string_literal_symbol(program, module_id, span_id, bytes.len());
    let data_id = module.declare_data(&sym_name, Linkage::Local, true, false)?;
    let mut desc = cranelift_module::DataDescription::new();
    desc.define(bytes.to_vec().into_boxed_slice());
    module.define_data(data_id, &desc)?;
    let gv = module.declare_data_in_func(data_id, builder.func);
    let ptr = builder.ins().global_value(types::I64, gv);
    let len = builder.ins().iconst(types::I64, bytes.len() as i64);
    Ok((ptr, len))
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
            .ok_or(CodegenError::UndefinedLocal(sym))
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
