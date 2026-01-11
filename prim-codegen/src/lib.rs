//! HIR-based Cranelift code generator (work in progress).
use cranelift::codegen::ir::{ArgumentPurpose, BlockArg};
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use mangle::{string_literal_symbol, symbol_name};
use prim_hir::HirProgram;
use std::collections::HashMap;

pub mod error;
mod mangle;
pub use error::CodegenError;

struct CraneliftCodeGenerator {
    module: ObjectModule,
    pointer_type: cranelift::prelude::Type,
    struct_layouts: HashMap<prim_hir::StructId, StructLayout>,
    func_ids: HashMap<prim_hir::FuncId, cranelift_module::FuncId>,
    func_params: HashMap<prim_hir::FuncId, Vec<cranelift::prelude::Type>>,
}

fn expected_return_lanes(
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    pointer_type: types::Type,
) -> Result<Vec<cranelift::prelude::Type>, CodegenError> {
    match ty {
        prim_hir::Type::Struct(id) => {
            let layout = layouts
                .get(id)
                .ok_or(CodegenError::MissingStructLayout(*id))?;
            let mut lanes = Vec::with_capacity(layout.fields.len());
            for field in &layout.fields {
                let lane = field
                    .ty
                    .as_scalar(pointer_type)
                    .ok_or(CodegenError::InvalidStructLane)?;
                lanes.push(lane);
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
    builder: &FunctionBuilder,
    vals: &[Value],
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    pointer_type: types::Type,
) -> Result<(), CodegenError> {
    let expected = expected_return_lanes(ty, layouts, pointer_type)?;
    if vals.len() != expected.len() {
        return Err(CodegenError::ReturnArityMismatch {
            expected: expected.len(),
            found: vals.len(),
        });
    }
    for (val, expected_ty) in vals.iter().zip(expected.iter()) {
        let got = builder.func.dfg.value_type(*val);
        if got != *expected_ty {
            return Err(CodegenError::ReturnTypeMismatch {
                expected: *expected_ty,
                found: got,
            });
        }
    }
    Ok(())
}

/// Layout information for a struct type.
#[derive(Clone, Debug)]
struct StructLayout {
    /// Total size in bytes (including padding).
    size: u32,
    /// Alignment requirement in bytes.
    align: u32,
    /// Fields in declaration order.
    fields: Vec<FieldLayout>,
}

/// Layout of a single struct field.
#[derive(Clone, Debug)]
struct FieldLayout {
    /// Field name for lookup.
    name: prim_hir::SymbolId,
    /// Byte offset from struct start (used for pointer-based access).
    offset: u32,
    /// Field type.
    ty: FieldType,
}

/// Type information for code generation.
#[derive(Clone, Debug)]
enum FieldType {
    /// Scalar: i8-i64, u8-u64, f32, f64, bool
    Scalar(cranelift::prelude::Type),
    /// Pointer to any type
    Pointer,
    /// Nested struct
    Struct(prim_hir::StructId),
    /// Fixed-size array [T; N]
    Array { elem: Box<FieldType>, len: u32 },
}

impl StructLayout {
    /// Find a field by name.
    fn field(&self, name: prim_hir::SymbolId) -> Option<&FieldLayout> {
        self.fields.iter().find(|f| f.name == name)
    }
}

impl FieldType {
    /// Get the Cranelift scalar type, if this is a scalar or pointer.
    fn as_scalar(&self, pointer_type: types::Type) -> Option<types::Type> {
        match self {
            FieldType::Scalar(ty) => Some(*ty),
            FieldType::Pointer => Some(pointer_type),
            FieldType::Struct(_) | FieldType::Array { .. } => None,
        }
    }

    /// Compute the byte size of this type.
    fn size(&self, pointer_size: u32, layouts: &HashMap<prim_hir::StructId, StructLayout>) -> u32 {
        match self {
            FieldType::Scalar(ty) => ty.bytes(),
            FieldType::Pointer => pointer_size,
            FieldType::Struct(id) => layouts.get(id).map(|l| l.size).unwrap_or(0),
            FieldType::Array { elem, len } => elem.size(pointer_size, layouts) * len,
        }
    }

    /// Compute the alignment of this type.
    fn align(&self, pointer_size: u32, layouts: &HashMap<prim_hir::StructId, StructLayout>) -> u32 {
        match self {
            FieldType::Scalar(ty) => ty.bytes(),
            FieldType::Pointer => pointer_size,
            FieldType::Struct(id) => layouts.get(id).map(|l| l.align).unwrap_or(1),
            FieldType::Array { elem, .. } => elem.align(pointer_size, layouts),
        }
    }
}

impl CraneliftCodeGenerator {
    pub fn new() -> Result<Self, CodegenError> {
        let isa_builder =
            cranelift_native::builder().map_err(|msg| CodegenError::UnsupportedTarget {
                message: msg.to_string(),
            })?;
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let builder = ObjectBuilder::new(
            isa,
            "prim_program",
            cranelift_module::default_libcall_names(),
        )?;
        let module = ObjectModule::new(builder);
        let pointer_type = module.isa().pointer_type();

        Ok(Self {
            module,
            pointer_type,
            struct_layouts: HashMap::new(),
            func_ids: HashMap::new(),
            func_params: HashMap::new(),
        })
    }

    pub fn generate(mut self, program: &HirProgram) -> Result<Vec<u8>, CodegenError> {
        self.compute_struct_layouts(program, self.pointer_type)?;
        // Declare all functions first to populate func_ids.
        for func in &program.items.functions {
            let sig = self.build_signature(func, program)?;
            let params: Vec<_> = sig.params.iter().map(|p| p.value_type).collect();

            let sym = export_symbol(func, program)?;
            let linkage = if program.main == Some(func.name) {
                Linkage::Export
            } else if func.runtime_binding.is_some() {
                Linkage::Import
            } else {
                Linkage::Local
            };

            let func_id = self.module.declare_function(&sym, linkage, &sig)?;
            self.func_ids.insert(func.id, func_id);
            self.func_params.insert(func.id, params);
        }

        let mut ctx = self.module.make_context();
        let mut builder_context = FunctionBuilderContext::new();
        for func in &program.items.functions {
            if func.runtime_binding.is_some() {
                continue;
            }
            self.generate_function(func, program, &mut ctx, &mut builder_context)?;
        }
        let product = self.module.finish();
        Ok(product.emit()?)
    }

    fn compute_struct_layouts(
        &mut self,
        program: &HirProgram,
        pointer_type: cranelift::prelude::Type,
    ) -> Result<(), CodegenError> {
        let pointer_size = pointer_type.bytes();

        // Process structs in order (assumes HIR provides them in dependency order)
        for st in &program.items.structs {
            let mut fields = Vec::new();
            let mut offset: u32 = 0;
            let mut struct_align: u32 = 1;

            for f in &st.fields {
                let field_type = hir_type_to_field_type(&f.ty)?;
                let field_align = field_type.align(pointer_size, &self.struct_layouts);
                let field_size = field_type.size(pointer_size, &self.struct_layouts);

                // Align offset to field's alignment requirement
                offset = align_to(offset, field_align);

                // Track maximum alignment for struct
                struct_align = struct_align.max(field_align);

                fields.push(FieldLayout {
                    name: f.name,
                    offset,
                    ty: field_type,
                });

                offset += field_size;
            }

            // Pad struct size to its alignment
            let size = align_to(offset, struct_align);

            self.struct_layouts.insert(
                st.id,
                StructLayout {
                    size,
                    align: struct_align,
                    fields,
                },
            );
        }
        Ok(())
    }

    fn build_signature(
        &self,
        func: &prim_hir::HirFunction,
        program: &HirProgram,
    ) -> Result<Signature, CodegenError> {
        let mut sig = self.module.make_signature();
        let is_runtime = func.runtime_binding.is_some();

        // Check if this function uses sret convention (not for runtime functions)
        let needs_sret = !is_runtime
            && func
                .ret
                .as_ref()
                .map(|ret| uses_sret(ret, &self.struct_layouts))
                .unwrap_or(false);

        // Add sret pointer as first parameter if needed
        if needs_sret {
            sig.params.push(AbiParam::special(
                self.pointer_type,
                ArgumentPurpose::StructReturn,
            ));
        }

        for param in &func.params {
            if is_runtime {
                // Runtime functions use C ABI: flatten struct parameters
                append_abi_params_flattened(
                    &mut sig,
                    &param.ty,
                    &self.struct_layouts,
                    self.pointer_type,
                )?;
            } else {
                append_abi_params(&mut sig, &param.ty, &self.struct_layouts, self.pointer_type)?;
            }
        }
        if program.main == Some(func.name) {
            sig.returns.push(AbiParam::new(types::I32));
        } else if let Some(ret) = &func.ret {
            if is_runtime {
                append_return_flattened(&mut sig, ret, &self.struct_layouts, self.pointer_type)?;
            } else {
                append_return(&mut sig, ret, &self.struct_layouts, self.pointer_type)?;
            }
        }
        Ok(sig)
    }

    fn generate_function(
        &mut self,
        func: &prim_hir::HirFunction,
        program: &HirProgram,
        ctx: &mut codegen::Context,
        builder_context: &mut FunctionBuilderContext,
    ) -> Result<(), CodegenError> {
        let sig = self.build_signature(func, program)?;
        let func_id = *self
            .func_ids
            .get(&func.id)
            .ok_or(CodegenError::MissingFunction(func.id))?;
        ctx.func.signature = sig;

        // Check if this function uses sret
        let needs_sret = func
            .ret
            .as_ref()
            .map(|ret| uses_sret(ret, &self.struct_layouts))
            .unwrap_or(false);

        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_context);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let abi_params = builder.block_params(entry).to_vec();

        // Extract sret pointer if needed, bind remaining params
        let (sret_ptr, regular_params) = if needs_sret {
            (Some(abi_params[0]), &abi_params[1..])
        } else {
            (None, &abi_params[..])
        };

        let mut locals = VarEnv::new();
        bind_params(&mut builder, &mut locals, regular_params, func)?;

        let mut loop_exits: Vec<Block> = Vec::new();
        let (trailing_val, terminated) = self.lower_block(
            &func.body,
            program,
            func.module,
            &mut builder,
            &mut locals,
            &mut loop_exits,
        )?;

        if !terminated {
            if program.main == Some(func.name) {
                let ret_val = builder.ins().iconst(types::I32, 0);
                builder.ins().return_(&[ret_val]);
            } else if let Some(ret_ty) = &func.ret {
                let out = trailing_val.ok_or(CodegenError::MissingReturnValue)?;

                if let Some(sret) = sret_ptr {
                    // Store return values to sret pointer
                    self.store_struct_to_ptr(&mut builder, sret, ret_ty, &out)?;
                    builder.ins().return_(&[]);
                } else {
                    validate_return_values(
                        &builder,
                        &out,
                        ret_ty,
                        &self.struct_layouts,
                        self.pointer_type,
                    )?;
                    builder.ins().return_(&out);
                }
            } else {
                builder.ins().return_(&[]);
            };
        }
        builder.finalize();

        self.module.define_function(func_id, ctx)?;
        self.module.clear_context(ctx);
        Ok(())
    }

    /// Copy struct from source pointer to destination pointer.
    fn store_struct_to_ptr(
        &self,
        builder: &mut FunctionBuilder,
        dst_ptr: Value,
        ty: &prim_hir::Type,
        values: &[Value],
    ) -> Result<(), CodegenError> {
        let struct_id = ty.as_struct().ok_or(CodegenError::InvalidFieldAccess)?;
        let layout = self
            .struct_layouts
            .get(&struct_id)
            .ok_or(CodegenError::MissingStructLayout(struct_id))?;

        // With pointer-based representation, values[0] is a pointer to the source struct
        let src_ptr = values[0];
        self.copy_struct(builder, src_ptr, dst_ptr, layout);
        Ok(())
    }

    /// Copy a struct from src_ptr to dst_ptr by copying each field.
    fn copy_struct(
        &self,
        builder: &mut FunctionBuilder,
        src_ptr: Value,
        dst_ptr: Value,
        layout: &StructLayout,
    ) {
        for field in &layout.fields {
            let src_field = builder.ins().iadd_imm(src_ptr, field.offset as i64);
            let dst_field = builder.ins().iadd_imm(dst_ptr, field.offset as i64);

            match &field.ty {
                FieldType::Scalar(ty) => {
                    let val = builder.ins().load(*ty, MemFlags::new(), src_field, 0);
                    builder.ins().store(MemFlags::new(), val, dst_field, 0);
                }
                FieldType::Pointer => {
                    let val = builder
                        .ins()
                        .load(self.pointer_type, MemFlags::new(), src_field, 0);
                    builder.ins().store(MemFlags::new(), val, dst_field, 0);
                }
                FieldType::Struct(nested_id) => {
                    if let Some(nested_layout) = self.struct_layouts.get(nested_id) {
                        self.copy_struct(builder, src_field, dst_field, nested_layout);
                    }
                }
                FieldType::Array { .. } => {
                    // TODO: handle arrays
                }
            }
        }
    }

    fn lower_stmt(
        &mut self,
        stmt: &prim_hir::HirStmt,
        program: &HirProgram,
        module_id: prim_hir::ModuleId,
        builder: &mut FunctionBuilder,
        locals: &mut VarEnv,
        loop_exits: &mut Vec<Block>,
    ) -> Result<StmtFlow, CodegenError> {
        match stmt {
            prim_hir::HirStmt::Let { name, value, .. } => {
                let vals = self.lower_expr(value, program, module_id, builder, locals)?;
                locals.declare_local(builder, *name, vals);
                Ok(StmtFlow::Continue)
            }
            prim_hir::HirStmt::Assign { target, value, .. } => {
                let vals = self.lower_expr(value, program, module_id, builder, locals)?;
                locals.assign_local(builder, *target, vals)?;
                Ok(StmtFlow::Continue)
            }
            prim_hir::HirStmt::Expr(expr) => {
                let _vals = self.lower_expr(expr, program, module_id, builder, locals)?;
                Ok(StmtFlow::Value)
            }
            prim_hir::HirStmt::Loop { body, .. } => {
                let body_block = builder.create_block();
                let exit = builder.create_block();
                builder.ins().jump(body_block, &[]);

                builder.switch_to_block(body_block);
                loop_exits.push(exit);
                let mut terminated = false;
                for s in &body.stmts {
                    let flow =
                        self.lower_stmt(s, program, module_id, builder, locals, loop_exits)?;
                    match flow {
                        StmtFlow::Terminated => {
                            terminated = true;
                            break;
                        }
                        StmtFlow::Continue | StmtFlow::Value => {}
                    }
                }
                loop_exits.pop();
                if !terminated {
                    builder.ins().jump(body_block, &[]);
                }
                builder.seal_block(body_block);

                builder.switch_to_block(exit);
                builder.seal_block(exit);
                Ok(StmtFlow::Continue)
            }
            prim_hir::HirStmt::While {
                condition, body, ..
            } => {
                let header_block = builder.create_block();
                let body_block = builder.create_block();
                let exit = builder.create_block();

                // Jump to header to evaluate condition
                builder.ins().jump(header_block, &[]);

                // Header block: evaluate condition and branch
                builder.switch_to_block(header_block);
                let cond = self.lower_expr(condition, program, module_id, builder, locals)?[0];
                builder.ins().brif(cond, body_block, &[], exit, &[]);

                // Body block: execute body statements
                builder.switch_to_block(body_block);
                loop_exits.push(exit);
                let mut terminated = false;
                for s in &body.stmts {
                    let flow =
                        self.lower_stmt(s, program, module_id, builder, locals, loop_exits)?;
                    match flow {
                        StmtFlow::Terminated => {
                            terminated = true;
                            break;
                        }
                        StmtFlow::Continue | StmtFlow::Value => {}
                    }
                }
                loop_exits.pop();
                if !terminated {
                    // Jump back to header to check condition again
                    builder.ins().jump(header_block, &[]);
                }

                // Seal blocks after all predecessors are known
                builder.seal_block(header_block);
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

    /// Lower a HirBlock, returning the trailing expression value if present.
    fn lower_block(
        &mut self,
        block: &prim_hir::HirBlock,
        program: &HirProgram,
        module_id: prim_hir::ModuleId,
        builder: &mut FunctionBuilder,
        locals: &mut VarEnv,
        loop_exits: &mut Vec<Block>,
    ) -> Result<(Option<Vec<Value>>, bool), CodegenError> {
        let mut terminated = false;
        for stmt in &block.stmts {
            let flow = self.lower_stmt(stmt, program, module_id, builder, locals, loop_exits)?;
            match flow {
                StmtFlow::Terminated => {
                    terminated = true;
                    break;
                }
                StmtFlow::Continue | StmtFlow::Value => {}
            }
        }

        if terminated {
            return Ok((None, true));
        }

        // Evaluate trailing expression if present
        if let Some(expr) = &block.expr {
            let vals = self.lower_expr(expr, program, module_id, builder, locals)?;
            Ok((Some(vals), false))
        } else {
            Ok((None, false))
        }
    }

    fn lower_expr(
        &mut self,
        expr: &prim_hir::HirExpr,
        program: &HirProgram,
        module_id: prim_hir::ModuleId,
        builder: &mut FunctionBuilder,
        locals: &mut VarEnv,
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
                let (data_ptr, len) = make_string_data(
                    builder,
                    &mut self.module,
                    program,
                    module_id,
                    *span,
                    value.as_bytes(),
                )?;

                // String literals have type Str (a struct with data and len fields)
                // Allocate a stack slot and store the (data, len) pair
                if let prim_hir::Type::Struct(struct_id) = ty {
                    let layout = self
                        .struct_layouts
                        .get(struct_id)
                        .ok_or(CodegenError::MissingStructLayout(*struct_id))?;

                    let slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        layout.size,
                        layout.align.try_into().unwrap_or(1),
                    ));
                    let str_ptr = builder.ins().stack_addr(self.pointer_type, slot, 0);

                    // Store fields at their computed offsets
                    // Str has: data (field 0) and len (field 1)
                    if layout.fields.len() >= 2 {
                        let data_offset = layout.fields[0].offset as i32;
                        let len_offset = layout.fields[1].offset as i32;
                        builder
                            .ins()
                            .store(MemFlags::new(), data_ptr, str_ptr, data_offset);
                        builder
                            .ins()
                            .store(MemFlags::new(), len, str_ptr, len_offset);
                    }

                    vec![str_ptr]
                } else {
                    // Fallback for untyped string (shouldn't happen)
                    vec![data_ptr, len]
                }
            }
            prim_hir::HirExpr::Ident { symbol, .. } => locals.use_symbol(builder, *symbol)?,
            prim_hir::HirExpr::Binary {
                op, left, right, ..
            } => {
                let l = self.lower_expr(left, program, module_id, builder, locals)?;
                let r = self.lower_expr(right, program, module_id, builder, locals)?;
                let (l, r) = (l[0], r[0]);
                let res = match op {
                    prim_hir::BinaryOp::Add => builder.ins().iadd(l, r),
                    prim_hir::BinaryOp::Subtract => builder.ins().isub(l, r),
                    prim_hir::BinaryOp::Multiply => builder.ins().imul(l, r),
                    prim_hir::BinaryOp::Divide => builder.ins().sdiv(l, r),
                    prim_hir::BinaryOp::Modulo => builder.ins().srem(l, r),
                    prim_hir::BinaryOp::Equals
                    | prim_hir::BinaryOp::NotEquals
                    | prim_hir::BinaryOp::Greater
                    | prim_hir::BinaryOp::GreaterEquals
                    | prim_hir::BinaryOp::Less
                    | prim_hir::BinaryOp::LessEquals => {
                        let cc = match op {
                            prim_hir::BinaryOp::Equals => IntCC::Equal,
                            prim_hir::BinaryOp::NotEquals => IntCC::NotEqual,
                            prim_hir::BinaryOp::Greater => IntCC::SignedGreaterThan,
                            prim_hir::BinaryOp::GreaterEquals => IntCC::SignedGreaterThanOrEqual,
                            prim_hir::BinaryOp::Less => IntCC::SignedLessThan,
                            prim_hir::BinaryOp::LessEquals => IntCC::SignedLessThanOrEqual,
                            _ => unreachable!(),
                        };
                        let cmp = builder.ins().icmp(cc, l, r);
                        let one = builder.ins().iconst(types::I8, 1);
                        let zero = builder.ins().iconst(types::I8, 0);
                        builder.ins().select(cmp, one, zero)
                    }
                };
                vec![res]
            }
            prim_hir::HirExpr::Call { func, args, ty, .. } => {
                let target = *self
                    .func_ids
                    .get(func)
                    .ok_or(CodegenError::MissingFunction(*func))?;
                let callee = self.module.declare_func_in_func(target, builder.func);

                // Check if the callee is a runtime-bound function
                let func_def = program.items.functions.iter().find(|f| f.id == *func);
                let is_runtime_call = func_def.is_some_and(|f| f.runtime_binding.is_some());

                // Runtime functions don't use sret, they return via registers
                let call_uses_sret = !is_runtime_call && uses_sret(ty, &self.struct_layouts);

                let mut lowered_args = Vec::new();

                // If sret, allocate stack slot and prepend pointer
                let sret_slot = if call_uses_sret {
                    let struct_id = ty.as_struct().ok_or(CodegenError::InvalidFieldAccess)?;
                    let layout = self
                        .struct_layouts
                        .get(&struct_id)
                        .ok_or(CodegenError::MissingStructLayout(struct_id))?;

                    let slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        layout.size,
                        layout.align.try_into().unwrap_or(1),
                    ));
                    let sret_ptr = builder.ins().stack_addr(self.pointer_type, slot, 0);
                    lowered_args.push(sret_ptr);
                    Some((slot, layout.clone()))
                } else {
                    None
                };

                // Get arg types from function definition for flattening
                let arg_types: Vec<_> = func_def
                    .map(|f| f.params.iter().map(|p| p.ty.clone()).collect())
                    .unwrap_or_default();

                for (i, a) in args.iter().enumerate() {
                    let vals = self.lower_expr(a, program, module_id, builder, locals)?;

                    // For runtime calls, flatten struct arguments
                    if is_runtime_call {
                        if let Some(prim_hir::Type::Struct(struct_id)) = arg_types.get(i) {
                            // vals[0] is a pointer to the struct; load each field
                            let layout = self
                                .struct_layouts
                                .get(struct_id)
                                .ok_or(CodegenError::MissingStructLayout(*struct_id))?;
                            let struct_ptr = vals[0];
                            for field in &layout.fields {
                                let field_ty = field
                                    .ty
                                    .as_scalar(self.pointer_type)
                                    .ok_or(CodegenError::InvalidStructLane)?;
                                let val = builder.ins().load(
                                    field_ty,
                                    MemFlags::new(),
                                    struct_ptr,
                                    field.offset as i32,
                                );
                                lowered_args.push(val);
                            }
                        } else {
                            lowered_args.extend(vals);
                        }
                    } else {
                        lowered_args.extend(vals);
                    }
                }

                if let Some(params) = self.func_params.get(func) {
                    // func_params already includes sret pointer if function uses sret
                    if lowered_args.len() != params.len() {
                        return Err(CodegenError::ArityMismatch {
                            expected: params.len(),
                            found: lowered_args.len(),
                        });
                    }
                    for (arg, expected_ty) in lowered_args.iter().zip(params.iter()) {
                        let got = builder.func.dfg.value_type(*arg);
                        if got != *expected_ty {
                            return Err(CodegenError::ArgTypeMismatch {
                                expected: *expected_ty,
                                found: got,
                            });
                        }
                    }
                }

                let call = builder.ins().call(callee, &lowered_args);

                if let Some((slot, _layout)) = sret_slot {
                    // Return pointer to struct in sret slot
                    let ptr = builder.ins().stack_addr(self.pointer_type, slot, 0);
                    vec![ptr]
                } else {
                    builder.inst_results(call).to_vec()
                }
            }
            prim_hir::HirExpr::StructLit {
                struct_id, fields, ..
            } => {
                let layout = self
                    .struct_layouts
                    .get(struct_id)
                    .cloned()
                    .ok_or(CodegenError::MissingStructLayout(*struct_id))?;

                // Allocate stack slot for this struct
                let slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    layout.size,
                    layout.align.try_into().unwrap_or(1),
                ));
                let struct_ptr = builder.ins().stack_addr(self.pointer_type, slot, 0);

                // Store each field at its offset
                for (field_sym, expr) in fields {
                    let field_layout =
                        layout
                            .field(*field_sym)
                            .ok_or(CodegenError::MissingStructField {
                                struct_id: *struct_id,
                                field: *field_sym,
                            })?;
                    let vals = self.lower_expr(expr, program, module_id, builder, locals)?;
                    let field_ptr = builder
                        .ins()
                        .iadd_imm(struct_ptr, field_layout.offset as i64);

                    match &field_layout.ty {
                        FieldType::Scalar(_) | FieldType::Pointer => {
                            let v = vals[0];
                            builder.ins().store(MemFlags::new(), v, field_ptr, 0);
                        }
                        FieldType::Struct(nested_id) => {
                            // Copy nested struct from source ptr to field location
                            let src_ptr = vals[0];
                            let nested_layout = self
                                .struct_layouts
                                .get(nested_id)
                                .ok_or(CodegenError::MissingStructLayout(*nested_id))?;
                            self.copy_struct(builder, src_ptr, field_ptr, nested_layout);
                        }
                        FieldType::Array { .. } => {
                            // TODO: handle arrays
                            return Err(CodegenError::InvalidArrayLane);
                        }
                    }
                }

                vec![struct_ptr]
            }
            prim_hir::HirExpr::Field {
                base, field, ty, ..
            } => {
                let base_vals = self.lower_expr(base, program, module_id, builder, locals)?;
                let struct_ptr = base_vals[0];
                let struct_id = base
                    .ty()
                    .as_struct()
                    .ok_or(CodegenError::InvalidFieldAccess)?;
                let layout = self
                    .struct_layouts
                    .get(&struct_id)
                    .ok_or(CodegenError::MissingStructLayout(struct_id))?;
                let field_layout =
                    layout
                        .field(*field)
                        .ok_or(CodegenError::MissingStructField {
                            struct_id,
                            field: *field,
                        })?;

                let field_ptr = builder
                    .ins()
                    .iadd_imm(struct_ptr, field_layout.offset as i64);

                match ty {
                    prim_hir::Type::Struct(_) => {
                        // For nested struct, return pointer to the embedded data
                        vec![field_ptr]
                    }
                    _ => {
                        // For scalars/pointers, load the value
                        let field_ty = field_layout
                            .ty
                            .as_scalar(self.pointer_type)
                            .ok_or(CodegenError::InvalidStructLane)?;
                        let val = builder.ins().load(field_ty, MemFlags::new(), field_ptr, 0);
                        vec![val]
                    }
                }
            }
            prim_hir::HirExpr::Deref { base, .. } => {
                let ptr = self.lower_expr(base, program, module_id, builder, locals)?[0];
                let pointee = match base.ty() {
                    prim_hir::Type::Pointer { pointee, .. } => pointee.as_ref(),
                    _ => return Err(CodegenError::InvalidDereference),
                };
                let lane = match pointee {
                    prim_hir::Type::Pointer { .. } => self.module.isa().pointer_type(),
                    _ => {
                        let (scalar, _) = scalar_lane(pointee)?;
                        scalar
                    }
                };
                let loaded = builder.ins().load(lane, MemFlags::new(), ptr, 0);
                vec![loaded]
            }
            prim_hir::HirExpr::ArrayLit { elements, .. } => {
                if elements.is_empty() {
                    vec![builder.ins().iconst(types::I64, 0)]
                } else {
                    // For now, return the first element value as a stand-in.
                    self.lower_expr(&elements[0], program, module_id, builder, locals)?
                }
            }
            prim_hir::HirExpr::If {
                condition,
                then_branch,
                else_branch,
                ty,
                ..
            } => {
                let cond = self.lower_expr(condition, program, module_id, builder, locals)?;
                let cond_val = cond[0];

                let then_block = builder.create_block();
                let merge_block = builder.create_block();
                let else_block = if else_branch.is_some() {
                    builder.create_block()
                } else {
                    merge_block
                };

                builder
                    .ins()
                    .brif(cond_val, then_block, &[], else_block, &[]);

                // Determine if we need phi values (if-expression with value)
                let needs_value = !matches!(ty, prim_hir::Type::Undetermined)
                    && (then_branch.expr.is_some()
                        || else_branch.as_ref().is_some_and(|b| b.expr.is_some()));

                // Then block
                builder.switch_to_block(then_block);
                builder.seal_block(then_block);

                let mut loop_exits: Vec<Block> = Vec::new();
                let (then_vals, then_terminated) = self.lower_block(
                    then_branch,
                    program,
                    module_id,
                    builder,
                    locals,
                    &mut loop_exits,
                )?;

                // Add block params for phi values BEFORE doing jumps
                // We need to figure out the return types first
                let param_types: Vec<cranelift::prelude::Type> = if needs_value {
                    match ty {
                        // Structs are pointer-based: phi value is a pointer
                        prim_hir::Type::Struct(_) => vec![self.pointer_type],
                        prim_hir::Type::Pointer { .. } => vec![self.pointer_type],
                        _ => {
                            let (lane, _) = scalar_lane(ty)?;
                            vec![lane]
                        }
                    }
                } else {
                    vec![]
                };

                // Add block params for merge block
                for param_ty in &param_types {
                    builder.append_block_param(merge_block, *param_ty);
                }

                // Helper to emit jump with optional block args
                let emit_branch_jump = |builder: &mut FunctionBuilder,
                                        vals: &Option<Vec<Value>>,
                                        needs_value: bool,
                                        target: Block| {
                    let args: Vec<BlockArg> = if needs_value {
                        vals.as_ref()
                            .map(|v| v.iter().map(|val| BlockArg::Value(*val)).collect())
                            .unwrap_or_default()
                    } else {
                        vec![]
                    };
                    builder.ins().jump(target, &args);
                };

                if !then_terminated {
                    emit_branch_jump(builder, &then_vals, needs_value, merge_block);
                }

                // Else block (if present)
                if let Some(else_body) = else_branch {
                    builder.switch_to_block(else_block);
                    builder.seal_block(else_block);

                    let (else_vals, else_terminated) = self.lower_block(
                        else_body,
                        program,
                        module_id,
                        builder,
                        locals,
                        &mut loop_exits,
                    )?;

                    if !else_terminated {
                        emit_branch_jump(builder, &else_vals, needs_value, merge_block);
                    }
                }

                builder.switch_to_block(merge_block);
                builder.seal_block(merge_block);

                if needs_value && !param_types.is_empty() {
                    builder.block_params(merge_block).to_vec()
                } else {
                    vec![]
                }
            }
            prim_hir::HirExpr::Block { block, .. } => {
                let mut loop_exits: Vec<Block> = Vec::new();
                let (vals, _terminated) =
                    self.lower_block(block, program, module_id, builder, locals, &mut loop_exits)?;
                vals.unwrap_or_default()
            }
        })
    }
}

fn scalar_lane(ty: &prim_hir::Type) -> Result<(cranelift::prelude::Type, u32), CodegenError> {
    use prim_hir::Type::*;
    let lane = match ty {
        Bool | I8 | U8 => types::I8,
        I16 | U16 => types::I16,
        I32 | U32 => types::I32,
        I64 | U64 | Isize | Usize => types::I64,
        F32 => types::F32,
        F64 => types::F64,
        Pointer { .. } => return Err(CodegenError::InvalidPointerLane),
        Struct(_) => return Err(CodegenError::InvalidStructLane),
        Array(_) => return Err(CodegenError::InvalidArrayLane),
        IntVar | FloatVar | Undetermined => return Err(CodegenError::UndeterminedType),
    };
    let size = lane.bytes();
    Ok((lane, size))
}

fn append_abi_params(
    sig: &mut Signature,
    ty: &prim_hir::Type,
    _layouts: &HashMap<prim_hir::StructId, StructLayout>,
    pointer_type: cranelift::prelude::Type,
) -> Result<(), CodegenError> {
    match ty {
        // Structs are passed as pointers
        prim_hir::Type::Struct(_) => sig.params.push(AbiParam::new(pointer_type)),
        prim_hir::Type::Pointer { .. } => sig.params.push(AbiParam::new(pointer_type)),
        prim_hir::Type::Array(_) => sig.params.push(AbiParam::new(pointer_type)),
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.params.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

/// Check if a return type requires the sret (struct return) convention.
/// All structs use sret - they are returned via pointer.
fn uses_sret(ty: &prim_hir::Type, _layouts: &HashMap<prim_hir::StructId, StructLayout>) -> bool {
    matches!(ty, prim_hir::Type::Struct(_))
}

fn append_return(
    sig: &mut Signature,
    ty: &prim_hir::Type,
    _layouts: &HashMap<prim_hir::StructId, StructLayout>,
    pointer_type: cranelift::prelude::Type,
) -> Result<(), CodegenError> {
    match ty {
        // Structs use sret - no return values in signature
        prim_hir::Type::Struct(_) => {}
        prim_hir::Type::Pointer { .. } => sig.returns.push(AbiParam::new(pointer_type)),
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.returns.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

/// Append ABI parameters with structs flattened (for C ABI / runtime functions).
fn append_abi_params_flattened(
    sig: &mut Signature,
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    pointer_type: cranelift::prelude::Type,
) -> Result<(), CodegenError> {
    match ty {
        prim_hir::Type::Struct(id) => {
            // Flatten struct fields as separate parameters
            let layout = layouts
                .get(id)
                .ok_or(CodegenError::MissingStructLayout(*id))?;
            for field in &layout.fields {
                let field_ty = field
                    .ty
                    .as_scalar(pointer_type)
                    .ok_or(CodegenError::InvalidStructLane)?;
                sig.params.push(AbiParam::new(field_ty));
            }
        }
        prim_hir::Type::Pointer { .. } => sig.params.push(AbiParam::new(pointer_type)),
        prim_hir::Type::Array(_) => sig.params.push(AbiParam::new(pointer_type)),
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.params.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

/// Append return type with structs flattened (for C ABI / runtime functions).
fn append_return_flattened(
    sig: &mut Signature,
    ty: &prim_hir::Type,
    layouts: &HashMap<prim_hir::StructId, StructLayout>,
    pointer_type: cranelift::prelude::Type,
) -> Result<(), CodegenError> {
    match ty {
        prim_hir::Type::Struct(id) => {
            // Flatten struct fields as separate return values
            let layout = layouts
                .get(id)
                .ok_or(CodegenError::MissingStructLayout(*id))?;
            for field in &layout.fields {
                let field_ty = field
                    .ty
                    .as_scalar(pointer_type)
                    .ok_or(CodegenError::InvalidStructLane)?;
                sig.returns.push(AbiParam::new(field_ty));
            }
        }
        prim_hir::Type::Pointer { .. } => sig.returns.push(AbiParam::new(pointer_type)),
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            sig.returns.push(AbiParam::new(lane));
        }
    }
    Ok(())
}

fn export_symbol(
    func: &prim_hir::HirFunction,
    program: &HirProgram,
) -> Result<String, CodegenError> {
    if let Some(binding) = &func.runtime_binding {
        return Ok(binding.clone());
    }
    if program.main == Some(func.name) {
        return Ok("prim_main".to_string());
    }
    Ok(symbol_name(func.name, program))
}

fn abi_slot_count(ty: &prim_hir::Type) -> usize {
    // All types are passed as single values (scalars directly, structs as pointers)
    match ty {
        prim_hir::Type::Struct(_) => 1, // Pointer to struct
        _ => 1,
    }
}

fn bind_params(
    builder: &mut FunctionBuilder,
    locals: &mut VarEnv,
    abi_params: &[Value],
    func: &prim_hir::HirFunction,
) -> Result<(), CodegenError> {
    let mut idx = 0usize;
    for param in &func.params {
        let slots = abi_slot_count(&param.ty);
        let slice = abi_params
            .get(idx..idx + slots)
            .expect("missing ABI parameter slots");
        locals.declare_local(builder, param.name, slice.to_vec());
        idx += slots;
    }
    Ok(())
}

enum StmtFlow {
    Continue,
    Value,
    Terminated,
}

fn make_string_data(
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
    let ptr = builder.ins().global_value(module.isa().pointer_type(), gv);
    let len = builder.ins().iconst(types::I64, bytes.len() as i64);
    Ok((ptr, len))
}

struct VarEnv {
    /// Maps symbol to a list of Cranelift Variables (one per slot for structs).
    locals: HashMap<prim_hir::SymbolId, Vec<Variable>>,
}

impl VarEnv {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    /// Declare and initialize a new local variable binding.
    fn declare_local(
        &mut self,
        builder: &mut FunctionBuilder,
        sym: prim_hir::SymbolId,
        values: Vec<Value>,
    ) {
        let mut vars = Vec::with_capacity(values.len());
        for val in values {
            let ty = builder.func.dfg.value_type(val);
            let var = builder.declare_var(ty);
            builder.def_var(var, val);
            vars.push(var);
        }
        self.locals.insert(sym, vars);
    }

    /// Assign new values to an existing local variable.
    fn assign_local(
        &mut self,
        builder: &mut FunctionBuilder,
        sym: prim_hir::SymbolId,
        values: Vec<Value>,
    ) -> Result<(), CodegenError> {
        let vars = self
            .locals
            .get(&sym)
            .ok_or(CodegenError::UndefinedLocal(sym))?;
        if vars.len() != values.len() {
            return Err(CodegenError::UndefinedLocal(sym));
        }
        for (var, val) in vars.iter().zip(values.iter()) {
            builder.def_var(*var, *val);
        }
        Ok(())
    }

    /// Read the current values of a local variable.
    fn use_symbol(
        &self,
        builder: &mut FunctionBuilder,
        sym: prim_hir::SymbolId,
    ) -> Result<Vec<Value>, CodegenError> {
        let vars = self
            .locals
            .get(&sym)
            .ok_or(CodegenError::UndefinedLocal(sym))?;
        Ok(vars.iter().map(|v| builder.use_var(*v)).collect())
    }
}

/// Entry point used by prim-cli.
pub fn generate_object_code(hir: &HirProgram) -> Result<Vec<u8>, CodegenError> {
    if hir.main.is_none() {
        return Err(CodegenError::MissingMain);
    }
    let codegen = CraneliftCodeGenerator::new()?;
    codegen.generate(hir)
}

/// Align a value up to the next multiple of `align`.
fn align_to(value: u32, align: u32) -> u32 {
    if align == 0 {
        return value;
    }
    (value + align - 1) & !(align - 1)
}

/// Convert a HIR type to a FieldType for layout computation.
fn hir_type_to_field_type(ty: &prim_hir::Type) -> Result<FieldType, CodegenError> {
    match ty {
        prim_hir::Type::Pointer { .. } => Ok(FieldType::Pointer),
        prim_hir::Type::Struct(id) => Ok(FieldType::Struct(*id)),
        prim_hir::Type::Array(elem) => {
            let elem_type = hir_type_to_field_type(elem)?;
            // For now, arrays don't have a known length at this stage
            // This will need refinement when array lengths are tracked
            Ok(FieldType::Array {
                elem: Box::new(elem_type),
                len: 0,
            })
        }
        _ => {
            let (lane, _) = scalar_lane(ty)?;
            Ok(FieldType::Scalar(lane))
        }
    }
}
