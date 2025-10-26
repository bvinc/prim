use prim_parse::{BinaryOp, Expr, Function, Program, Stmt, StructDefinition, Type};
use std::collections::HashMap;

use cranelift::prelude::*;
// use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

mod error;
pub use error::CodegenError;

// Small value representation supporting scalars and aggregates (e.g., StrSlice ptr+len)
#[derive(Clone)]
enum Val {
    One(Value),
    Many(Vec<Value>),
}

#[derive(Clone)]
enum Var {
    One(Variable),
    Many(Vec<Variable>),
}

pub struct CraneliftCodeGenerator {
    module: ObjectModule,
    ctx: codegen::Context,
    builder_context: FunctionBuilderContext,
    struct_layouts: HashMap<String, StructLayout>,
}

#[derive(Clone, Debug)]
struct StructLayout {
    fields: Vec<FieldLayout>,
    total_size: u32,
}

#[derive(Clone, Debug)]
struct FieldLayout {
    name: String,
    offset: u32,
    size: u32,
    cranelift_type: cranelift::prelude::Type,
}

#[derive(Clone, Copy)]
#[allow(dead_code)]
struct LoopContext {
    header: Block,
    exit: Block,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BlockStatus {
    Reachable,
    Terminated,
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
        })
    }

    pub fn generate(mut self, program: &Program, source: &str) -> Result<Vec<u8>, CodegenError> {
        // Create println function first
        let println_func_id = self.create_println_function()?;

        // Compute struct layouts
        self.compute_struct_layouts(&program.structs, source)?;

        // First pass: declare all functions (including imports via @runtime)
        let mut function_ids = HashMap::new();
        for function in &program.functions {
            let sig = self.create_function_signature(function, source);
            let linkage = self.determine_linkage(function, source);

            // Export Prim's `main` under the symbol name `prim_main`.
            let sym_name = if let Some(binding) = &function.runtime_binding {
                binding.clone()
            } else if function.name.text(source) == "main" {
                "prim_main".to_string()
            } else {
                function.name.text(source).to_string()
            };

            let func_id = self.module.declare_function(&sym_name, linkage, &sig)?;

            function_ids.insert(function.name.text(source).to_string(), func_id);
        }

        // Second pass: generate function bodies
        for function in &program.functions {
            // Do not generate bodies for runtime-bound imports
            if function.runtime_binding.is_some() {
                continue;
            }
            self.generate_function(function, println_func_id, &function_ids, source)?;
        }

        // Finalize the object
        let product = self.module.finish();
        Ok(product.emit()?)
    }

    fn scalar_lane(ty: &Type) -> (cranelift::prelude::Type, i64) {
        match ty {
            Type::Bool | Type::I8 | Type::U8 => (types::I8, 1),
            Type::I16 | Type::U16 => (types::I16, 2),
            Type::I32 | Type::U32 => (types::I32, 4),
            Type::I64 | Type::U64 | Type::Isize | Type::Usize => (types::I64, 8),
            Type::Pointer { .. } | Type::Struct(_) | Type::Array(_) => (types::I64, 8),
            Type::F32 => (types::F32, 4),
            Type::F64 => (types::F64, 8),
            Type::StrSlice => (types::I64, 8),
            Type::Undetermined => {
                panic!("Type checker must resolve all expression types before code generation")
            }
        }
    }

    fn zero_value(builder: &mut FunctionBuilder, lane: cranelift::prelude::Type) -> Value {
        if lane.is_int() {
            builder.ins().iconst(lane, 0)
        } else if lane == types::F32 {
            builder.ins().f32const(Ieee32::with_bits(0))
        } else if lane == types::F64 {
            builder.ins().f64const(Ieee64::with_bits(0))
        } else {
            builder.ins().iconst(lane, 0)
        }
    }

    fn ensure_value_type(
        builder: &mut FunctionBuilder,
        value: Value,
        target: cranelift::prelude::Type,
    ) -> Value {
        let current = builder.func.dfg.value_type(value);
        if current == target {
            value
        } else if current.is_int() && target.is_int() {
            if current.bits() > target.bits() {
                builder.ins().ireduce(target, value)
            } else if current.bits() < target.bits() {
                builder.ins().uextend(target, value)
            } else {
                value
            }
        } else {
            value
        }
    }

    fn generate_function(
        &mut self,
        function: &Function,
        println_func_id: cranelift_module::FuncId,
        function_ids: &HashMap<String, cranelift_module::FuncId>,
        source: &str,
    ) -> Result<(), CodegenError> {
        let sig = self.create_function_signature(function, source);
        let func_id = function_ids[function.name.text(source)];

        self.generate_function_body(function, println_func_id, function_ids, sig, source)?;

        // Define the function
        self.module.define_function(func_id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);

        Ok(())
    }

    fn create_function_signature(&mut self, function: &Function, source: &str) -> Signature {
        let mut sig = self.module.make_signature();

        // Add parameters to signature (flatten aggregates)
        for param in &function.parameters {
            match &param.type_annotation {
                prim_parse::Type::StrSlice => {
                    sig.params.push(AbiParam::new(types::I64));
                    sig.params.push(AbiParam::new(types::I64));
                }
                prim_parse::Type::Struct(name_span) => {
                    let name = name_span.text(source).to_string();
                    if let Some(layout) = self.struct_layouts.get(&name) {
                        for fld in &layout.fields {
                            sig.params.push(AbiParam::new(fld.cranelift_type));
                        }
                    } else {
                        sig.params.push(AbiParam::new(types::I64));
                    }
                }
                prim_parse::Type::Array(_) => sig.params.push(AbiParam::new(types::I64)),
                _ => {
                    let (lane, _) = Self::scalar_lane(&param.type_annotation);
                    sig.params.push(AbiParam::new(lane));
                }
            }
        }

        // Add return type to signature
        if function.name.text(source) == "main" {
            sig.returns.push(AbiParam::new(types::I32)); // main returns int
        } else if let Some(return_type) = &function.return_type {
            let (lane, _) = Self::scalar_lane(return_type);
            sig.returns.push(AbiParam::new(lane));
        }

        sig
    }

    fn determine_linkage(&self, function: &Function, source: &str) -> Linkage {
        if function.runtime_binding.is_some() {
            Linkage::Import
        } else if function.name.text(source) == "main" {
            Linkage::Export
        } else {
            Linkage::Local
        }
    }

    fn generate_function_body(
        &mut self,
        function: &Function,
        println_func_id: cranelift_module::FuncId,
        function_ids: &HashMap<String, cranelift_module::FuncId>,
        sig: Signature,
        source: &str,
    ) -> Result<(), CodegenError> {
        self.ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let (variables, _) = Self::setup_function_parameters(
            &self.struct_layouts,
            &mut builder,
            entry_block,
            function,
            source,
        );

        let mut variables = variables;
        let mut last_expr_value = None;
        let mut loop_stack = Vec::new();

        for stmt in &function.body {
            match stmt {
                Stmt::Expr(expr) => {
                    last_expr_value = Some(Self::generate_expression_impl_static(
                        &self.struct_layouts,
                        &variables,
                        &mut self.module,
                        &mut builder,
                        expr,
                        println_func_id,
                        function_ids,
                        source,
                    )?);
                }
                _ => {
                    let _ = Self::generate_statement_impl_static(
                        &self.struct_layouts,
                        &mut variables,
                        &mut self.module,
                        &mut builder,
                        stmt,
                        println_func_id,
                        function_ids,
                        source,
                        &mut loop_stack,
                    )?;
                    last_expr_value = None;
                }
            }
        }
        Self::generate_function_return(&mut builder, function, last_expr_value, source)?;
        builder.seal_all_blocks();
        builder.finalize();
        Ok(())
    }

    fn setup_function_parameters(
        struct_layouts: &HashMap<String, StructLayout>,
        builder: &mut FunctionBuilder,
        entry_block: Block,
        function: &Function,
        source: &str,
    ) -> (HashMap<String, Var>, usize) {
        let mut variables: HashMap<String, Var> = HashMap::new();

        // Add parameters to variables
        let block_params: Vec<Value> = builder.block_params(entry_block).to_vec();
        let mut i = 0usize;
        for param in &function.parameters {
            let name = param.name.text(source).to_string();
            match &param.type_annotation {
                prim_parse::Type::StrSlice => {
                    let a = builder.declare_var(types::I64);
                    let b = builder.declare_var(types::I64);
                    builder.def_var(a, block_params[i]);
                    builder.def_var(b, block_params[i + 1]);
                    variables.insert(name, Var::Many(vec![a, b]));
                    i += 2;
                }
                prim_parse::Type::Pointer { .. } => {
                    let (lane, _) = Self::scalar_lane(&param.type_annotation);
                    let var = builder.declare_var(lane);
                    builder.def_var(var, block_params[i]);
                    variables.insert(name, Var::One(var));
                    i += 1;
                }
                prim_parse::Type::Struct(name_span) => {
                    let mut vs = Vec::new();
                    let struct_name = name_span.text(source).to_string();
                    if let Some(layout) = struct_layouts.get(&struct_name) {
                        for fld in &layout.fields {
                            let v = builder.declare_var(fld.cranelift_type);
                            builder.def_var(v, block_params[i]);
                            i += 1;
                            vs.push(v);
                        }
                    }
                    if vs.is_empty() {
                        let v = builder.declare_var(types::I64);
                        builder.def_var(v, block_params[i]);
                        i += 1;
                        variables.insert(name, Var::One(v));
                    } else {
                        variables.insert(name, Var::Many(vs));
                    }
                }
                prim_parse::Type::Array(_) => {
                    let (lane, _) = Self::scalar_lane(&param.type_annotation);
                    let var = builder.declare_var(lane);
                    builder.def_var(var, block_params[i]);
                    variables.insert(name, Var::One(var));
                    i += 1;
                }
                _ => {
                    let (lane, _) = Self::scalar_lane(&param.type_annotation);
                    let var = builder.declare_var(lane);
                    builder.def_var(var, block_params[i]);
                    variables.insert(name, Var::One(var));
                    i += 1;
                }
            }
        }

        (variables, 0)
    }

    fn generate_function_return(
        builder: &mut FunctionBuilder,
        function: &Function,
        last_expr_value: Option<Val>,
        source: &str,
    ) -> Result<(), CodegenError> {
        if function.name.text(source) == "main" {
            // Main function returns 0
            let zero = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[zero]);
        } else if function.return_type.is_some() {
            // Function has return type, return the last expression or default value
            if let Some(return_val) = last_expr_value {
                match return_val {
                    Val::One(v) => {
                        let expected_lane = function
                            .return_type
                            .as_ref()
                            .map(|ty| Self::scalar_lane(ty).0)
                            .unwrap_or(types::I64);
                        let coerced = Self::ensure_value_type(builder, v, expected_lane);
                        builder.ins().return_(&[coerced]);
                    }
                    Val::Many(_) => {
                        return Err(CodegenError::InvalidExpression {
                            message: "Returning aggregates not yet supported".to_string(),
                            context: "function return".to_string(),
                        });
                    }
                }
            } else {
                // Return a default value if no expression
                let expected_lane = function
                    .return_type
                    .as_ref()
                    .map(|ty| Self::scalar_lane(ty).0)
                    .unwrap_or(types::I64);
                let zero = Self::zero_value(builder, expected_lane);
                builder.ins().return_(&[zero]);
            }
        } else {
            // Other functions return void
            builder.ins().return_(&[]);
        }
        Ok(())
    }

    fn generate_statement_impl_static(
        struct_layouts: &HashMap<String, StructLayout>,
        variables: &mut HashMap<String, Var>,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
        stmt: &Stmt,
        println_func_id: cranelift_module::FuncId,
        function_ids: &HashMap<String, cranelift_module::FuncId>,
        source: &str,
        loop_stack: &mut Vec<LoopContext>,
    ) -> Result<BlockStatus, CodegenError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation: _,
                value,
            } => {
                // Generate expression first
                let val = Self::generate_expression_impl_static(
                    struct_layouts,
                    variables,
                    module,
                    builder,
                    value,
                    println_func_id,
                    function_ids,
                    source,
                )?;
                match val {
                    Val::One(v) => {
                        let (lane, _) = Self::scalar_lane(value.resolved_type());
                        let coerced = Self::ensure_value_type(builder, v, lane);
                        let var = builder.declare_var(lane);
                        builder.def_var(var, coerced);
                        variables.insert(name.text(source).to_string(), Var::One(var));
                    }
                    Val::Many(vals) => {
                        let mut agg = Vec::with_capacity(vals.len());
                        for v in vals {
                            let var = builder.declare_var(types::I64);
                            builder.def_var(var, v);
                            agg.push(var);
                        }
                        variables.insert(name.text(source).to_string(), Var::Many(agg));
                    }
                }

                Ok(BlockStatus::Reachable)
            }
            Stmt::Expr(expr) => {
                Self::generate_expression_impl_static(
                    struct_layouts,
                    variables,
                    module,
                    builder,
                    expr,
                    println_func_id,
                    function_ids,
                    source,
                )?;
                Ok(BlockStatus::Reachable)
            }
            Stmt::Loop { body, .. } => {
                let loop_header = builder.create_block();
                let loop_body = builder.create_block();
                let loop_exit = builder.create_block();

                builder.ins().jump(loop_header, &[]);

                builder.switch_to_block(loop_header);
                builder.ins().jump(loop_body, &[]);

                builder.switch_to_block(loop_body);
                loop_stack.push(LoopContext {
                    header: loop_header,
                    exit: loop_exit,
                });

                let mut body_status = BlockStatus::Reachable;
                for stmt in body {
                    body_status = Self::generate_statement_impl_static(
                        struct_layouts,
                        variables,
                        module,
                        builder,
                        stmt,
                        println_func_id,
                        function_ids,
                        source,
                        loop_stack,
                    )?;
                    if matches!(body_status, BlockStatus::Terminated) {
                        break;
                    }
                }

                loop_stack.pop();

                if matches!(body_status, BlockStatus::Reachable) {
                    let resume_block = builder
                        .current_block()
                        .expect("loop body should have an active block");
                    builder.ins().jump(loop_header, &[]);
                    builder.seal_block(resume_block);
                }
                builder.seal_block(loop_header);
                builder.switch_to_block(loop_exit);

                Ok(BlockStatus::Reachable)
            }
            Stmt::Break { span } => {
                let ctx = loop_stack
                    .last()
                    .ok_or_else(|| CodegenError::InvalidExpression {
                        message: format!(
                            "'break' used outside of a loop near byte {}",
                            span.start()
                        ),
                        context: "loop lowering".to_string(),
                    })?;
                builder.ins().jump(ctx.exit, &[]);
                if let Some(active) = builder.current_block() {
                    builder.seal_block(active);
                }
                Ok(BlockStatus::Terminated)
            }
        }
    }

    fn generate_expression_impl_static(
        struct_layouts: &HashMap<String, StructLayout>,
        variables: &HashMap<String, Var>,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        println_func_id: cranelift_module::FuncId,
        function_ids: &HashMap<String, cranelift_module::FuncId>,
        source: &str,
    ) -> Result<Val, CodegenError> {
        match expr {
            Expr::ArrayLiteral { elements, ty } => {
                // Only implement for byte arrays for now: [u8]
                // Determine element type
                let elem_ty = match ty {
                    Type::Array(inner) => inner.as_ref(),
                    _ => &Type::I64,
                };

                // Local helper mapping
                let (elem_size, elem_cl_ty) = match elem_ty {
                    Type::U8 | Type::I8 | Type::Bool => (1u32, types::I8),
                    Type::U16 | Type::I16 => (2u32, types::I16),
                    Type::U32 | Type::I32 => (4u32, types::I32),
                    _ => (8u32, types::I64),
                };

                // Compute total size and alignment
                let count = elements.len() as i64;
                let total_size = (elem_size as i64) * count;
                let align = elem_size.max(1) as i64;

                // Declare extern runtime alloc: prim_rt_alloc(size: usize, align: usize) -> *mut u8
                let mut alloc_sig = module.make_signature();
                alloc_sig.params.push(AbiParam::new(types::I64));
                alloc_sig.params.push(AbiParam::new(types::I64));
                alloc_sig.returns.push(AbiParam::new(types::I64));
                let alloc_func_id =
                    module.declare_function("prim_rt_alloc", Linkage::Import, &alloc_sig)?;
                let alloc_local = module.declare_func_in_func(alloc_func_id, builder.func);

                // Allocate data buffer (or null if empty)
                let base_ptr = if total_size > 0 {
                    let size_val = builder.ins().iconst(types::I64, total_size);
                    let align_val = builder.ins().iconst(types::I64, align);
                    let call = builder.ins().call(alloc_local, &[size_val, align_val]);
                    let results = builder.inst_results(call);
                    results[0]
                } else {
                    builder.ins().iconst(types::I64, 0)
                };

                // Store elements sequentially
                for (i, el) in elements.iter().enumerate() {
                    let val = Self::generate_expression_impl_static(
                        struct_layouts,
                        variables,
                        module,
                        builder,
                        el,
                        println_func_id,
                        function_ids,
                        source,
                    )?;
                    let v = match val {
                        Val::One(v) => v,
                        Val::Many(_) => {
                            return Err(CodegenError::InvalidExpression {
                                message: "array elements must be scalars".to_string(),
                                context: "array literal".to_string(),
                            });
                        }
                    };
                    // Convert to element CL type if needed
                    let store_val = if builder.func.dfg.value_type(v) == elem_cl_ty {
                        v
                    } else {
                        // Narrow or extend to match
                        match elem_cl_ty {
                            types::I8 => builder.ins().ireduce(types::I8, v),
                            types::I16 => builder.ins().ireduce(types::I16, v),
                            types::I32 => builder.ins().ireduce(types::I32, v),
                            types::I64 => v,
                            _ => v,
                        }
                    };
                    let offset = (i as i64) * (elem_size as i64);
                    let off_val = builder.ins().iconst(types::I64, offset);
                    let addr = builder.ins().iadd(base_ptr, off_val);
                    builder.ins().store(MemFlags::new(), store_val, addr, 0);
                }

                // Build header { ptr, len, cap } on the stack and return pointer to it
                let header_size = 8 * 3; // 24 bytes (ptr, len, cap)
                let slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    header_size as u32,
                    3, // 8-byte alignment
                ));
                let header_ptr = builder.ins().stack_addr(types::I64, slot, 0);

                let len_val = builder.ins().iconst(types::I64, count);
                let cap_val = builder.ins().iconst(types::I64, count); // cap == len for literal

                // Store ptr at offset 0
                builder
                    .ins()
                    .store(MemFlags::new(), base_ptr, header_ptr, 0);
                // Store len at offset 8
                let len_off = builder.ins().iconst(types::I64, 8);
                let len_addr = builder.ins().iadd(header_ptr, len_off);
                builder.ins().store(MemFlags::new(), len_val, len_addr, 0);
                // Store cap at offset 16
                let cap_off = builder.ins().iconst(types::I64, 16);
                let cap_addr = builder.ins().iadd(header_ptr, cap_off);
                builder.ins().store(MemFlags::new(), cap_val, cap_addr, 0);

                Ok(Val::One(header_ptr))
            }
            Expr::IntLiteral { span: value, ty } => {
                // Parse the literal (handle type suffixes like 42u32)
                let value_text = value.text(source);
                let num_part = value_text
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect::<String>();
                let num: i64 = num_part
                    .parse()
                    .map_err(|_| CodegenError::InvalidExpression {
                        message: format!("Invalid integer literal: {}", value_text),
                        context: "number parsing".to_string(),
                    })?;
                let (lane, _) = Self::scalar_lane(ty);
                Ok(Val::One(builder.ins().iconst(lane, num)))
            }
            Expr::FloatLiteral { span: value, .. } => Err(CodegenError::InvalidExpression {
                message: format!(
                    "Float literals are not yet supported: {}",
                    value.text(source)
                ),
                context: "float literal evaluation".to_string(),
            }),
            Expr::BoolLiteral { value, .. } => {
                let bool_val = if *value { 1 } else { 0 };
                Ok(Val::One(builder.ins().iconst(types::I8, bool_val)))
            }
            Expr::Identifier { span: name, .. } => {
                let name_text = name.text(source);
                if let Some(var) = variables.get(name_text) {
                    Ok(match var {
                        Var::One(v) => Val::One(builder.use_var(*v)),
                        Var::Many(vs) => {
                            let mut outs = Vec::with_capacity(vs.len());
                            for vv in vs {
                                outs.push(builder.use_var(*vv));
                            }
                            Val::Many(outs)
                        }
                    })
                } else {
                    Err(CodegenError::UndefinedVariable {
                        name: name_text.to_string(),
                        context: "expression evaluation".to_string(),
                    })
                }
            }
            Expr::StringLiteral { span, .. } => {
                // Parse and materialize string bytes as a private data object.
                let raw = span.text(source);
                let bytes = Self::unescape_string_literal(raw).map_err(|e| {
                    CodegenError::InvalidExpression {
                        message: format!("Invalid string literal: {}", e),
                        context: "string literal".to_string(),
                    }
                })?;

                // Unique symbol by source span to avoid collisions; content dedup can come later.
                let sym_name = format!("strlit_{}_{}", span.start(), span.end());
                let data_id = module.declare_data(&sym_name, Linkage::Local, true, false)?;
                let mut desc = cranelift_module::DataDescription::new();
                desc.define(bytes.clone().into_boxed_slice());
                module.define_data(data_id, &desc)?;

                // Generate address of the data and the length constant.
                let gv = module.declare_data_in_func(data_id, builder.func);
                let ptr = builder.ins().global_value(types::I64, gv);
                let len = builder.ins().iconst(types::I64, bytes.len() as i64);
                Ok(Val::Many(vec![ptr, len]))
            }
            Expr::Binary {
                left, op, right, ..
            } => {
                let left_val = Self::generate_expression_impl_static(
                    struct_layouts,
                    variables,
                    module,
                    builder,
                    left,
                    println_func_id,
                    function_ids,
                    source,
                )?;
                let right_val = Self::generate_expression_impl_static(
                    struct_layouts,
                    variables,
                    module,
                    builder,
                    right,
                    println_func_id,
                    function_ids,
                    source,
                )?;

                let (l, r) = match (left_val, right_val) {
                    (Val::One(l), Val::One(r)) => (l, r),
                    _ => {
                        return Err(CodegenError::InvalidExpression {
                            message: "binary ops only support scalar operands".to_string(),
                            context: "binary op".to_string(),
                        });
                    }
                };
                let result = match op {
                    BinaryOp::Add => builder.ins().iadd(l, r),
                    BinaryOp::Subtract => builder.ins().isub(l, r),
                    BinaryOp::Multiply => builder.ins().imul(l, r),
                    BinaryOp::Divide => builder.ins().sdiv(l, r),
                    BinaryOp::Equals => {
                        let left_ty = left.resolved_type();
                        let cmp = match left_ty {
                            Type::F32 => builder.ins().fcmp(FloatCC::Equal, l, r),
                            Type::F64 => builder.ins().fcmp(FloatCC::Equal, l, r),
                            _ => builder.ins().icmp(IntCC::Equal, l, r),
                        };
                        let one = builder.ins().iconst(types::I8, 1);
                        let zero = builder.ins().iconst(types::I8, 0);
                        builder.ins().select(cmp, one, zero)
                    }
                };
                Ok(Val::One(result))
            }
            Expr::StructLiteral { name, fields, .. } => {
                let struct_name = name.text(source);
                let layout = struct_layouts.get(struct_name).ok_or_else(|| {
                    CodegenError::InvalidExpression {
                        message: format!("Unknown struct type: {}", struct_name),
                        context: "struct literal evaluation".to_string(),
                    }
                })?;

                // Allocate memory for the struct on the stack
                let struct_size = layout.total_size;
                let slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    struct_size,
                    3, // 8-byte alignment = 2^3
                ));

                // Initialize all fields to zero first
                let zero_i32 = builder.ins().iconst(types::I32, 0);
                let zero_i64 = builder.ins().iconst(types::I64, 0);
                let zero_i8 = builder.ins().iconst(types::I8, 0);

                for field_layout in &layout.fields {
                    let field_addr =
                        builder
                            .ins()
                            .stack_addr(types::I64, slot, field_layout.offset as i32);
                    match field_layout.cranelift_type {
                        types::I8 => {
                            builder.ins().store(MemFlags::new(), zero_i8, field_addr, 0);
                        }
                        types::I32 => {
                            builder
                                .ins()
                                .store(MemFlags::new(), zero_i32, field_addr, 0);
                        }
                        types::I64 => {
                            builder
                                .ins()
                                .store(MemFlags::new(), zero_i64, field_addr, 0);
                        }
                        _ => {}
                    }
                }

                // Set provided field values
                for struct_field in fields {
                    let field_name = struct_field.name.text(source);
                    let field_layout = layout
                        .fields
                        .iter()
                        .find(|f| f.name == field_name)
                        .ok_or_else(|| CodegenError::InvalidExpression {
                            message: format!(
                                "Unknown field '{}' in struct '{}'",
                                field_name, struct_name
                            ),
                            context: "struct literal field assignment".to_string(),
                        })?;

                    // Generate the field value
                    let field_value = Self::generate_expression_impl_static(
                        struct_layouts,
                        variables,
                        module,
                        builder,
                        &struct_field.value,
                        println_func_id,
                        function_ids,
                        source,
                    )?;

                    // Store the value at the correct offset
                    let field_addr =
                        builder
                            .ins()
                            .stack_addr(types::I64, slot, field_layout.offset as i32);
                    match field_value {
                        Val::One(v) => {
                            builder.ins().store(MemFlags::new(), v, field_addr, 0);
                        }
                        Val::Many(_) => {
                            return Err(CodegenError::InvalidExpression {
                                message: "cannot assign multi-value to scalar field".to_string(),
                                context: "struct literal field assignment".to_string(),
                            });
                        }
                    }
                }

                // Return pointer to the struct
                Ok(Val::One(builder.ins().stack_addr(types::I64, slot, 0)))
            }
            Expr::FieldAccess { object, field, .. } => {
                // Generate the object expression (should be a struct pointer)
                let object_val = Self::generate_expression_impl_static(
                    struct_layouts,
                    variables,
                    module,
                    builder,
                    object,
                    println_func_id,
                    function_ids,
                    source,
                )?;
                let object_val = match object_val {
                    Val::One(v) => v,
                    _ => {
                        return Err(CodegenError::InvalidExpression {
                            message: "field access requires object pointer value".to_string(),
                            context: "field access".to_string(),
                        });
                    }
                };

                // Get the struct type from the object expression
                // For now, we'll need to infer the struct type from context
                // This is a simplified approach - in a real compiler, we'd have type checking
                let field_name = field.text(source);

                // Try to find which struct layout contains this field
                let mut matching_layout = None;
                for layout in struct_layouts.values() {
                    if layout.fields.iter().any(|f| f.name == field_name) {
                        matching_layout = Some(layout);
                        break;
                    }
                }

                let layout = matching_layout.ok_or_else(|| CodegenError::InvalidExpression {
                    message: format!("Field '{}' not found in any struct", field_name),
                    context: "field access evaluation".to_string(),
                })?;

                let field_layout = layout.fields.iter().find(|f| f.name == field_name).unwrap(); // Safe because we found it above

                // Calculate field address: object_ptr + field_offset
                let field_offset_val = builder.ins().iconst(types::I64, field_layout.offset as i64);
                let field_addr = builder.ins().iadd(object_val, field_offset_val);

                // Load the field value from memory
                let field_value =
                    builder
                        .ins()
                        .load(field_layout.cranelift_type, MemFlags::new(), field_addr, 0);

                Ok(Val::One(field_value))
            }
            Expr::FunctionCall { path, args, .. } => {
                let is_println =
                    path.segments.len() == 1 && path.segments[0].text(source) == "println";
                if is_println && args.len() == 1 {
                    let arg_val = Self::generate_expression_impl_static(
                        struct_layouts,
                        variables,
                        module,
                        builder,
                        &args[0],
                        println_func_id,
                        function_ids,
                        source,
                    )?;

                    let scalar = match arg_val {
                        Val::One(v) => v,
                        Val::Many(_) => {
                            return Err(CodegenError::InvalidExpression {
                                message: "println currently supports scalar values only"
                                    .to_string(),
                                context: "println".to_string(),
                            });
                        }
                    };

                    // Check if this is a boolean type based on value type
                    let value_type = builder.func.dfg.value_type(scalar);
                    if value_type == types::I8 {
                        let println_bool_func_id =
                            Self::get_or_create_println_bool_function(module)?;
                        let local_func =
                            module.declare_func_in_func(println_bool_func_id, builder.func);
                        let call = builder.ins().call(local_func, &[scalar]);
                        let _results = builder.inst_results(call);
                    } else {
                        let i64_val = if value_type == types::I64 {
                            scalar
                        } else {
                            // Convert to i64 (sign-extend for signed types, zero-extend for unsigned)
                            if value_type == types::I32 || value_type == types::I16 {
                                builder.ins().sextend(types::I64, scalar)
                            } else {
                                // Default: zero-extend other types
                                builder.ins().uextend(types::I64, scalar)
                            }
                        };
                        let local_func = module.declare_func_in_func(println_func_id, builder.func);
                        let _ = builder.ins().call(local_func, &[i64_val]);
                    }
                    // Return void value (0)
                    Ok(Val::One(builder.ins().iconst(types::I64, 0)))
                } else {
                    let sym = path.mangle(source, "__");
                    if let Some(&func_id) = function_ids.get(&sym) {
                        // User-defined function call (supports flattened aggregates)
                        let mut flat_args: Vec<Value> = Vec::new();
                        for arg in args {
                            match Self::generate_expression_impl_static(
                                struct_layouts,
                                variables,
                                module,
                                builder,
                                arg,
                                println_func_id,
                                function_ids,
                                source,
                            )? {
                                Val::One(v) => flat_args.push(v),
                                Val::Many(vs) => flat_args.extend(vs),
                            }
                        }
                        let local_func = module.declare_func_in_func(func_id, builder.func);
                        let call = builder.ins().call(local_func, &flat_args);
                        let results = builder.inst_results(call);
                        if results.is_empty() {
                            Ok(Val::One(builder.ins().iconst(types::I64, 0)))
                        } else {
                            Ok(Val::One(results[0]))
                        }
                    } else if let Some(last) = path.segments.last() {
                        let last_name = last.text(source).to_string();
                        if let Some(&func_id) = function_ids.get(&last_name) {
                            let mut flat_args: Vec<Value> = Vec::new();
                            for arg in args {
                                match Self::generate_expression_impl_static(
                                    struct_layouts,
                                    variables,
                                    module,
                                    builder,
                                    arg,
                                    println_func_id,
                                    function_ids,
                                    source,
                                )? {
                                    Val::One(v) => flat_args.push(v),
                                    Val::Many(vs) => flat_args.extend(vs),
                                }
                            }
                            let local_func = module.declare_func_in_func(func_id, builder.func);
                            let call = builder.ins().call(local_func, &flat_args);
                            let results = builder.inst_results(call);
                            if results.is_empty() {
                                Ok(Val::One(builder.ins().iconst(types::I64, 0)))
                            } else {
                                Ok(Val::One(results[0]))
                            }
                        } else {
                            Err(CodegenError::UnsupportedFunctionCall {
                                name: sym,
                                context: "function call".to_string(),
                            })
                        }
                    } else if sym.starts_with("std__") {
                        // External import: coerce all args to i64 and call
                        let mut sig = module.make_signature();
                        let mut coerced: Vec<Value> = Vec::new();
                        for arg in args {
                            match Self::generate_expression_impl_static(
                                struct_layouts,
                                variables,
                                module,
                                builder,
                                arg,
                                println_func_id,
                                function_ids,
                                source,
                            )? {
                                Val::One(v) => {
                                    let vt = builder.func.dfg.value_type(v);
                                    let v_i64 = if vt == types::I64 {
                                        v
                                    } else if vt == types::I32
                                        || vt == types::I16
                                        || vt == types::I8
                                    {
                                        builder.ins().uextend(types::I64, v)
                                    } else {
                                        v
                                    };
                                    sig.params.push(AbiParam::new(types::I64));
                                    coerced.push(v_i64);
                                }
                                Val::Many(vs) => {
                                    for v in vs {
                                        let vt = builder.func.dfg.value_type(v);
                                        let v_i64 = if vt == types::I64 {
                                            v
                                        } else if vt == types::I32
                                            || vt == types::I16
                                            || vt == types::I8
                                        {
                                            builder.ins().uextend(types::I64, v)
                                        } else {
                                            v
                                        };
                                        sig.params.push(AbiParam::new(types::I64));
                                        coerced.push(v_i64);
                                    }
                                }
                            }
                        }
                        sig.returns.push(AbiParam::new(types::I64));
                        let ext_id = module
                            .declare_function(&sym, Linkage::Import, &sig)
                            .map_err(|e| CodegenError::CraneliftModuleError(Box::new(e)))?;
                        let local = module.declare_func_in_func(ext_id, builder.func);
                        let call = builder.ins().call(local, &coerced);
                        let results = builder.inst_results(call);
                        if results.is_empty() {
                            Ok(Val::One(builder.ins().iconst(types::I64, 0)))
                        } else {
                            Ok(Val::One(results[0]))
                        }
                    } else {
                        Err(CodegenError::UnsupportedFunctionCall {
                            name: sym,
                            context: "function call".to_string(),
                        })
                    }
                }
            }
            Expr::Dereference { operand, ty } => {
                // Generate the pointer expression
                let ptr_val = Self::generate_expression_impl_static(
                    struct_layouts,
                    variables,
                    module,
                    builder,
                    operand,
                    println_func_id,
                    function_ids,
                    source,
                )?;
                let ptr_val = match ptr_val {
                    Val::One(v) => v,
                    _ => {
                        return Err(CodegenError::InvalidExpression {
                            message: "dereference expects pointer value".to_string(),
                            context: "dereference".to_string(),
                        });
                    }
                };

                // Load using the pointee lane determined by the type checker
                let (lane, _) = Self::scalar_lane(ty);
                let loaded_val = builder.ins().load(lane, MemFlags::new(), ptr_val, 0);
                Ok(Val::One(loaded_val))
            }
        }
    }

    fn unescape_string_literal(src: &str) -> Result<Vec<u8>, String> {
        // Expect a quoted string: "..."
        let s = src
            .strip_prefix('"')
            .and_then(|t| t.strip_suffix('"'))
            .ok_or_else(|| "missing quotes".to_string())?;
        let mut out = Vec::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars
                    .next()
                    .ok_or_else(|| "trailing backslash".to_string())?
                {
                    'n' => out.push(b'\n'),
                    't' => out.push(b'\t'),
                    'r' => out.push(b'\r'),
                    '0' => out.push(b'\0'),
                    '"' => out.push(b'"'),
                    '\\' => out.push(b'\\'),
                    other => return Err(format!("unsupported escape: \\{}", other)),
                }
            } else {
                out.extend(c.to_string().as_bytes());
            }
        }
        Ok(out)
    }

    fn create_println_function(&mut self) -> Result<cranelift_module::FuncId, CodegenError> {
        // Create printf function signature (from C library)
        let mut printf_sig = self.module.make_signature();
        printf_sig.params.push(AbiParam::new(types::I64)); // format string pointer
        printf_sig.params.push(AbiParam::new(types::I64)); // number to print
        printf_sig.returns.push(AbiParam::new(types::I32)); // return value

        // Declare printf as external function
        let printf_func_id =
            self.module
                .declare_function("printf", Linkage::Import, &printf_sig)?;

        // Create println function signature
        let mut println_sig = self.module.make_signature();
        println_sig.params.push(AbiParam::new(types::I64));

        // Declare println function
        let println_func_id =
            self.module
                .declare_function("println", Linkage::Local, &println_sig)?;

        // Create format string data
        let format_string = "%ld\n\0"; // format for long integer with newline
        let format_data_id =
            self.module
                .declare_data("format_string", Linkage::Local, true, false)?;

        // Define the format string data
        let mut format_data_desc = cranelift_module::DataDescription::new();
        format_data_desc.define(format_string.as_bytes().to_vec().into_boxed_slice());
        self.module.define_data(format_data_id, &format_data_desc)?;

        // Create function context
        let mut func_ctx = cranelift::codegen::Context::new();
        func_ctx.func.signature = println_sig;

        // Build function body
        {
            let mut builder = FunctionBuilder::new(&mut func_ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Get the number parameter
            let number_param = builder.block_params(entry_block)[0];

            // Get format string address
            let format_global_value = self
                .module
                .declare_data_in_func(format_data_id, builder.func);
            let format_addr = builder.ins().global_value(types::I64, format_global_value);

            // Get printf function reference
            let printf_func_ref = self
                .module
                .declare_func_in_func(printf_func_id, builder.func);

            // Call printf(format_string, number)
            let _call_result = builder
                .ins()
                .call(printf_func_ref, &[format_addr, number_param]);

            builder.ins().return_(&[]);
            builder.finalize();
        }

        // Define the function
        self.module
            .define_function(println_func_id, &mut func_ctx)?;
        self.module.clear_context(&mut func_ctx);

        Ok(println_func_id)
    }

    fn get_or_create_println_bool_function(
        module: &mut ObjectModule,
    ) -> Result<cranelift_module::FuncId, CodegenError> {
        if let Some(cranelift_module::FuncOrDataId::Func(func_id)) =
            module.get_name("println_bool_cached")
        {
            return Ok(func_id);
        }

        Self::create_println_bool_function(module)
    }

    fn create_println_bool_function(
        module: &mut ObjectModule,
    ) -> Result<cranelift_module::FuncId, CodegenError> {
        // Use random suffix to avoid name conflicts when called multiple times
        let _unique_suffix = format!("_{}", std::ptr::addr_of!(*module) as usize);

        // Use puts instead of printf to avoid signature conflicts
        let mut puts_sig = module.make_signature();
        puts_sig.params.push(AbiParam::new(types::I64)); // string pointer
        puts_sig.returns.push(AbiParam::new(types::I32)); // return value

        // Declare puts as external function
        let puts_func_id = module.declare_function("puts", Linkage::Import, &puts_sig)?;

        // Create println_bool function signature
        let mut println_bool_sig = module.make_signature();
        println_bool_sig.params.push(AbiParam::new(types::I8)); // boolean value (0 or 1)

        // Use cached function name
        let println_bool_func_name = "println_bool_cached";

        // Declare println_bool function
        let println_bool_func_id =
            module.declare_function(println_bool_func_name, Linkage::Local, &println_bool_sig)?;

        // Create strings for true and false (puts adds newline automatically)
        let true_string = "true\0";
        let false_string = "false\0";

        let true_data_name = "bool_true_string_cached";
        let false_data_name = "bool_false_string_cached";

        let true_data_id = module.declare_data(true_data_name, Linkage::Local, true, false)?;

        let false_data_id = module.declare_data(false_data_name, Linkage::Local, true, false)?;

        // Define the true string data
        let mut true_data_desc = cranelift_module::DataDescription::new();
        true_data_desc.define(true_string.as_bytes().to_vec().into_boxed_slice());
        module.define_data(true_data_id, &true_data_desc)?;

        // Define the false string data
        let mut false_data_desc = cranelift_module::DataDescription::new();
        false_data_desc.define(false_string.as_bytes().to_vec().into_boxed_slice());
        module.define_data(false_data_id, &false_data_desc)?;

        // Create function context
        let mut func_ctx = cranelift::codegen::Context::new();
        func_ctx.func.signature = println_bool_sig;

        // Build function body
        {
            let mut builder_context = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut func_ctx.func, &mut builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            let true_block = builder.create_block();
            let false_block = builder.create_block();
            let end_block = builder.create_block();

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Get the boolean parameter
            let bool_param = builder.block_params(entry_block)[0];

            // Check if the value is 0 (false) or non-zero (true)
            let zero = builder.ins().iconst(types::I8, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, bool_param, zero);

            // Branch based on the boolean value
            builder
                .ins()
                .brif(is_zero, false_block, &[], true_block, &[]);

            // True block: print "true"
            builder.switch_to_block(true_block);
            let true_global_value = module.declare_data_in_func(true_data_id, builder.func);
            let true_addr = builder.ins().global_value(types::I64, true_global_value);
            let puts_func_ref = module.declare_func_in_func(puts_func_id, builder.func);
            let _call_result = builder.ins().call(puts_func_ref, &[true_addr]);
            builder.ins().jump(end_block, &[]);
            builder.seal_block(true_block);

            // False block: print "false"
            builder.switch_to_block(false_block);
            let false_global_value = module.declare_data_in_func(false_data_id, builder.func);
            let false_addr = builder.ins().global_value(types::I64, false_global_value);
            let puts_func_ref = module.declare_func_in_func(puts_func_id, builder.func);
            let _call_result = builder.ins().call(puts_func_ref, &[false_addr]);
            builder.ins().jump(end_block, &[]);
            builder.seal_block(false_block);

            // End block
            builder.switch_to_block(end_block);
            builder.seal_block(end_block);
            builder.ins().return_(&[]);
            builder.finalize();
        }

        // Define the function
        module.define_function(println_bool_func_id, &mut func_ctx)?;
        module.clear_context(&mut func_ctx);

        Ok(println_bool_func_id)
    }

    fn compute_struct_layouts(
        &mut self,
        structs: &[StructDefinition],
        source: &str,
    ) -> Result<(), CodegenError> {
        for struct_def in structs {
            let struct_name = struct_def.name.text(source).to_string();
            let mut fields = Vec::new();
            let mut current_offset = 0u32;

            for field_def in &struct_def.fields {
                let field_name = field_def.name.text(source).to_string();
                let (field_size, cranelift_type) =
                    self.get_type_size_and_cranelift_type(&field_def.field_type);

                // Align to type size (simple alignment)
                let alignment = field_size;
                current_offset = current_offset.div_ceil(alignment) * alignment;

                fields.push(FieldLayout {
                    name: field_name,
                    offset: current_offset,
                    size: field_size,
                    cranelift_type,
                });

                current_offset += field_size;
            }

            // Align total size to largest field alignment (or 8 bytes max)
            let max_alignment = fields.iter().map(|f| f.size).max().unwrap_or(1).min(8);
            let total_size = current_offset.div_ceil(max_alignment) * max_alignment;

            let layout = StructLayout { fields, total_size };

            self.struct_layouts.insert(struct_name, layout);
        }
        Ok(())
    }

    fn get_type_size_and_cranelift_type(&self, type_annotation: &Type) -> (u32, types::Type) {
        match type_annotation {
            Type::U8 => (1, types::I8),
            Type::I8 => (1, types::I8),
            Type::U16 => (2, types::I16),
            Type::I16 => (2, types::I16),
            Type::U32 => (4, types::I32),
            Type::I32 => (4, types::I32),
            Type::U64 => (8, types::I64),
            Type::I64 => (8, types::I64),
            Type::Usize => (8, types::I64), // 64-bit systems
            Type::Isize => (8, types::I64), // 64-bit systems
            Type::F32 => (4, types::F32),
            Type::F64 => (8, types::F64),
            Type::Bool => (1, types::I8),
            Type::Array(_) => (8, types::I64), // Arrays are represented as pointers for now
            Type::StrSlice => (16, types::I64), // Slice is (ptr,len); placeholder CL type for sizing only
            Type::Struct(_) => (8, types::I64), // Struct references are pointers, 8 bytes on 64-bit systems
            Type::Pointer { .. } => (8, types::I64), // All pointers are 8 bytes on 64-bit systems
            Type::Undetermined => panic!(
                "Cannot determine size for undetermined type - type checking should have resolved all types"
            ),
        }
    }
}

pub fn generate_object_code(program: &Program, source: &str) -> Result<Vec<u8>, CodegenError> {
    let generator = CraneliftCodeGenerator::new()?;
    generator.generate(program, source)
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_parse::{TypeCheckError, parse, type_check};

    #[test]
    fn test_generate_simple_let() {
        let source = "fn main() { let x: u32 = 5 }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        // Just check that we get some object code without panicking
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_arithmetic() {
        let source = "fn main() { let result = 3 }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_println() {
        let source = "fn main() { println(5) }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_complex_expression() {
        let source = "fn main() { let result = 2 + 3 * 4 }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_error_undefined_variable() {
        let source = "fn main() { let result = unknown_var }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        match result {
            Err(TypeCheckError::UndefinedVariable(name)) => {
                assert_eq!(name, "unknown_var");
            }
            other => panic!("Expected UndefinedVariable error, got {:?}", other),
        }
    }

    #[test]
    fn test_error_unsupported_function() {
        let source = "fn main() { unsupported_func() }";
        let program = parse(source).unwrap();
        let result = type_check(program, source);

        match result {
            Err(TypeCheckError::UndefinedFunction(name)) => {
                assert_eq!(name, "unsupported_func");
            }
            other => panic!("Expected UndefinedFunction error, got {:?}", other),
        }
    }

    #[test]
    fn test_generate_boolean_literals() {
        let source = "fn main() { let flag: bool = true; println(flag) }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_boolean_false() {
        let source = "fn main() { println(false) }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_boolean_equality_expression() {
        let source = "fn main() { let flag = 1 == 1; println(flag) }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_function_return_bool() {
        let source = "fn is_zero(x: i64) -> bool { x == 0 } fn main() { let result = is_zero(0); println(result) }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_boolean_array_literal() {
        let source = "fn main() { let flags = [true, false, true] }";
        let program = parse(source).unwrap();
        let program = type_check(program, source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }
}
