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
                prim_parse::Type::Bool => sig.params.push(AbiParam::new(types::I8)),
                prim_parse::Type::Pointer { .. } => sig.params.push(AbiParam::new(types::I64)),
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
                _ => sig.params.push(AbiParam::new(types::I64)),
            }
        }

        // Add return type to signature
        if function.name.text(source) == "main" {
            sig.returns.push(AbiParam::new(types::I32)); // main returns int
        } else if let Some(_return_type) = &function.return_type {
            sig.returns.push(AbiParam::new(types::I64)); // For now, all returns are i64
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
                    Self::generate_statement_impl_static(
                        &self.struct_layouts,
                        &mut variables,
                        &mut self.module,
                        &mut builder,
                        stmt,
                        println_func_id,
                        function_ids,
                        source,
                    )?;
                    last_expr_value = None;
                }
            }
        }
        Self::generate_function_return(&mut builder, function, last_expr_value, source)?;

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
                prim_parse::Type::Bool => {
                    let var = builder.declare_var(types::I8);
                    builder.def_var(var, block_params[i]);
                    variables.insert(name, Var::One(var));
                    i += 1;
                }
                prim_parse::Type::StrSlice => {
                    let a = builder.declare_var(types::I64);
                    let b = builder.declare_var(types::I64);
                    builder.def_var(a, block_params[i]);
                    builder.def_var(b, block_params[i + 1]);
                    variables.insert(name, Var::Many(vec![a, b]));
                    i += 2;
                }
                prim_parse::Type::Pointer { .. } => {
                    let var = builder.declare_var(types::I64);
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
                _ => {
                    let var = builder.declare_var(types::I64);
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
                        builder.ins().return_(&[v]);
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
                let zero = builder.ins().iconst(types::I64, 0);
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
    ) -> Result<(), CodegenError> {
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
                        let vt = match value {
                            Expr::BoolLiteral { .. } => types::I8,
                            _ => types::I64,
                        };
                        let var = builder.declare_var(vt);
                        builder.def_var(var, v);
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

                Ok(())
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
                Ok(())
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
            Expr::IntLiteral { span: value, .. } => {
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
                Ok(Val::One(builder.ins().iconst(types::I64, num)))
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
                        let cmp = builder.ins().icmp(IntCC::Equal, l, r);
                        builder.ins().uextend(types::I64, cmp)
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
            Expr::FunctionCall { name, args, .. } => {
                let func_name = name.text(source);

                if func_name == "println" && args.len() == 1 {
                    // Generate the argument
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
                        // For boolean values, call boolean-specific println
                        let println_bool_func_id =
                            Self::get_or_create_println_bool_function(module)?;
                        let local_func =
                            module.declare_func_in_func(println_bool_func_id, builder.func);
                        let call = builder.ins().call(local_func, &[scalar]);
                        let _results = builder.inst_results(call);
                    } else {
                        // For integer values, convert to i64 if needed and use regular println
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
                        let call = builder.ins().call(local_func, &[i64_val]);
                        let _results = builder.inst_results(call);
                    }

                    // Return void value (0)
                    Ok(Val::One(builder.ins().iconst(types::I64, 0)))
                } else if let Some(&func_id) = function_ids.get(func_name) {
                    // User-defined function call
                    let mut arg_vals = Vec::new();
                    for arg in args {
                        let val = Self::generate_expression_impl_static(
                            struct_layouts,
                            variables,
                            module,
                            builder,
                            arg,
                            println_func_id,
                            function_ids,
                            source,
                        )?;
                        match val {
                            Val::One(v) => arg_vals.push(v),
                            Val::Many(vs) => arg_vals.extend(vs),
                        }
                    }

                    // Get function reference
                    let local_func = module.declare_func_in_func(func_id, builder.func);

                    // Call the function
                    let call = builder.ins().call(local_func, &arg_vals);
                    let results = builder.inst_results(call);

                    // Return the result (or 0 if void function)
                    if results.is_empty() {
                        Ok(Val::One(builder.ins().iconst(types::I64, 0)))
                    } else {
                        Ok(Val::One(results[0]))
                    }
                } else {
                    Err(CodegenError::UnsupportedFunctionCall {
                        name: func_name.to_string(),
                        context: "function call".to_string(),
                    })
                }
            }
            Expr::Dereference { operand, .. } => {
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

                // LIMITATION: Always loads 8 bytes (i64) regardless of pointee type
                //
                // Examples of incorrect behavior:
                //   *ptr_u8   loads 8 bytes instead of 1 byte
                //   *ptr_i32  loads 8 bytes instead of 4 bytes
                //
                // This can cause memory safety issues and incorrect values.
                // To fix: implement type tracking for expressions or require explicit type annotations.
                let loaded_val = builder.ins().load(types::I64, MemFlags::new(), ptr_val, 0);
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
        // Try to get existing function first
        if let Ok(func_id) = module.declare_function("println_bool_cached", Linkage::Local, &{
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(types::I8));
            sig
        }) {
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
    use prim_parse::parse;

    #[test]
    fn test_generate_simple_let() {
        let source = "fn main() { let x: u32 = 5 }";
        let program = parse(source).unwrap();
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
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_println() {
        let source = "fn main() { println(5) }";
        let program = parse(source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_complex_expression() {
        let source = "fn main() { let result = 2 + 3 * 4 }";
        let program = parse(source).unwrap();
        let object_code = generate_object_code(&program, source);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_error_undefined_variable() {
        let source = "fn main() { let result = unknown_var }";
        let program = parse(source).unwrap();
        let result = generate_object_code(&program, source);

        match result {
            Err(CodegenError::UndefinedVariable { name, context: _ }) => {
                assert_eq!(name, "unknown_var");
            }
            _ => panic!("Expected UndefinedVariable error, got {:?}", result),
        }
    }

    #[test]
    fn test_error_unsupported_function() {
        let source = "fn main() { unsupported_func() }";
        let program = parse(source).unwrap();
        let result = generate_object_code(&program, source);

        match result {
            Err(CodegenError::UnsupportedFunctionCall { name, context: _ }) => {
                assert_eq!(name, "unsupported_func");
            }
            _ => panic!("Expected UnsupportedFunctionCall error, got {:?}", result),
        }
    }

    #[test]
    fn test_generate_boolean_literals() {
        let source = "fn main() { let flag: bool = true; println(flag) }";
        let program = parse(source).unwrap();
        let object_code = generate_object_code(&program, source);

        if let Err(e) = &object_code {
            println!("Error generating boolean literals: {:?}", e);
        }
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_boolean_false() {
        let source = "fn main() { println(false) }";
        let program = parse(source).unwrap();
        let object_code = generate_object_code(&program, source);

        if let Err(e) = &object_code {
            println!("Error generating boolean false: {:?}", e);
        }
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }
}
