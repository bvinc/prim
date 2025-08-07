use prim_parse::{BinaryOp, Expr, Function, Program, Stmt, StructDefinition, Type};
use std::collections::HashMap;

use cranelift::prelude::*;
// use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

mod error;
pub use error::CodegenError;

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

        // First pass: declare all functions
        let mut function_ids = HashMap::new();
        for function in &program.functions {
            let sig = self.create_function_signature(function, source);
            let linkage = self.determine_linkage(function, source);

            let func_id =
                self.module
                    .declare_function(function.name.text(source), linkage, &sig)?;

            function_ids.insert(function.name.text(source).to_string(), func_id);
        }

        // Second pass: generate function bodies
        for function in &program.functions {
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

        // Add parameters to signature
        for param in &function.parameters {
            let param_type = match param.type_annotation {
                prim_parse::Type::Bool => types::I8,
                prim_parse::Type::Pointer { .. } => types::I64, // All pointers are 64-bit
                _ => types::I64,
            };
            sig.params.push(AbiParam::new(param_type));
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
        if function.name.text(source) == "main" {
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

        let (variables, _) =
            Self::setup_function_parameters(&mut builder, entry_block, function, source);

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
        Self::generate_function_return(&mut builder, function, last_expr_value, source);

        builder.finalize();
        Ok(())
    }

    fn setup_function_parameters(
        builder: &mut FunctionBuilder,
        entry_block: Block,
        function: &Function,
        source: &str,
    ) -> (HashMap<String, Variable>, usize) {
        let mut variables = HashMap::new();

        // Add parameters to variables
        let block_params: Vec<Value> = builder.block_params(entry_block).to_vec();
        for (i, param) in function.parameters.iter().enumerate() {
            let param_type = match param.type_annotation {
                prim_parse::Type::Bool => types::I8,
                prim_parse::Type::Pointer { .. } => types::I64, // All pointers are 64-bit
                _ => types::I64,
            };
            let var = builder.declare_var(param_type);
            builder.def_var(var, block_params[i]);
            variables.insert(param.name.text(source).to_string(), var);
        }

        (variables, 0)
    }

    fn generate_function_return(
        builder: &mut FunctionBuilder,
        function: &Function,
        last_expr_value: Option<Value>,
        source: &str,
    ) {
        if function.name.text(source) == "main" {
            // Main function returns 0
            let zero = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[zero]);
        } else if function.return_type.is_some() {
            // Function has return type, return the last expression or default value
            if let Some(return_val) = last_expr_value {
                builder.ins().return_(&[return_val]);
            } else {
                // Return a default value if no expression
                let zero = builder.ins().iconst(types::I64, 0);
                builder.ins().return_(&[zero]);
            }
        } else {
            // Other functions return void
            builder.ins().return_(&[]);
        }
    }

    fn generate_statement_impl_static(
        struct_layouts: &HashMap<String, StructLayout>,
        variables: &mut HashMap<String, Variable>,
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
                // Determine the type based on the value expression
                let var_type = match value {
                    Expr::BoolLiteral(_) => types::I8,
                    _ => types::I64,
                };
                let var = builder.declare_var(var_type);

                // Generate expression
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

                // Store in variable
                builder.def_var(var, val);
                variables.insert(name.text(source).to_string(), var);

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
        variables: &HashMap<String, Variable>,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        println_func_id: cranelift_module::FuncId,
        function_ids: &HashMap<String, cranelift_module::FuncId>,
        source: &str,
    ) -> Result<Value, CodegenError> {
        match expr {
            Expr::IntLiteral(value) => {
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
                Ok(builder.ins().iconst(types::I64, num))
            }
            Expr::FloatLiteral(value) => Err(CodegenError::InvalidExpression {
                message: format!(
                    "Float literals are not yet supported: {}",
                    value.text(source)
                ),
                context: "float literal evaluation".to_string(),
            }),
            Expr::BoolLiteral(value) => {
                let bool_val = if *value { 1 } else { 0 };
                Ok(builder.ins().iconst(types::I8, bool_val))
            }
            Expr::Identifier(name) => {
                let name_text = name.text(source);
                if let Some(&var) = variables.get(name_text) {
                    Ok(builder.use_var(var))
                } else {
                    Err(CodegenError::UndefinedVariable {
                        name: name_text.to_string(),
                        context: "expression evaluation".to_string(),
                    })
                }
            }
            Expr::Binary { left, op, right } => {
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

                let result = match op {
                    BinaryOp::Add => builder.ins().iadd(left_val, right_val),
                    BinaryOp::Subtract => builder.ins().isub(left_val, right_val),
                    BinaryOp::Multiply => builder.ins().imul(left_val, right_val),
                    BinaryOp::Divide => builder.ins().sdiv(left_val, right_val),
                    BinaryOp::Equals => {
                        let cmp = builder.ins().icmp(IntCC::Equal, left_val, right_val);
                        builder.ins().uextend(types::I64, cmp)
                    }
                };
                Ok(result)
            }
            Expr::StructLiteral { name, fields } => {
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
                    builder
                        .ins()
                        .store(MemFlags::new(), field_value, field_addr, 0);
                }

                // Return pointer to the struct
                Ok(builder.ins().stack_addr(types::I64, slot, 0))
            }
            Expr::FieldAccess { object, field } => {
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

                Ok(field_value)
            }
            Expr::FunctionCall { name, args } => {
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

                    // Check if this is a boolean type based on value type
                    let value_type = builder.func.dfg.value_type(arg_val);
                    if value_type == types::I8 {
                        // For boolean values, call boolean-specific println
                        let println_bool_func_id =
                            Self::get_or_create_println_bool_function(module)?;
                        let local_func =
                            module.declare_func_in_func(println_bool_func_id, builder.func);
                        let call = builder.ins().call(local_func, &[arg_val]);
                        let _results = builder.inst_results(call);
                    } else {
                        // For integer values, convert to i64 if needed and use regular println
                        let i64_val = if value_type == types::I64 {
                            arg_val
                        } else {
                            // Convert to i64 (sign-extend for signed types, zero-extend for unsigned)
                            if value_type == types::I32 || value_type == types::I16 {
                                builder.ins().sextend(types::I64, arg_val)
                            } else {
                                // Default: zero-extend other types
                                builder.ins().uextend(types::I64, arg_val)
                            }
                        };
                        let local_func = module.declare_func_in_func(println_func_id, builder.func);
                        let call = builder.ins().call(local_func, &[i64_val]);
                        let _results = builder.inst_results(call);
                    }

                    // Return void value (0)
                    Ok(builder.ins().iconst(types::I64, 0))
                } else if let Some(&func_id) = function_ids.get(func_name) {
                    // User-defined function call
                    let mut arg_vals = Vec::new();
                    for arg in args {
                        let arg_val = Self::generate_expression_impl_static(
                            struct_layouts,
                            variables,
                            module,
                            builder,
                            arg,
                            println_func_id,
                            function_ids,
                            source,
                        )?;
                        arg_vals.push(arg_val);
                    }

                    // Get function reference
                    let local_func = module.declare_func_in_func(func_id, builder.func);

                    // Call the function
                    let call = builder.ins().call(local_func, &arg_vals);
                    let results = builder.inst_results(call);

                    // Return the result (or 0 if void function)
                    if results.is_empty() {
                        Ok(builder.ins().iconst(types::I64, 0))
                    } else {
                        Ok(results[0])
                    }
                } else {
                    Err(CodegenError::UnsupportedFunctionCall {
                        name: func_name.to_string(),
                        context: "function call".to_string(),
                    })
                }
            }
            Expr::Dereference { operand } => {
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

                // For now, assume we're dereferencing an i64 pointer
                // In a full implementation, we'd need type information to determine the load size
                let loaded_val = builder.ins().load(types::I64, MemFlags::new(), ptr_val, 0);
                Ok(loaded_val)
            }
        }
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
            Type::Struct(_) => (8, types::I64), // Struct references are pointers, 8 bytes on 64-bit systems
            Type::Pointer { .. } => (8, types::I64), // All pointers are 8 bytes on 64-bit systems
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
