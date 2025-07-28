use prim_parse::{BinaryOp, Expr, Function, Program, Stmt};
use std::collections::HashMap;

use cranelift::prelude::*;
// use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

mod error;
pub use error::{CodegenError, CodegenResult};

pub struct CraneliftCodeGenerator {
    module: ObjectModule,
    ctx: codegen::Context,
    builder_context: FunctionBuilderContext,
}

impl CraneliftCodeGenerator {
    pub fn new() -> CodegenResult<Self> {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("use_colocated_libcalls", "false")
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;
        flag_builder
            .set("is_pic", "false")
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;
        let isa_builder =
            cranelift_native::builder().map_err(|msg| CodegenError::UnsupportedTarget {
                message: msg.to_string(),
            })?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;

        let builder = ObjectBuilder::new(
            isa,
            "prim_program",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| CodegenError::CraneliftError {
            message: e.to_string(),
        })?;
        let module = ObjectModule::new(builder);
        let ctx = module.make_context();

        Ok(Self {
            module,
            ctx,
            builder_context: FunctionBuilderContext::new(),
        })
    }

    pub fn generate(mut self, program: &Program) -> CodegenResult<Vec<u8>> {
        // Create println function first
        let println_func_id = self.create_println_function()?;

        // Generate all functions
        for function in &program.functions {
            self.generate_function(function, println_func_id)?;
        }

        // Finalize the object
        let product = self.module.finish();
        product.emit().map_err(|e| CodegenError::CraneliftError {
            message: e.to_string(),
        })
    }

    fn generate_function(
        &mut self,
        function: &Function,
        println_func_id: cranelift_module::FuncId,
    ) -> CodegenResult<()> {
        // Create function signature
        let mut sig = self.module.make_signature();

        // Add parameters to signature
        for _param in &function.parameters {
            sig.params.push(AbiParam::new(types::I64)); // For now, all params are i64
        }

        // Add return type to signature
        if function.name == "main" {
            sig.returns.push(AbiParam::new(types::I32)); // main returns int
        } else if let Some(_return_type) = &function.return_type {
            sig.returns.push(AbiParam::new(types::I64)); // For now, all returns are i64
        }

        // Determine linkage - main is exported, others are local
        let linkage = if function.name == "main" {
            Linkage::Export
        } else {
            Linkage::Local
        };

        let func_id = self
            .module
            .declare_function(&function.name, linkage, &sig)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;

        // Generate function body
        self.ctx.func.signature = sig;

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Create fresh variable context for this function
            let mut variables = HashMap::new();
            let mut variable_counter = 0;

            // Add parameters to variables
            let block_params: Vec<Value> = builder.block_params(entry_block).to_vec();
            for (i, param) in function.parameters.iter().enumerate() {
                let var = Variable::new(variable_counter);
                variable_counter += 1;
                builder.declare_var(var, types::I64);
                builder.def_var(var, block_params[i]);
                variables.insert(param.name.clone(), var);
            }

            // Generate code for each statement in function body, keeping track of last expression
            let mut last_expr_value = None;
            for stmt in &function.body {
                match stmt {
                    Stmt::Expr(expr) => {
                        // Keep track of the last expression value
                        last_expr_value = Some(Self::generate_expression_impl(
                            &variables,
                            &mut self.module,
                            &mut builder,
                            expr,
                            println_func_id,
                        )?);
                    }
                    _ => {
                        Self::generate_statement_impl(
                            &mut variables,
                            &mut variable_counter,
                            &mut self.module,
                            &mut builder,
                            stmt,
                            println_func_id,
                        )?;
                        last_expr_value = None; // Reset if we have a non-expression statement
                    }
                }
            }

            // Add return statement
            if function.name == "main" {
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

            builder.finalize();
        }

        // Define the function
        self.module
            .define_function(func_id, &mut self.ctx)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;
        self.module.clear_context(&mut self.ctx);

        Ok(())
    }

    fn generate_statement_impl(
        variables: &mut HashMap<String, Variable>,
        variable_counter: &mut usize,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
        stmt: &Stmt,
        println_func_id: cranelift_module::FuncId,
    ) -> CodegenResult<()> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation: _,
                value,
            } => {
                // Create a variable
                let var = Variable::new(*variable_counter);
                *variable_counter += 1;
                builder.declare_var(var, types::I64);

                // Generate expression
                let val = Self::generate_expression_impl(
                    variables,
                    module,
                    builder,
                    value,
                    println_func_id,
                )?;

                // Store in variable
                builder.def_var(var, val);
                variables.insert(name.clone(), var);

                Ok(())
            }
            Stmt::Expr(expr) => {
                Self::generate_expression_impl(variables, module, builder, expr, println_func_id)?;
                Ok(())
            }
        }
    }

    fn generate_expression_impl(
        variables: &HashMap<String, Variable>,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        println_func_id: cranelift_module::FuncId,
    ) -> CodegenResult<Value> {
        match expr {
            Expr::IntLiteral(value) => {
                // Parse the literal (handle type suffixes like 42u32)
                let num_part = value
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect::<String>();
                let num: i64 = num_part.parse().unwrap_or(0);
                Ok(builder.ins().iconst(types::I64, num))
            }
            Expr::FloatLiteral(_) => {
                // For now, treat floats as integers (truncated)
                // TODO: Implement proper float support
                Ok(builder.ins().iconst(types::I64, 0))
            }
            Expr::Identifier(name) => {
                if let Some(&var) = variables.get(name) {
                    Ok(builder.use_var(var))
                } else {
                    Err(CodegenError::UndefinedVariable { name: name.clone() })
                }
            }
            Expr::Binary { left, op, right } => {
                let left_val = Self::generate_expression_impl(
                    variables,
                    module,
                    builder,
                    left,
                    println_func_id,
                )?;
                let right_val = Self::generate_expression_impl(
                    variables,
                    module,
                    builder,
                    right,
                    println_func_id,
                )?;

                let result = match op {
                    BinaryOp::Add => builder.ins().iadd(left_val, right_val),
                    BinaryOp::Subtract => builder.ins().isub(left_val, right_val),
                    BinaryOp::Multiply => builder.ins().imul(left_val, right_val),
                    BinaryOp::Equals => {
                        let cmp = builder.ins().icmp(IntCC::Equal, left_val, right_val);
                        builder.ins().uextend(types::I64, cmp)
                    }
                };
                Ok(result)
            }
            Expr::FunctionCall { name, args } => {
                if name == "println" && args.len() == 1 {
                    // Generate the argument
                    let arg_val = Self::generate_expression_impl(
                        variables,
                        module,
                        builder,
                        &args[0],
                        println_func_id,
                    )?;

                    // Get function reference
                    let local_func = module.declare_func_in_func(println_func_id, builder.func);

                    // Call the function
                    let call = builder.ins().call(local_func, &[arg_val]);
                    let _results = builder.inst_results(call);

                    // Return void value (0)
                    Ok(builder.ins().iconst(types::I64, 0))
                } else {
                    Err(CodegenError::UnsupportedFunctionCall { name: name.clone() })
                }
            }
        }
    }

    fn create_println_function(&mut self) -> CodegenResult<cranelift_module::FuncId> {
        // Create printf function signature (from C library)
        let mut printf_sig = self.module.make_signature();
        printf_sig.params.push(AbiParam::new(types::I64)); // format string pointer
        printf_sig.params.push(AbiParam::new(types::I64)); // number to print
        printf_sig.returns.push(AbiParam::new(types::I32)); // return value

        // Declare printf as external function
        let printf_func_id = self
            .module
            .declare_function("printf", Linkage::Import, &printf_sig)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;

        // Create println function signature
        let mut println_sig = self.module.make_signature();
        println_sig.params.push(AbiParam::new(types::I64));

        // Declare println function
        let println_func_id = self
            .module
            .declare_function("println", Linkage::Local, &println_sig)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;

        // Create format string data
        let format_string = "%ld\n\0"; // format for long integer with newline
        let format_data_id = self
            .module
            .declare_data("format_string", Linkage::Local, true, false)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;

        // Define the format string data
        let mut format_data_desc = cranelift_module::DataDescription::new();
        format_data_desc.define(format_string.as_bytes().to_vec().into_boxed_slice());
        self.module
            .define_data(format_data_id, &format_data_desc)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;

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
            .define_function(println_func_id, &mut func_ctx)
            .map_err(|e| CodegenError::CraneliftError {
                message: e.to_string(),
            })?;
        self.module.clear_context(&mut func_ctx);

        Ok(println_func_id)
    }
}

pub fn generate_object_code(program: &Program) -> CodegenResult<Vec<u8>> {
    let generator = CraneliftCodeGenerator::new()?;
    generator.generate(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_parse::parse;

    #[test]
    fn test_generate_simple_let() {
        let program = parse("fn main() { let x: u32 = 5 }").unwrap();
        let object_code = generate_object_code(&program);

        // Just check that we get some object code without panicking
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_arithmetic() {
        let program = parse("fn main() { let result = 3 }").unwrap();
        let object_code = generate_object_code(&program);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_println() {
        let program = parse("fn main() { println(5) }").unwrap();
        let object_code = generate_object_code(&program);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_generate_complex_expression() {
        let program = parse("fn main() { let result = 2 + 3 * 4 }").unwrap();
        let object_code = generate_object_code(&program);

        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }

    #[test]
    fn test_error_undefined_variable() {
        let program = parse("fn main() { let result = unknown_var }").unwrap();
        let result = generate_object_code(&program);

        match result {
            Err(CodegenError::UndefinedVariable { name }) => {
                assert_eq!(name, "unknown_var");
            }
            _ => panic!("Expected UndefinedVariable error"),
        }
    }

    #[test]
    fn test_error_unsupported_function() {
        let program = parse("fn main() { unsupported_func() }").unwrap();
        let result = generate_object_code(&program);

        match result {
            Err(CodegenError::UnsupportedFunctionCall { name }) => {
                assert_eq!(name, "unsupported_func");
            }
            _ => panic!("Expected UnsupportedFunctionCall error"),
        }
    }
}
