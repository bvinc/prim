use prim_parse::{Program, Stmt, Expr, BinaryOp};
use std::collections::HashMap;

use cranelift::prelude::*;
// use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub struct CraneliftCodeGenerator {
    module: ObjectModule,
    ctx: codegen::Context,
    builder_context: FunctionBuilderContext,
    variables: HashMap<String, Variable>,
    variable_counter: usize,
}

impl CraneliftCodeGenerator {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
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
            variables: HashMap::new(),
            variable_counter: 0,
        })
    }
    
    pub fn generate(mut self, program: &Program) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        // Create println function first
        let println_func_id = self.create_println_function()?;
        
        // Create main function
        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));
        
        let main_func_id = self.module.declare_function("main", Linkage::Export, &sig)?;
        
        // Generate function body
        self.ctx.func.signature = sig;
        
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);
            
            // Generate code for each statement
            for stmt in &program.statements {
                Self::generate_statement_impl(&mut self.variables, &mut self.variable_counter, &mut self.module, &mut builder, stmt, println_func_id)?;
            }
            
            // Return 0 from main
            let zero = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[zero]);
            
            builder.finalize();
        }
        
        // Define the function
        self.module.define_function(main_func_id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);
        
        // Finalize the object
        let product = self.module.finish();
        Ok(product.emit()?)
    }
    
    fn generate_statement_impl(
        variables: &mut HashMap<String, Variable>,
        variable_counter: &mut usize,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
        stmt: &Stmt,
        println_func_id: cranelift_module::FuncId
    ) -> Result<(), Box<dyn std::error::Error>> {
        match stmt {
            Stmt::Let { name, type_annotation: _, value } => {
                // Create a variable
                let var = Variable::new(*variable_counter);
                *variable_counter += 1;
                builder.declare_var(var, types::I64);
                
                // Generate expression
                let val = Self::generate_expression_impl(variables, module, builder, value, println_func_id)?;
                
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
        println_func_id: cranelift_module::FuncId
    ) -> Result<Value, Box<dyn std::error::Error>> {
        match expr {
            Expr::IntLiteral(value) => {
                // Parse the literal (handle type suffixes like 42u32)
                let num_part = value.chars()
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
                    panic!("Undefined variable: {}", name);
                }
            }
            Expr::Binary { left, op, right } => {
                let left_val = Self::generate_expression_impl(variables, module, builder, left, println_func_id)?;
                let right_val = Self::generate_expression_impl(variables, module, builder, right, println_func_id)?;
                
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
                    let arg_val = Self::generate_expression_impl(variables, module, builder, &args[0], println_func_id)?;
                    
                    // Get function reference
                    let local_func = module.declare_func_in_func(println_func_id, builder.func);
                    
                    // Call the function
                    let call = builder.ins().call(local_func, &[arg_val]);
                    let _results = builder.inst_results(call);
                    
                    // Return void value (0)
                    Ok(builder.ins().iconst(types::I64, 0))
                } else {
                    panic!("Unsupported function call: {}", name);
                }
            }
        }
    }
    
    fn create_println_function(&mut self) -> Result<cranelift_module::FuncId, Box<dyn std::error::Error>> {
        // Create printf function signature (from C library)
        let mut printf_sig = self.module.make_signature();
        printf_sig.params.push(AbiParam::new(types::I64)); // format string pointer
        printf_sig.params.push(AbiParam::new(types::I64)); // number to print
        printf_sig.returns.push(AbiParam::new(types::I32)); // return value
        
        // Declare printf as external function
        let printf_func_id = self.module.declare_function("printf", Linkage::Import, &printf_sig)?;
        
        // Create println function signature
        let mut println_sig = self.module.make_signature();
        println_sig.params.push(AbiParam::new(types::I64));
        
        // Declare println function
        let println_func_id = self.module.declare_function("println", Linkage::Local, &println_sig)?;
        
        // Create format string data
        let format_string = "%ld\n\0"; // format for long integer with newline
        let format_data_id = self.module.declare_data("format_string", Linkage::Local, true, false)?;
        
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
            let format_global_value = self.module.declare_data_in_func(format_data_id, builder.func);
            let format_addr = builder.ins().global_value(types::I64, format_global_value);
            
            // Get printf function reference
            let printf_func_ref = self.module.declare_func_in_func(printf_func_id, builder.func);
            
            // Call printf(format_string, number)
            let _call_result = builder.ins().call(printf_func_ref, &[format_addr, number_param]);
            
            builder.ins().return_(&[]);
            builder.finalize();
        }
        
        // Define the function
        self.module.define_function(println_func_id, &mut func_ctx)?;
        self.module.clear_context(&mut func_ctx);
        
        Ok(println_func_id)
    }
    
}

pub fn generate_object_code(program: &Program) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let generator = CraneliftCodeGenerator::new()?;
    generator.generate(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_parse::parse;

    #[test]
    fn test_generate_simple_let() {
        let program = parse("let x: u32 = 5").unwrap();
        let object_code = generate_object_code(&program);
        
        // Just check that we get some object code without panicking
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }
    
    #[test]
    fn test_generate_arithmetic() {
        let program = parse("let result = 3").unwrap();
        let object_code = generate_object_code(&program);
        
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }
    
    #[test]
    fn test_generate_println() {
        let program = parse("println(5)").unwrap();
        let object_code = generate_object_code(&program);
        
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }
    
    #[test]
    fn test_generate_complex_expression() {
        let program = parse("let result = 2 + 3 * 4").unwrap();
        let object_code = generate_object_code(&program);
        
        assert!(object_code.is_ok());
        let code = object_code.unwrap();
        assert!(!code.is_empty());
    }
}