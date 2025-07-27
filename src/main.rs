use std::env;
use std::fs;
use std::process::Command;
use prim_parse::parse;
use prim_codegen::generate_object_code;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <source.prim>", args[0]);
        std::process::exit(1);
    }
    
    let filename = &args[1];
    
    // Read the source file
    let source_code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            std::process::exit(1);
        }
    };
    
    // Parse the source code
    let program = match parse(&source_code) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Parse error: {:?}", err);
            std::process::exit(1);
        }
    };
    
    // Generate object code directly using Cranelift
    let object_code = match generate_object_code(&program) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Code generation error: {}", err);
            std::process::exit(1);
        }
    };
    
    // Write object code to file
    let obj_filename = "output.o";
    if let Err(err) = fs::write(obj_filename, &object_code) {
        eprintln!("Error writing object file: {}", err);
        std::process::exit(1);
    }
    
    println!("Generated object code written to {}", obj_filename);
    
    // Link with GCC to get C runtime library
    let link_output = Command::new("gcc")
        .args(["output.o", "-o", "output"])
        .output();
        
    match link_output {
        Ok(result) => {
            if result.status.success() {
                println!("\nObject code generation and linking successful! Executable: ./output");
                
                // Try to run the program
                println!("\nRunning program:");
                let run_result = Command::new("./output").output();
                match run_result {
                    Ok(run_output) => {
                        print!("{}", String::from_utf8_lossy(&run_output.stdout));
                        if !run_output.stderr.is_empty() {
                            eprint!("{}", String::from_utf8_lossy(&run_output.stderr));
                        }
                    }
                    Err(err) => {
                        eprintln!("Error running program: {}", err);
                    }
                }
            } else {
                eprintln!("Linking failed:");
                eprint!("{}", String::from_utf8_lossy(&result.stderr));
                std::process::exit(1);
            }
        }
        Err(err) => {
            eprintln!("Error running linker: {}", err);
            eprintln!("Make sure GNU binutils (ld) is installed and in your PATH");
            std::process::exit(1);
        }
    }
}