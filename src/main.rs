use prim_codegen::generate_object_code;
use prim_parse::parse;
use std::env;
use std::fs;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help(&args[0]);
        std::process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
        "build" => {
            if args.len() != 3 {
                eprintln!("Usage: {} build <source.prim>", args[0]);
                std::process::exit(1);
            }
            build_program(&args[2]);
        }
        "run" => {
            if args.len() != 3 {
                eprintln!("Usage: {} run <source.prim>", args[0]);
                std::process::exit(1);
            }
            run_program(&args[2]);
        }
        "help" | "--help" | "-h" => {
            print_help(&args[0]);
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            print_help(&args[0]);
            std::process::exit(1);
        }
    }
}

fn print_help(program_name: &str) {
    println!("Prim Programming Language Compiler");
    println!();
    println!("USAGE:");
    println!("    {} <COMMAND> <source.prim>", program_name);
    println!();
    println!("COMMANDS:");
    println!("    build    Compile source to executable without running");
    println!("    run      Compile and run source file");
    println!("    help     Show this help message");
    println!();
    println!("EXAMPLES:");
    println!("    {} build example.prim", program_name);
    println!("    {} run example.prim", program_name);
}

fn build_program(filename: &str) {
    let program = compile_source(filename);

    // Generate object code directly using Cranelift
    let object_code = match generate_object_code(&program) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Code generation error: {}", err);
            std::process::exit(1);
        }
    };

    // Create executable name from source file name
    let executable_name = filename.trim_end_matches(".prim");
    let obj_filename = format!("{}.o", executable_name);

    // Write object code to file
    if let Err(err) = fs::write(&obj_filename, &object_code) {
        eprintln!("Error writing object file: {}", err);
        std::process::exit(1);
    }

    // Link with GCC to get C runtime library
    let link_output = Command::new("gcc")
        .args([&obj_filename, "-o", executable_name])
        .output();

    match link_output {
        Ok(result) => {
            if result.status.success() {
                println!("Successfully built executable: {}", executable_name);

                // Clean up object file
                if let Err(err) = fs::remove_file(&obj_filename) {
                    eprintln!(
                        "Warning: Could not clean up object file {}: {}",
                        obj_filename, err
                    );
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

fn run_program(filename: &str) {
    let program = compile_source(filename);

    // Generate object code directly using Cranelift
    let object_code = match generate_object_code(&program) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Code generation error: {}", err);
            std::process::exit(1);
        }
    };

    // Write object code to temporary file
    let obj_filename = "temp_output.o";
    let executable_name = "temp_output";

    if let Err(err) = fs::write(obj_filename, &object_code) {
        eprintln!("Error writing object file: {}", err);
        std::process::exit(1);
    }

    // Link with GCC to get C runtime library
    let link_output = Command::new("gcc")
        .args([obj_filename, "-o", executable_name])
        .output();

    match link_output {
        Ok(result) => {
            if result.status.success() {
                // Run the program immediately
                let run_result = Command::new(format!("./{}", executable_name)).output();
                match run_result {
                    Ok(run_output) => {
                        print!("{}", String::from_utf8_lossy(&run_output.stdout));
                        if !run_output.stderr.is_empty() {
                            eprint!("{}", String::from_utf8_lossy(&run_output.stderr));
                        }

                        // Clean up temporary files before exiting
                        let _ = fs::remove_file(obj_filename);
                        let _ = fs::remove_file(executable_name);

                        // Exit with the same code as the executed program
                        std::process::exit(run_output.status.code().unwrap_or(0));
                    }
                    Err(err) => {
                        // Clean up temporary files before exiting
                        let _ = fs::remove_file(obj_filename);
                        let _ = fs::remove_file(executable_name);

                        eprintln!("Error running program: {}", err);
                        std::process::exit(1);
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

fn compile_source(filename: &str) -> prim_parse::Program {
    // Read the source file
    let source_code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            std::process::exit(1);
        }
    };

    // Parse the source code
    match parse(&source_code) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
