use prim_codegen::generate_object_code;
use prim_parse::parse;
use std::env;
use std::fs;
use std::process::Command;

mod error;
pub use error::{CompilerError, CompilerResult, PrimError};

#[derive(Debug)]
enum MainError {
    InsufficientArguments,
    InvalidUsage(String),
    UnknownCommand(String),
    CompilationError(String),
    IoError(std::io::Error),
    LinkingError(String),
    ExecutionError(String),
}

impl std::fmt::Display for MainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MainError::InsufficientArguments => write!(f, "Insufficient arguments provided"),
            MainError::InvalidUsage(usage) => write!(f, "{}", usage),
            MainError::UnknownCommand(cmd) => write!(f, "Unknown command: {}", cmd),
            MainError::CompilationError(msg) => write!(f, "Compilation error: {}", msg),
            MainError::IoError(err) => write!(f, "IO error: {}", err),
            MainError::LinkingError(msg) => write!(f, "Linking error: {}", msg),
            MainError::ExecutionError(msg) => write!(f, "Execution error: {}", msg),
        }
    }
}

impl std::error::Error for MainError {}

impl From<std::io::Error> for MainError {
    fn from(err: std::io::Error) -> Self {
        MainError::IoError(err)
    }
}

fn main() {
    let exit_code = match run_main() {
        Ok(code) => code,
        Err(MainError::InsufficientArguments) => {
            let args: Vec<String> = env::args().collect();
            print_help(&args[0]);
            1
        }
        Err(err) => {
            eprintln!("{}", err);
            1
        }
    };

    std::process::exit(exit_code);
}

fn run_main() -> Result<i32, MainError> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err(MainError::InsufficientArguments);
    }

    let command = &args[1];

    match command.as_str() {
        "build" => {
            if args.len() != 3 {
                return Err(MainError::InvalidUsage(format!(
                    "Usage: {} build <source.prim>",
                    args[0]
                )));
            }
            build_program(&args[2])?;
            Ok(0)
        }
        "run" => {
            if args.len() != 3 {
                return Err(MainError::InvalidUsage(format!(
                    "Usage: {} run <source.prim>",
                    args[0]
                )));
            }
            let exit_code = run_program(&args[2])?;
            Ok(exit_code)
        }
        "help" | "--help" | "-h" => {
            print_help(&args[0]);
            Ok(0)
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            print_help(&args[0]);
            Err(MainError::UnknownCommand(command.to_string()))
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

fn build_program(filename: &str) -> Result<(), MainError> {
    let program = compile_source(filename)?;

    // Generate object code directly using Cranelift
    let object_code = generate_object_code(&program)
        .map_err(|err| MainError::CompilationError(format!("Code generation error: {}", err)))?;

    // Create executable name from source file name
    let executable_name = filename.trim_end_matches(".prim");
    let obj_filename = format!("{}.o", executable_name);

    // Write object code to file
    fs::write(&obj_filename, &object_code)?;

    // Link with GCC to get C runtime library
    let link_output = Command::new("gcc")
        .args([&obj_filename, "-o", executable_name])
        .output()
        .map_err(|err| MainError::LinkingError(format!("Error running linker: {}. Make sure GNU binutils (ld) is installed and in your PATH", err)))?;

    if link_output.status.success() {
        println!("Successfully built executable: {}", executable_name);

        // Clean up object file
        if let Err(err) = fs::remove_file(&obj_filename) {
            eprintln!(
                "Warning: Could not clean up object file {}: {}",
                obj_filename, err
            );
        }
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&link_output.stderr);
        Err(MainError::LinkingError(format!(
            "Linking failed: {}",
            stderr
        )))
    }
}

fn run_program(filename: &str) -> Result<i32, MainError> {
    let program = compile_source(filename)?;

    // Generate object code directly using Cranelift
    let object_code = generate_object_code(&program)
        .map_err(|err| MainError::CompilationError(format!("Code generation error: {}", err)))?;

    // Write object code to temporary file
    let obj_filename = "temp_output.o";
    let executable_name = "temp_output";

    fs::write(obj_filename, &object_code)?;

    // Link with GCC to get C runtime library
    let link_output = Command::new("gcc")
        .args([obj_filename, "-o", executable_name])
        .output()
        .map_err(|err| MainError::LinkingError(format!("Error running linker: {}. Make sure GNU binutils (ld) is installed and in your PATH", err)))?;

    if !link_output.status.success() {
        let stderr = String::from_utf8_lossy(&link_output.stderr);
        return Err(MainError::LinkingError(format!(
            "Linking failed: {}",
            stderr
        )));
    }

    // Run the program immediately
    let run_result = Command::new(format!("./{}", executable_name))
        .output()
        .map_err(|err| MainError::ExecutionError(format!("Error running program: {}", err)))?;

    // Print program output
    print!("{}", String::from_utf8_lossy(&run_result.stdout));
    if !run_result.stderr.is_empty() {
        eprint!("{}", String::from_utf8_lossy(&run_result.stderr));
    }

    // Clean up temporary files
    let _ = fs::remove_file(obj_filename);
    let _ = fs::remove_file(executable_name);

    // Return the same exit code as the executed program
    Ok(run_result.status.code().unwrap_or(0))
}

fn compile_source(filename: &str) -> Result<prim_parse::Program, MainError> {
    // Read the source file
    let source_code = fs::read_to_string(filename).map_err(MainError::IoError)?;

    // Parse the source code
    parse(&source_code).map_err(|err| MainError::CompilationError(err.to_string()))
}
