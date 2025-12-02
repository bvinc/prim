use prim_codegen::generate_object_code;
use prim_compiler::{load_program, type_check_program};
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

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
    let (program, parse_ms, type_ms) = compile_source(filename)?;
    let link_start = Instant::now();

    // Generate object code directly using Cranelift
    let object_code = generate_object_code(&program)
        .map_err(|err| MainError::CompilationError(format!("Code generation error: {}", err)))?;

    // Create executable name from source file name
    let executable_name = filename.trim_end_matches(".prim");
    let obj_filename = format!("{}.o", executable_name);

    // Write object code to file
    fs::write(&obj_filename, &object_code)?;

    // Ensure runtime is built and link it so it provides the C `main` symbol
    let rt_lib = runtime_lib_path()?;
    let link_output = Command::new("gcc")
        .args([&obj_filename, rt_lib.to_string_lossy().as_ref(), "-o", executable_name])
        .output()
        .map_err(|err| MainError::LinkingError(format!("Error running linker: {}. Make sure GNU binutils (ld) is installed and in your PATH", err)))?;

    if link_output.status.success() {
        let link_ms = link_start.elapsed().as_millis();
        println!("Successfully built executable: {}", executable_name);
        println!(
            "[timing] parse: {} ms, typecheck: {} ms, link: {} ms",
            parse_ms, type_ms, link_ms
        );

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
    let (program, _, _) = compile_source(filename)?;

    // Generate object code directly using Cranelift
    let object_code = generate_object_code(&program)
        .map_err(|err| MainError::CompilationError(format!("Code generation error: {}", err)))?;

    // Create temporary files for object code and executable
    let temp_obj = tempfile::Builder::new()
        .suffix(".o")
        .tempfile()
        .map_err(MainError::IoError)?;
    let obj_filename = temp_obj.path().to_string_lossy().to_string();

    // Create a unique temporary file path for the executable
    // We use tempfile to get a unique name, then convert to a persistent path
    let temp_exe_file = tempfile::Builder::new()
        .tempfile()
        .map_err(MainError::IoError)?;
    let temp_exe_path = temp_exe_file.into_temp_path();
    let executable_name = temp_exe_path.to_string_lossy().to_string();

    // Write object code to temporary file
    fs::write(&obj_filename, &object_code)?;

    // Ensure runtime is built and link it so it provides the C `main` symbol
    let rt_lib = runtime_lib_path()?;
    let link_output = Command::new("gcc")
        .args([&obj_filename, rt_lib.to_string_lossy().as_ref(), "-o", &executable_name])
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
    let run_result = Command::new(&executable_name)
        .output()
        .map_err(|err| MainError::ExecutionError(format!("Error running program: {}", err)))?;

    // Print program output
    print!("{}", String::from_utf8_lossy(&run_result.stdout));
    if !run_result.stderr.is_empty() {
        eprint!("{}", String::from_utf8_lossy(&run_result.stderr));
    }

    // Temporary files will be automatically cleaned up when temp_obj and temp_exe_path drop

    // Return the same exit code as the executed program
    Ok(run_result.status.code().unwrap_or(0))
}

fn compile_source(path: &str) -> Result<(prim_compiler::Program, u128, u128), MainError> {
    let parse_start = Instant::now();
    let mut loaded =
        load_program(path).map_err(|err| MainError::CompilationError(err.to_string()))?;
    let parse_ms = parse_start.elapsed().as_millis();

    let type_start = Instant::now();
    type_check_program(&mut loaded.program)
        .map_err(|err| MainError::CompilationError(format!("Type check error: {}", err)))?;
    let type_ms = type_start.elapsed().as_millis();

    Ok((loaded.program, parse_ms, type_ms))
}

/// Return a path to the runtime static library
fn runtime_lib_path() -> Result<PathBuf, MainError> {
    let prim_root =
        prim_compiler::prim_root().map_err(|e| MainError::CompilationError(e.to_string()))?;

    let staticlib_name = if cfg!(target_os = "windows") {
        "prim_rt.lib"
    } else {
        "libprim_rt.a"
    };

    let staticlib_path = prim_root.join("lib").join(staticlib_name);
    if !staticlib_path.exists() {
        return Err(MainError::LinkingError(format!(
            "runtime static library not found at {}",
            staticlib_path.display()
        )));
    }

    Ok(staticlib_path)
}
