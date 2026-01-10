use prim::{RunError, compile_and_run, compile_source, link_executable};
use prim_codegen::generate_object_code;
use std::env;
use std::fs;
use std::time::Instant;

#[derive(Debug)]
enum MainError {
    InsufficientArguments,
    InvalidUsage(String),
    UnknownCommand(String),
    Run(RunError),
}

impl std::fmt::Display for MainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MainError::InsufficientArguments => write!(f, "Insufficient arguments provided"),
            MainError::InvalidUsage(usage) => write!(f, "{}", usage),
            MainError::UnknownCommand(cmd) => write!(f, "Unknown command: {}", cmd),
            MainError::Run(err) => write!(f, "{}", err),
        }
    }
}

impl std::error::Error for MainError {}

impl From<RunError> for MainError {
    fn from(err: RunError) -> Self {
        MainError::Run(err)
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
    let compile_start = Instant::now();
    let hir = compile_source(filename)?;
    let compile_dur = compile_start.elapsed();

    let link_start = Instant::now();
    let object_code = generate_object_code(&hir)
        .map_err(|err| RunError::CompilationError(format!("Code generation error: {}", err)))?;

    let executable_name = filename.trim_end_matches(".prim");
    let obj_filename = format!("{}.o", executable_name);

    fs::write(&obj_filename, &object_code).map_err(RunError::IoError)?;
    link_executable(&obj_filename, executable_name)?;

    let link_dur = link_start.elapsed();
    println!("Successfully built executable: {}", executable_name);
    println!(
        "[timing] compile: {} ms, link: {} ms",
        compile_dur.as_millis(),
        link_dur.as_millis()
    );

    // Clean up object file
    if let Err(err) = fs::remove_file(&obj_filename) {
        eprintln!(
            "Warning: Could not clean up object file {}: {}",
            obj_filename, err
        );
    }
    Ok(())
}

fn run_program(filename: &str) -> Result<i32, MainError> {
    let output = compile_and_run(filename)?;

    print!("{}", output.stdout);
    if !output.stderr.is_empty() {
        eprint!("{}", output.stderr);
    }

    Ok(output.exit_code)
}
