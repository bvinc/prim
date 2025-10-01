use prim_codegen::generate_object_code;
use prim_parse::parse;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
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
    let (program, source) = compile_source(filename)?;

    // Generate object code directly using Cranelift
    let object_code = generate_object_code(&program, &source)
        .map_err(|err| MainError::CompilationError(format!("Code generation error: {}", err)))?;

    // Create executable name from source file name
    let executable_name = filename.trim_end_matches(".prim");
    let obj_filename = format!("{}.o", executable_name);

    // Write object code to file
    fs::write(&obj_filename, &object_code)?;

    // Ensure runtime is built and link it so it provides the C `main` symbol
    let rt_lib = ensure_runtime_staticlib()?;
    let link_output = Command::new("gcc")
        .args([&obj_filename, rt_lib.to_string_lossy().as_ref(), "-o", executable_name])
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
    let (program, source) = compile_source(filename)?;

    // Generate object code directly using Cranelift
    let object_code = generate_object_code(&program, &source)
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
    let rt_lib = ensure_runtime_staticlib()?;
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

fn compile_source(path: &str) -> Result<(prim_parse::Program, String), MainError> {
    let p = Path::new(path);
    if p.is_dir() {
        compile_module_dir(p)
    } else {
        // Single file path
        let source_code = fs::read_to_string(path).map_err(MainError::IoError)?;
        let program =
            parse(&source_code).map_err(|err| MainError::CompilationError(err.to_string()))?;
        Ok((program, source_code))
    }
}

fn compile_module_dir(dir: &Path) -> Result<(prim_parse::Program, String), MainError> {
    // Determine module root: if this is a cmd/ directory, the root is its parent
    let module_root = if dir.file_name().map_or(false, |n| n == "cmd") {
        dir.parent().unwrap_or(dir)
    } else {
        dir
    };

    // Collect all .prim files (sorted for determinism)
    let mut files: Vec<PathBuf> = fs::read_dir(dir)
        .map_err(MainError::IoError)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map_or(false, |ext| ext == "prim"))
        .collect();
    files.sort();

    if files.is_empty() {
        return Err(MainError::CompilationError(
            "No .prim files found in module directory".into(),
        ));
    }

    // Verify all files declare the same module name and that it is `main` (binary)
    let mut module_name: Option<String> = None;
    let mut stripped_sources = Vec::new();
    let mut import_names: Vec<String> = Vec::new();

    for file in &files {
        let src = fs::read_to_string(file).map_err(MainError::IoError)?;
        let program = prim_parse::parse_unit(&src)
            .map_err(|err| MainError::CompilationError(format!("{}: {}", file.display(), err)))?;
        let this_name = program
            .module_name
            .as_ref()
            .map(|span| span.text(&src).to_string())
            .ok_or_else(|| {
                MainError::CompilationError(format!(
                    "{}: missing 'mod <name>' declaration at top of file",
                    file.display()
                ))
            })?;
        match &module_name {
            None => module_name = Some(this_name),
            Some(prev) if prev == &this_name => {}
            Some(prev) => {
                return Err(MainError::CompilationError(format!(
                    "Module name mismatch: {} declares '{}', expected '{}'",
                    file.display(),
                    this_name,
                    prev
                )));
            }
        }
        // Gather imports from this file
        // Pull imports from parsed unit and convert to string with '.' separator
        for p in &program.imports {
            import_names.push(p.mangle(&src, "."));
        }
        stripped_sources.push(strip_module_and_imports(&src)?);
    }

    let module_name = module_name.unwrap();
    if module_name != "main" {
        return Err(MainError::CompilationError(format!(
            "Binary module must be named 'main', found '{}'",
            module_name
        )));
    }

    // Resolve imports recursively within module_root siblings, detect cycles, and concatenate
    // Also prepare a stdlib search root at <workspace>/prim-std/src for `import std.*`.
    let cli_manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(cli_manifest_dir).parent().ok_or_else(|| {
        MainError::IoError(std::io::Error::new(
            std::io::ErrorKind::Other,
            "invalid workspace root",
        ))
    })?;
    let std_root = workspace_root.join("prim-std").join("src");
    let mut visited = std::collections::HashSet::<String>::new();
    let mut stack = Vec::<String>::new();
    let mut dep_sources: Vec<String> = Vec::new();
    for imp in import_names {
        resolve_module_recursive(
            module_root,
            &std_root,
            &imp,
            &mut visited,
            &mut stack,
            &mut dep_sources,
        )?;
    }

    // Concatenate dependency sources first, then this module's sources
    let mut combined_source = String::new();
    if !dep_sources.is_empty() {
        combined_source.push_str(&dep_sources.join("\n"));
        combined_source.push('\n');
    }
    combined_source.push_str(&stripped_sources.join("\n"));

    let program =
        parse(&combined_source).map_err(|err| MainError::CompilationError(err.to_string()))?;

    Ok((program, combined_source))
}

fn strip_module_and_imports(src: &str) -> Result<String, MainError> {
    use prim_tok::{TokenKind, Tokenizer};
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer
        .tokenize()
        .map_err(|e| MainError::CompilationError(e.to_string()))?;

    let mut i = 0usize;
    // Skip leading newlines
    while i < tokens.len() && matches!(tokens[i].kind, TokenKind::Newline) {
        i += 1;
    }

    // Optional mod header
    if i + 1 < tokens.len() && matches!(tokens[i].kind, TokenKind::Mod) {
        i += 1; // 'mod'
        if i < tokens.len() && matches!(tokens[i].kind, TokenKind::Identifier) {
            i += 1; // module name
        }
        // consume terminators
        while i < tokens.len()
            && matches!(tokens[i].kind, TokenKind::Semicolon | TokenKind::Newline)
        {
            i += 1;
        }
    }

    // Zero or more imports
    loop {
        // Skip optional leading newlines between imports
        while i < tokens.len() && matches!(tokens[i].kind, TokenKind::Newline) {
            i += 1;
        }
        if i + 1 < tokens.len() && matches!(tokens[i].kind, TokenKind::Import) {
            i += 1; // 'import'
            if i < tokens.len() && matches!(tokens[i].kind, TokenKind::Identifier) {
                i += 1; // module name
            }
            while i < tokens.len()
                && matches!(tokens[i].kind, TokenKind::Semicolon | TokenKind::Newline)
            {
                i += 1;
            }
        } else {
            break;
        }
    }

    let start = tokens.get(i).map(|t| t.position).unwrap_or(src.len());
    Ok(src[start..].to_string())
}

// extract_imports removed: imports parsed by prim-parse

fn resolve_module_recursive(
    module_root: &Path,
    std_root: &Path,
    name: &str,
    visited: &mut std::collections::HashSet<String>,
    stack: &mut Vec<String>,
    out_sources: &mut Vec<String>,
) -> Result<(), MainError> {
    let qual_name = name.replace('.', "__");
    if stack.contains(&qual_name) {
        let mut cycle = stack.clone();
        cycle.push(qual_name.clone());
        return Err(MainError::CompilationError(format!(
            "Import cycle detected: {}",
            cycle.join(" -> ")
        )));
    }
    if !visited.insert(qual_name.clone()) {
        return Ok(());
    }
    stack.push(qual_name.clone());

    // Compute module directory by joining segments, with special-case for std.*
    let mut parts = name.split('.');
    let first = parts.next().unwrap_or("");
    let mut mod_dir = if first == "std" {
        std_root.to_path_buf()
    } else {
        module_root.to_path_buf()
    };
    // include the first segment as well (std/..., or non-std root/<first>)
    if !first.is_empty() {
        mod_dir = mod_dir.join(first);
    }
    for s in parts {
        mod_dir = mod_dir.join(s);
    }
    let mut files: Vec<PathBuf> = fs::read_dir(&mod_dir)
        .map_err(MainError::IoError)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map_or(false, |ext| ext == "prim"))
        .collect();
    files.sort();
    if files.is_empty() {
        return Err(MainError::CompilationError(format!(
            "Imported module '{}' not found at {}",
            name,
            mod_dir.display()
        )));
    }

    let mut module_name: Option<String> = None;
    let mut imports = Vec::new();
    let mut body_sources = Vec::new();
    for file in &files {
        let src = fs::read_to_string(file).map_err(MainError::IoError)?;
        let program = prim_parse::parse_unit(&src)
            .map_err(|err| MainError::CompilationError(format!("{}: {}", file.display(), err)))?;
        let this_name = program
            .module_name
            .as_ref()
            .map(|span| span.text(&src).to_string())
            .ok_or_else(|| {
                MainError::CompilationError(format!(
                    "{}: missing 'mod <name>' declaration at top of file",
                    file.display()
                ))
            })?;
        match &module_name {
            None => module_name = Some(this_name.clone()),
            Some(prev) if prev == &this_name => {}
            Some(prev) => {
                return Err(MainError::CompilationError(format!(
                    "Module name mismatch: {} declares '{}', expected '{}'",
                    file.display(),
                    this_name,
                    prev
                )));
            }
        }
        // Pull imports from this imported module
        for p in &program.imports {
            imports.push(p.mangle(&src, "."));
        }
        // Strip headers then prefix using fully-qualified module name
        let stripped = strip_module_and_imports(&src)?;
        body_sources.push(prefix_functions_with_module(&stripped, &qual_name)?);
    }
    for imp in imports {
        resolve_module_recursive(module_root, std_root, &imp, visited, stack, out_sources)?;
    }
    out_sources.push(body_sources.join("\n"));
    stack.pop();
    Ok(())
}

fn prefix_functions_with_module(src: &str, module_name: &str) -> Result<String, MainError> {
    use prim_tok::{TokenKind, Tokenizer};
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer
        .tokenize()
        .map_err(|e| MainError::CompilationError(e.to_string()))?;

    let mut out = String::with_capacity(src.len() + 32);
    let mut i = 0usize;
    let mut cursor = 0usize;
    while i < tokens.len() {
        let t = &tokens[i];
        if matches!(t.kind, TokenKind::Fn) {
            // emit up to 'fn'
            out.push_str(&src[cursor..t.position]);
            // emit 'fn'
            out.push_str("fn");
            cursor = t.position + t.text.len();
            // skip whitespace between fn and name by copying as-is until next token
            i += 1;
            if i < tokens.len() {
                let name_tok = &tokens[i];
                if matches!(name_tok.kind, TokenKind::Identifier) {
                    // emit whitespace between and then the prefixed name
                    out.push_str(&src[cursor..name_tok.position]);
                    out.push_str(module_name);
                    out.push_str("__");
                    out.push_str(name_tok.text);
                    cursor = name_tok.position + name_tok.text.len();
                    i += 1;
                    continue;
                }
            }
        }
        i += 1;
    }
    // emit remainder
    out.push_str(&src[cursor..]);
    Ok(out)
}

// Build prim-rt (if needed) and return path to its static library.
fn ensure_runtime_staticlib() -> Result<PathBuf, MainError> {
    // Determine workspace root and cargo profile used to build this binary
    // CARGO_MANIFEST_DIR points to prim-cli; workspace root is one level up
    let cli_manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(cli_manifest_dir).parent().ok_or_else(|| {
        MainError::IoError(std::io::Error::new(
            std::io::ErrorKind::Other,
            "invalid workspace root",
        ))
    })?;
    // Prefer runtime env var when available; default to "debug"
    let profile_dir = std::env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());

    // Expected staticlib path
    // Cargo normalizes crate name dashes to underscores in artifact names
    let staticlib_name = if cfg!(target_os = "windows") {
        "prim_rt.lib"
    } else {
        "libprim_rt.a"
    };
    let staticlib_path = workspace_root
        .join("target")
        .join(&profile_dir)
        .join(staticlib_name);

    if !staticlib_path.exists() {
        // Build prim-rt in the appropriate profile
        let mut args = vec!["build", "-p", "prim-rt", "--features", "rt-entry"]; // default dev (debug)
        if profile_dir == "release" {
            args.push("--release");
        }
        let status = Command::new("cargo")
            .args(args)
            .current_dir(workspace_root)
            .status()
            .map_err(|e| {
                MainError::LinkingError(format!("failed to invoke cargo to build runtime: {}", e))
            })?;
        if !status.success() {
            return Err(MainError::LinkingError("building prim-rt failed".into()));
        }
    }

    if !staticlib_path.exists() {
        return Err(MainError::LinkingError(format!(
            "runtime static library not found at {}",
            staticlib_path.display()
        )));
    }

    Ok(staticlib_path)
}
