use indexmap::IndexSet;
use prim_codegen::generate_object_code;
use prim_parse::{self, parse};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

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

#[derive(Debug, Clone)]
struct ImportRequest {
    module: Vec<String>,
    coverage: ImportCoverage,
}

#[derive(Debug, Clone)]
enum ImportCoverage {
    All,
    Symbols(IndexSet<String>),
}

#[derive(Debug, Clone)]
struct DefinitionFragment {
    name: String,
    kind: DefinitionKind,
    text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DefinitionKind {
    Struct,
    Function,
    Trait,
    Impl,
}

#[derive(Debug, Clone)]
struct ModuleCacheEntry {
    fragments: Vec<DefinitionFragment>,
    imports: Vec<ImportRequest>,
}

fn trace_imports_enabled() -> bool {
    static TRACE: OnceLock<bool> = OnceLock::new();
    *TRACE.get_or_init(|| env::var("PRIM_TRACE_IMPORTS").is_ok())
}

fn trace_imports(message: &str) {
    if trace_imports_enabled() {
        eprintln!("[imports] {}", message);
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
        // Directory module: use module-aware compilation (supports mod/import and std.* via prim-std)
        return compile_module_dir(p);
    }

    // Single file path: read user source
    let user_source = fs::read_to_string(path).map_err(MainError::IoError)?;

    // Parse unit to discover imports (no need for a module header)
    let unit = prim_parse::parse_unit(&user_source)
        .map_err(|err| MainError::CompilationError(err.to_string()))?;

    // Resolve imports recursively, including std.* from prim-std/src
    let cli_manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(cli_manifest_dir)
        .parent()
        .ok_or_else(|| MainError::IoError(std::io::Error::other("invalid workspace root")))?;
    let std_root = workspace_root.join("prim-std").join("src");
    let module_root = p.parent().unwrap_or_else(|| Path::new("."));

    let planned = plan_import_requests(&unit.imports, &user_source, module_root, &std_root)?;
    let mut import_index = HashMap::new();
    let mut import_requests = Vec::new();

    // Always include standard prelude imports first
    merge_import_request(
        &mut import_requests,
        &mut import_index,
        vec!["std".to_string(), "string".to_string()],
        ImportCoverage::All,
    );
    merge_import_request(
        &mut import_requests,
        &mut import_index,
        vec!["std".to_string(), "io".to_string()],
        ImportCoverage::All,
    );

    for ImportRequest { module, coverage } in planned {
        merge_import_request(&mut import_requests, &mut import_index, module, coverage);
    }

    let mut visited_modules = HashSet::new();
    let mut visited_symbols = HashSet::new();
    let mut stack = Vec::<String>::new();
    let mut dep_sources: Vec<String> = Vec::new();
    let mut module_cache: HashMap<String, ModuleCacheEntry> = HashMap::new();

    for request in &import_requests {
        resolve_module_recursive(
            module_root,
            &std_root,
            &request.module,
            &request.coverage,
            &mut visited_modules,
            &mut visited_symbols,
            &mut stack,
            &mut dep_sources,
            &mut module_cache,
        )?;
    }

    // Minimal prelude providing print_str via libc write for single-file programs.
    // This keeps unqualified print_str available without explicit imports.
    const PRELUDE: &str = r#"
import std.string
import std.io
"#;

    // Strip headers from the user source (mod/import) and combine with deps
    let stripped_user = strip_to_body(&unit, &user_source);
    let mut combined_source = String::new();
    // Always include prelude first for single-file mode
    combined_source.push_str(PRELUDE);
    combined_source.push('\n');
    if !dep_sources.is_empty() {
        combined_source.push_str(&dep_sources.join("\n\n"));
        combined_source.push('\n');
    }
    combined_source.push_str(&stripped_user);

    // Parse the combined source code
    let program =
        parse(&combined_source).map_err(|err| MainError::CompilationError(err.to_string()))?;
    Ok((program, combined_source))
}

fn compile_module_dir(dir: &Path) -> Result<(prim_parse::Program, String), MainError> {
    // Determine module root: if this is a cmd/ directory, the root is its parent
    let module_root = if dir.file_name().is_some_and(|n| n == "cmd") {
        dir.parent().unwrap_or(dir)
    } else {
        dir
    };

    let cli_manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(cli_manifest_dir)
        .parent()
        .ok_or_else(|| MainError::IoError(std::io::Error::other("invalid workspace root")))?;
    let std_root = workspace_root.join("prim-std").join("src");

    // Collect all .prim files (sorted for determinism)
    let mut files: Vec<PathBuf> = fs::read_dir(dir)
        .map_err(MainError::IoError)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "prim"))
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
    let mut import_requests = Vec::new();
    let mut import_index = HashMap::new();

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
        let planned = plan_import_requests(&program.imports, &src, module_root, &std_root)?;
        for ImportRequest { module, coverage } in planned {
            merge_import_request(&mut import_requests, &mut import_index, module, coverage);
        }
        stripped_sources.push(strip_to_body(&program, &src));
    }

    let module_name = module_name.unwrap();
    if module_name != "main" {
        return Err(MainError::CompilationError(format!(
            "Binary module must be named 'main', found '{}'",
            module_name
        )));
    }

    // Resolve imports recursively within module_root siblings, detect cycles, and concatenate.
    let mut visited_modules = HashSet::new();
    let mut visited_symbols = HashSet::new();
    let mut stack = Vec::<String>::new();
    let mut dep_sources: Vec<String> = Vec::new();
    let mut module_cache: HashMap<String, ModuleCacheEntry> = HashMap::new();
    for request in &import_requests {
        resolve_module_recursive(
            module_root,
            &std_root,
            &request.module,
            &request.coverage,
            &mut visited_modules,
            &mut visited_symbols,
            &mut stack,
            &mut dep_sources,
            &mut module_cache,
        )?;
    }

    // Concatenate dependency sources first, then this module's sources
    let mut combined_source = String::new();
    if !dep_sources.is_empty() {
        combined_source.push_str(&dep_sources.join("\n\n"));
        combined_source.push('\n');
    }
    if !stripped_sources.is_empty() {
        combined_source.push_str(&stripped_sources.join("\n"));
    }

    let program =
        parse(&combined_source).map_err(|err| MainError::CompilationError(err.to_string()))?;

    Ok((program, combined_source))
}

fn module_search_path(module_root: &Path, std_root: &Path, segments: &[String]) -> PathBuf {
    if segments.first().is_some_and(|s| s == "std") {
        let mut path = std_root.join("std");
        for seg in segments.iter().skip(1) {
            path.push(seg);
        }
        path
    } else {
        let mut path = module_root.to_path_buf();
        for seg in segments {
            path.push(seg);
        }
        path
    }
}

fn module_exists(module_root: &Path, std_root: &Path, segments: &[String]) -> bool {
    if segments.is_empty() {
        return false;
    }
    let search_path = module_search_path(module_root, std_root, segments);
    if search_path.is_dir() {
        fs::read_dir(&search_path)
            .ok()
            .into_iter()
            .flat_map(|iter| iter.filter_map(|e| e.ok()))
            .any(|entry| entry.path().extension().is_some_and(|ext| ext == "prim"))
    } else {
        search_path.with_extension("prim").exists()
    }
}

fn merge_import_request(
    acc: &mut Vec<ImportRequest>,
    index: &mut HashMap<String, usize>,
    module: Vec<String>,
    coverage: ImportCoverage,
) {
    let key = module.join(".");
    if let Some(&slot) = index.get(&key) {
        match (&mut acc[slot].coverage, coverage) {
            (ImportCoverage::All, _) => {}
            (_, ImportCoverage::All) => {
                acc[slot].coverage = ImportCoverage::All;
            }
            (ImportCoverage::Symbols(existing), ImportCoverage::Symbols(symbols)) => {
                existing.extend(symbols);
            }
        }
    } else {
        index.insert(key, acc.len());
        acc.push(ImportRequest { module, coverage });
    }
}

fn plan_import_requests(
    imports: &[prim_parse::ImportDecl],
    source: &str,
    module_root: &Path,
    std_root: &Path,
) -> Result<Vec<ImportRequest>, MainError> {
    let mut acc = Vec::new();
    let mut index = HashMap::new();
    for decl in imports {
        let (module, coverage) = convert_import_decl(decl, source, module_root, std_root)?;
        merge_import_request(&mut acc, &mut index, module, coverage);
    }
    Ok(acc)
}

fn convert_import_decl(
    decl: &prim_parse::ImportDecl,
    source: &str,
    module_root: &Path,
    std_root: &Path,
) -> Result<(Vec<String>, ImportCoverage), MainError> {
    let mut module_segments = decl.module_segments(source);
    if module_segments.is_empty() {
        return Err(MainError::CompilationError(
            "Import must specify at least one segment".into(),
        ));
    }

    match &decl.selector {
        prim_parse::ImportSelector::All => {
            if module_exists(module_root, std_root, &module_segments) {
                return Ok((module_segments, ImportCoverage::All));
            }
            if let Some(trailing) = &decl.trailing_symbol {
                if module_segments.len() < 2 {
                    let module_display = module_segments.join(".");
                    let search_path = module_search_path(module_root, std_root, &module_segments);
                    return Err(MainError::CompilationError(format!(
                        "Imported module '{}' not found at {}",
                        module_display,
                        search_path.display()
                    )));
                }
                let symbol_name = trailing.text(source).to_string();
                module_segments.pop();
                if !module_exists(module_root, std_root, &module_segments) {
                    let module_display = module_segments.join(".");
                    let search_path = module_search_path(module_root, std_root, &module_segments);
                    return Err(MainError::CompilationError(format!(
                        "Imported module '{}' not found at {}",
                        module_display,
                        search_path.display()
                    )));
                }
                let mut symbols = IndexSet::new();
                symbols.insert(symbol_name);
                Ok((module_segments, ImportCoverage::Symbols(symbols)))
            } else {
                let module_display = module_segments.join(".");
                let search_path = module_search_path(module_root, std_root, &module_segments);
                Err(MainError::CompilationError(format!(
                    "Imported module '{}' not found at {}",
                    module_display,
                    search_path.display()
                )))
            }
        }
        prim_parse::ImportSelector::Named(_) => {
            if !module_exists(module_root, std_root, &module_segments) {
                let module_display = module_segments.join(".");
                let search_path = module_search_path(module_root, std_root, &module_segments);
                return Err(MainError::CompilationError(format!(
                    "Imported module '{}' not found at {}",
                    module_display,
                    search_path.display()
                )));
            }
            let mut symbols = IndexSet::new();
            if let Some(names) = decl.selector_names(source) {
                for name in names {
                    symbols.insert(name);
                }
            }
            Ok((module_segments, ImportCoverage::Symbols(symbols)))
        }
    }
}

fn extract_definitions(program: &prim_parse::Program, source: &str) -> Vec<DefinitionFragment> {
    let mut annotated = Vec::new();

    for s in &program.structs {
        annotated.push((
            s.span.start(),
            DefinitionFragment {
                name: s.name.text(source).to_string(),
                kind: DefinitionKind::Struct,
                text: s.span.text(source).to_string(),
            },
        ));
    }

    for f in &program.functions {
        annotated.push((
            f.span.start(),
            DefinitionFragment {
                name: f.name.text(source).to_string(),
                kind: DefinitionKind::Function,
                text: f.span.text(source).to_string(),
            },
        ));
    }

    for t in &program.traits {
        annotated.push((
            t.span.start(),
            DefinitionFragment {
                name: t.name.text(source).to_string(),
                kind: DefinitionKind::Trait,
                text: t.span.text(source).to_string(),
            },
        ));
    }

    for im in &program.impls {
        let trait_name = im.trait_name.text(source).to_string();
        let struct_name = im.struct_name.text(source).to_string();
        annotated.push((
            im.span.start(),
            DefinitionFragment {
                name: format!("impl {} for {}", trait_name, struct_name),
                kind: DefinitionKind::Impl,
                text: im.span.text(source).to_string(),
            },
        ));
    }

    annotated.sort_by_key(|(start, _)| *start);
    annotated
        .into_iter()
        .map(|(_, fragment)| fragment)
        .collect()
}

fn strip_to_body(program: &prim_parse::Program, source: &str) -> String {
    extract_definitions(program, source)
        .into_iter()
        .map(|fragment| fragment.text)
        .filter(|text| !text.trim().is_empty())
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn load_module_entry<'a>(
    module_root: &Path,
    std_root: &Path,
    module_segments: &[String],
    cache: &'a mut HashMap<String, ModuleCacheEntry>,
) -> Result<&'a ModuleCacheEntry, MainError> {
    let module_key = module_segments.join(".");
    if !cache.contains_key(&module_key) {
        let search_path = module_search_path(module_root, std_root, module_segments);
        let mut files: Vec<PathBuf> = if search_path.is_dir() {
            fs::read_dir(&search_path)
                .map_err(MainError::IoError)?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter(|path| path.extension().is_some_and(|ext| ext == "prim"))
                .collect()
        } else {
            let file_candidate = search_path.with_extension("prim");
            if file_candidate.exists() {
                vec![file_candidate]
            } else {
                Vec::new()
            }
        };
        files.sort();
        if files.is_empty() {
            return Err(MainError::CompilationError(format!(
                "Imported module '{}' not found at {}",
                module_key,
                search_path.display()
            )));
        }

        let mut module_name: Option<String> = None;
        let mut fragments = Vec::new();
        let mut imports = Vec::new();
        let mut import_index = HashMap::new();

        for file in &files {
            let src = fs::read_to_string(file).map_err(MainError::IoError)?;
            let program = prim_parse::parse_unit(&src).map_err(|err| {
                MainError::CompilationError(format!("{}: {}", file.display(), err))
            })?;
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
                Some(existing) if existing == &this_name => {}
                Some(existing) => {
                    return Err(MainError::CompilationError(format!(
                        "Module name mismatch: {} declares '{}', expected '{}'",
                        file.display(),
                        this_name,
                        existing
                    )));
                }
            }

            let defs = extract_definitions(&program, &src);
            fragments.extend(defs);

            let planned = plan_import_requests(&program.imports, &src, module_root, std_root)?;
            for ImportRequest { module, coverage } in planned {
                merge_import_request(&mut imports, &mut import_index, module, coverage);
            }
        }

        cache.insert(module_key.clone(), ModuleCacheEntry { fragments, imports });
    }

    Ok(cache.get(&module_key).unwrap())
}

fn resolve_module_recursive(
    module_root: &Path,
    std_root: &Path,
    module_segments: &[String],
    coverage: &ImportCoverage,
    visited_modules: &mut HashSet<String>,
    visited_symbols: &mut HashSet<(String, String)>,
    stack: &mut Vec<String>,
    out_sources: &mut Vec<String>,
    cache: &mut HashMap<String, ModuleCacheEntry>,
) -> Result<(), MainError> {
    let module_key = module_segments.join(".");
    if stack.contains(&module_key) {
        let mut cycle = stack.clone();
        cycle.push(module_key.clone());
        return Err(MainError::CompilationError(format!(
            "Import cycle detected: {}",
            cycle.join(" -> ")
        )));
    }
    stack.push(module_key.clone());

    let imports = {
        let entry = load_module_entry(module_root, std_root, module_segments, cache)?;
        entry.imports.clone()
    };

    for request in &imports {
        resolve_module_recursive(
            module_root,
            std_root,
            &request.module,
            &request.coverage,
            visited_modules,
            visited_symbols,
            stack,
            out_sources,
            cache,
        )?;
    }

    let entry = load_module_entry(module_root, std_root, module_segments, cache)?;

    match coverage {
        ImportCoverage::All => {
            if visited_modules.insert(module_key.clone()) {
                let mut collected = Vec::new();
                for fragment in &entry.fragments {
                    let key = (module_key.clone(), fragment.name.clone());
                    if visited_symbols.insert(key) {
                        trace_imports(&format!(
                            "symbol {}::{} ({:?})",
                            module_key, fragment.name, fragment.kind
                        ));
                        if !fragment.text.trim().is_empty() {
                            collected.push(fragment.text.clone());
                        }
                    } else {
                        trace_imports(&format!(
                            "symbol {}::{} skipped (already imported)",
                            module_key, fragment.name
                        ));
                    }
                }
                if !collected.is_empty() {
                    trace_imports(&format!("module {} (all)", module_key));
                    out_sources.push(collected.join("\n\n"));
                } else {
                    trace_imports(&format!(
                        "module {} (all) produced no new definitions",
                        module_key
                    ));
                }
            } else {
                trace_imports(&format!("module {} already imported", module_key));
            }
        }
        ImportCoverage::Symbols(symbols) => {
            let mut requested: HashSet<String> = symbols.iter().cloned().collect();
            for fragment in &entry.fragments {
                if requested.contains(&fragment.name) {
                    requested.remove(&fragment.name);
                    let key = (module_key.clone(), fragment.name.clone());
                    if visited_symbols.insert(key.clone()) {
                        trace_imports(&format!(
                            "symbol {}::{} ({:?})",
                            module_key, fragment.name, fragment.kind
                        ));
                        out_sources.push(fragment.text.clone());
                    } else {
                        trace_imports(&format!(
                            "symbol {}::{} skipped (duplicate)",
                            module_key, fragment.name
                        ));
                    }
                }
            }
            if let Some(missing) = requested.into_iter().next() {
                stack.pop();
                let module_display = module_segments.join(".");
                return Err(MainError::CompilationError(format!(
                    "Module '{}' does not define '{}'",
                    module_display, missing
                )));
            }
        }
    }

    stack.pop();
    Ok(())
}

#[allow(dead_code)]
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
            out.push_str(&src[cursor..t.range.start]);
            // emit 'fn'
            out.push_str("fn");
            cursor = t.range.end;
            // skip whitespace between fn and name by copying as-is until next token
            i += 1;
            if i < tokens.len() {
                let name_tok = &tokens[i];
                if matches!(name_tok.kind, TokenKind::Identifier) {
                    // emit whitespace between and then the prefixed name
                    out.push_str(&src[cursor..name_tok.range.start]);
                    out.push_str(module_name);
                    out.push_str("__");
                    out.push_str(&src[name_tok.range.clone()]);
                    cursor = name_tok.range.end;
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
    let workspace_root = Path::new(cli_manifest_dir)
        .parent()
        .ok_or_else(|| MainError::IoError(std::io::Error::other("invalid workspace root")))?;
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

    // Always ensure prim-rt is built. If it's already up-to-date, Cargo will
    // no-op quickly.
    let mut args = vec!["build", "-p", "prim-rt"]; // default dev (debug)
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

    if !staticlib_path.exists() {
        return Err(MainError::LinkingError(format!(
            "runtime static library not found at {}",
            staticlib_path.display()
        )));
    }

    Ok(staticlib_path)
}
