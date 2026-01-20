//! Prim compiler library interface.
//!
//! This module exposes the core compilation and execution functionality
//! for use by both the CLI binary and tests.

use prim_codegen::generate_object_code;
use prim_compiler::{CompileError, FileId, HirProgram, LoadError, LoadOptions, SourceMap, compile};
use prim_tok::{Span, byte_offset_to_line_col};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;

/// Output from running a compiled program.
#[derive(Debug)]
pub struct RunOutput {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
}

/// Errors that can occur during compilation or execution.
#[derive(Debug)]
pub enum RunError {
    CompilationError(String),
    IoError(std::io::Error),
    LinkingError(String),
    ExecutionError(String),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunError::CompilationError(msg) => write!(f, "Compilation error: {}", msg),
            RunError::IoError(err) => write!(f, "IO error: {}", err),
            RunError::LinkingError(msg) => write!(f, "Linking error: {}", msg),
            RunError::ExecutionError(msg) => write!(f, "Execution error: {}", msg),
        }
    }
}

impl std::error::Error for RunError {}

impl From<std::io::Error> for RunError {
    fn from(err: std::io::Error) -> Self {
        RunError::IoError(err)
    }
}

/// Compile and run a Prim source file, capturing the output.
///
/// Uses PRIM_ROOT from environment or exe-relative discovery.
pub fn compile_and_run(path: &str) -> Result<RunOutput, RunError> {
    compile_and_run_with_root(path, None)
}

/// Compile and run a Prim source file with an explicit prim root.
///
/// If `prim_root` is None, uses PRIM_ROOT from environment or exe-relative discovery.
pub fn compile_and_run_with_root(
    path: &str,
    prim_root: Option<&Path>,
) -> Result<RunOutput, RunError> {
    let hir = compile_source_with_root(path, prim_root)?;

    let object_code = generate_object_code(&hir)
        .map_err(|err| RunError::CompilationError(format!("Code generation error: {}", err)))?;

    // Create temporary files for object code and executable
    let temp_obj = tempfile::Builder::new()
        .suffix(".o")
        .tempfile()
        .map_err(RunError::IoError)?;
    let obj_filename = temp_obj.path().to_string_lossy().to_string();

    let temp_exe_file = tempfile::Builder::new()
        .tempfile()
        .map_err(RunError::IoError)?;
    let temp_exe_path = temp_exe_file.into_temp_path();
    let executable_name = temp_exe_path.to_string_lossy().to_string();

    fs::write(&obj_filename, &object_code)?;
    link_executable_with_root(&obj_filename, &executable_name, prim_root)?;

    let run_result = Command::new(&executable_name)
        .output()
        .map_err(|err| RunError::ExecutionError(format!("Error running program: {}", err)))?;

    Ok(RunOutput {
        stdout: String::from_utf8_lossy(&run_result.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&run_result.stderr).into_owned(),
        exit_code: run_result.status.code().unwrap_or(1),
    })
}

/// Compile source code to HIR.
pub fn compile_source(path: &str) -> Result<HirProgram, RunError> {
    compile_source_with_root(path, None)
}

/// Compile source code to HIR with an explicit prim root.
pub fn compile_source_with_root(
    path: &str,
    prim_root: Option<&Path>,
) -> Result<HirProgram, RunError> {
    let options = LoadOptions {
        prim_root: prim_root.map(Path::to_path_buf),
        ..Default::default()
    };
    let (source_map, result) = compile(path, options);
    result.map_err(|err| format_compile_error(path, &source_map, err))
}

/// Format a compile error with source location information.
pub fn format_compile_error(
    fallback_path: &str,
    source_map: &Arc<SourceMap>,
    err: CompileError,
) -> RunError {
    let msg = match &err {
        CompileError::Load(LoadError::Parse(e)) => {
            // Parse errors don't have FileId, use fallback path
            let source = std::fs::read_to_string(fallback_path).unwrap_or_default();
            format_with_span(fallback_path, &source, e.span(), e)
        }
        CompileError::Load(e) => e.to_string(),
        CompileError::TypeCheck(e) => {
            // Convert prim_hir::FileId to prim_compiler::FileId via inner u32
            format_error_with_file(source_map, FileId(e.file.0), e.span, e, fallback_path)
        }
        CompileError::Resolve(errors) => errors
            .iter()
            .map(|e| format_error_with_file(source_map, e.file(), e.span(), e, fallback_path))
            .collect::<Vec<_>>()
            .join("\n"),
        CompileError::Lowering(errors) => errors
            .iter()
            .map(|e| {
                format_error_with_file(source_map, FileId(e.file().0), e.span(), e, fallback_path)
            })
            .collect::<Vec<_>>()
            .join("\n"),
    };
    RunError::CompilationError(msg)
}

fn format_error_with_file(
    source_map: &SourceMap,
    file_id: FileId,
    span: Span,
    msg: &impl std::fmt::Display,
    fallback_path: &str,
) -> String {
    let (path, source) = match source_map.get_path(file_id) {
        Some(p) => {
            let source = source_map.read_source(file_id).unwrap_or_default();
            (p.to_string_lossy().into_owned(), source)
        }
        None => {
            let source = std::fs::read_to_string(fallback_path).unwrap_or_default();
            (fallback_path.to_string(), source)
        }
    };
    format_with_span(&path, &source, Some(span), msg)
}

fn format_with_span(
    path: &str,
    source: &str,
    span: Option<Span>,
    msg: &impl std::fmt::Display,
) -> String {
    match span {
        Some(s) => {
            let (line, col) = byte_offset_to_line_col(source, s.start());
            format!("{}:{}:{}: {}", path, line, col, msg)
        }
        None => format!("{}: {}", path, msg),
    }
}

/// Link object code into an executable.
pub fn link_executable(obj_path: &str, exe_path: &str) -> Result<(), RunError> {
    link_executable_with_root(obj_path, exe_path, None)
}

/// Link object code into an executable with an explicit prim root.
pub fn link_executable_with_root(
    obj_path: &str,
    exe_path: &str,
    prim_root: Option<&Path>,
) -> Result<(), RunError> {
    let rt_lib = runtime_lib_path_with_root(prim_root)?;
    let output = Command::new("gcc")
        .args([obj_path, rt_lib.to_string_lossy().as_ref(), "-o", exe_path])
        .output()
        .map_err(|err| {
            RunError::LinkingError(format!(
                "Error running linker: {}. Make sure gcc is installed and in your PATH",
                err
            ))
        })?;

    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(RunError::LinkingError(format!(
            "Linking failed: {}",
            stderr
        )))
    }
}

/// Return a path to the runtime static library.
pub fn runtime_lib_path() -> Result<PathBuf, RunError> {
    runtime_lib_path_with_root(None)
}

/// Return a path to the runtime static library with an explicit prim root.
pub fn runtime_lib_path_with_root(prim_root: Option<&Path>) -> Result<PathBuf, RunError> {
    let root = match prim_root {
        Some(r) => r.to_path_buf(),
        None => {
            prim_compiler::prim_root().map_err(|e| RunError::CompilationError(e.to_string()))?
        }
    };

    let staticlib_name = if cfg!(target_os = "windows") {
        "prim_rt.lib"
    } else {
        "libprim_rt.a"
    };

    let staticlib_path = root.join("lib").join(staticlib_name);
    if !staticlib_path.exists() {
        return Err(RunError::LinkingError(format!(
            "runtime static library not found at {}",
            staticlib_path.display()
        )));
    }

    Ok(staticlib_path)
}
