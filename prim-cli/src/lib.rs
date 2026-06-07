//! Prim compiler library interface.
//!
//! This module exposes the core compilation and execution functionality
//! for use by both the CLI binary and tests.

use prim_compiler::hir;
use prim_compiler::{CompileError, FileId, LoadError, LoadOptions, SourceMap, compile};
use prim_tok::{Span, byte_offset_to_line_col};
use prim_wasm::generate_wasm;
use std::fs;
use std::path::Path;
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
    ExecutionError(String),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunError::CompilationError(msg) => write!(f, "Compilation error: {}", msg),
            RunError::IoError(err) => write!(f, "IO error: {}", err),
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
/// Compiles to wasm and runs via wasmtime.
/// If `prim_root` is None, uses PRIM_ROOT from environment or exe-relative discovery.
pub fn compile_and_run_with_root(
    path: &str,
    prim_root: Option<&Path>,
) -> Result<RunOutput, RunError> {
    let hir = compile_source_with_root(path, prim_root)?;

    let wasm_bytes = generate_wasm(&hir)
        .map_err(|err| RunError::CompilationError(format!("Code generation error: {}", err)))?;

    let temp_wasm = tempfile::Builder::new()
        .suffix(".wasm")
        .tempfile()
        .map_err(RunError::IoError)?;

    fs::write(temp_wasm.path(), &wasm_bytes)?;

    let run_result = Command::new("wasmtime")
        .arg("run")
        // The scheduler in `_start` uses cont.new/resume from the
        // stack-switching proposal, typed `(ref $main_cont)` from the
        // function-references proposal, and a tag declaration from the
        // exceptions proposal (tags were introduced there; stack-switching
        // re-uses the encoding).
        .arg("-W")
        .arg("stack-switching=y")
        .arg("-W")
        .arg("function-references=y")
        .arg("-W")
        .arg("exceptions=y")
        .arg(temp_wasm.path())
        .output()
        .map_err(|err| {
            RunError::ExecutionError(format!(
                "Error running wasmtime: {}. Make sure wasmtime is installed.",
                err
            ))
        })?;

    Ok(RunOutput {
        stdout: String::from_utf8_lossy(&run_result.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&run_result.stderr).into_owned(),
        exit_code: run_result.status.code().unwrap_or(1),
    })
}

/// Compile source code to HIR.
pub fn compile_source(path: &str) -> Result<hir::Program, RunError> {
    compile_source_with_root(path, None)
}

/// Compile source code to HIR with an explicit prim root.
pub fn compile_source_with_root(
    path: &str,
    prim_root: Option<&Path>,
) -> Result<hir::Program, RunError> {
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
            format_error_with_file(source_map, e.file, e.span, e, fallback_path)
        }
        CompileError::Resolve(errors) => errors
            .iter()
            .map(|e| format_error_with_file(source_map, e.file(), e.span(), e, fallback_path))
            .collect::<Vec<_>>()
            .join("\n"),
        CompileError::Lowering(errors) => errors
            .iter()
            .map(|e| format_error_with_file(source_map, e.file(), e.span(), e, fallback_path))
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
            let source = source_map
                .read_source(file_id)
                .map(|s| s.to_string())
                .unwrap_or_default();
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

/// Generate a wasm binary from a source file.
pub fn generate_wasm_binary(path: &str) -> Result<Vec<u8>, RunError> {
    generate_wasm_binary_with_root(path, None)
}

/// Generate a wasm binary from a source file with an explicit prim root.
pub fn generate_wasm_binary_with_root(
    path: &str,
    prim_root: Option<&Path>,
) -> Result<Vec<u8>, RunError> {
    let hir = compile_source_with_root(path, prim_root)?;
    generate_wasm(&hir)
        .map_err(|err| RunError::CompilationError(format!("Code generation error: {}", err)))
}
