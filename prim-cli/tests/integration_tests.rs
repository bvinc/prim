//! Integration tests for the CLI binary.
//!
//! These tests spawn the prim binary as a subprocess to verify:
//! - CLI argument parsing
//! - PRIM_ROOT environment variable handling
//! - Stdlib and runtime library discovery
//! - Error messages and exit codes

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::tempdir_in;
mod common;
use common::staged_prim_root;

fn prim_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_prim"))
}

fn target_tempdir() -> tempfile::TempDir {
    let target_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .join("target")
        .join("tests-tmp");
    fs::create_dir_all(&target_dir).expect("create target tests tmp");
    tempdir_in(target_dir).expect("create tempdir in target")
}

#[test]
fn test_cli_build_command() {
    let prim_root = staged_prim_root();
    let temp = target_tempdir();

    let test_program = "import std.io.println_i32\nfn main() { println_i32(42) }";
    let test_path = temp.path().join("test_cli_build.prim");
    fs::write(&test_path, test_program).expect("Failed to write test file");

    let output = Command::new(prim_bin())
        .args(["build", test_path.to_string_lossy().as_ref()])
        .env("PRIM_ROOT", prim_root.to_string_lossy().as_ref())
        .output()
        .expect("Failed to execute build command");

    assert!(
        output.status.success(),
        "Build command failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let executable_path = temp.path().join("test_cli_build");
    assert!(executable_path.exists(), "Executable was not created");

    let run_output = Command::new(&executable_path)
        .output()
        .expect("Failed to run generated executable");

    assert!(
        run_output.status.success(),
        "Generated executable failed to run"
    );
    assert_eq!(String::from_utf8_lossy(&run_output.stdout).trim(), "42");
}

#[test]
fn test_cli_run_command() {
    let prim_root = staged_prim_root();
    let temp = target_tempdir();

    let test_program = "import std.io.println_i32\nfn main() { println_i32(123) }";
    let test_path = temp.path().join("test_cli_run.prim");
    fs::write(&test_path, test_program).expect("Failed to write test file");

    let output = Command::new(prim_bin())
        .args(["run", test_path.to_string_lossy().as_ref()])
        .env("PRIM_ROOT", prim_root.to_string_lossy().as_ref())
        .output()
        .expect("Failed to execute run command");

    assert!(
        output.status.success(),
        "Run command failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "123");
}

#[test]
fn test_cli_help_command() {
    let output = Command::new(prim_bin())
        .args(["help"])
        .output()
        .expect("Failed to execute help command");

    assert!(output.status.success(), "Help command failed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Prim Programming Language Compiler"));
    assert!(stdout.contains("build"));
    assert!(stdout.contains("run"));
}

#[test]
fn test_cli_help_flag() {
    let output = Command::new(prim_bin())
        .args(["--help"])
        .output()
        .expect("Failed to execute --help");

    assert!(output.status.success(), "--help flag failed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Prim Programming Language Compiler"));
}

#[test]
fn test_cli_no_args_shows_help() {
    let output = Command::new(prim_bin())
        .output()
        .expect("Failed to execute with no args");

    // Should exit with non-zero but show help
    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("USAGE:"));
}

#[test]
fn test_cli_unknown_command() {
    let output = Command::new(prim_bin())
        .args(["unknown_command"])
        .output()
        .expect("Failed to execute unknown command");

    assert!(!output.status.success(), "Unknown command should fail");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Unknown command: unknown_command"));
}

#[test]
fn test_cli_missing_prim_root() {
    let temp = target_tempdir();
    let test_program = "import std.io.println_i32\nfn main() { println_i32(1) }";
    let test_path = temp.path().join("test_missing_root.prim");
    fs::write(&test_path, test_program).expect("Failed to write test file");

    // Run without PRIM_ROOT set (and clear it if inherited)
    let output = Command::new(prim_bin())
        .args(["run", test_path.to_string_lossy().as_ref()])
        .env_remove("PRIM_ROOT")
        .output()
        .expect("Failed to execute run command");

    // Should fail because PRIM_ROOT is not set
    assert!(!output.status.success(), "Should fail without PRIM_ROOT");
}

#[test]
fn test_cli_missing_source_file() {
    let prim_root = staged_prim_root();

    let output = Command::new(prim_bin())
        .args(["run", "/nonexistent/path/to/file.prim"])
        .env("PRIM_ROOT", prim_root.to_string_lossy().as_ref())
        .output()
        .expect("Failed to execute run command");

    assert!(!output.status.success(), "Should fail with missing file");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("IO error") || stderr.contains("No such file"),
        "Expected IO error message, got: {}",
        stderr
    );
}

#[test]
fn test_cli_build_missing_arg() {
    let output = Command::new(prim_bin())
        .args(["build"])
        .output()
        .expect("Failed to execute build without arg");

    assert!(!output.status.success(), "Build without arg should fail");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Usage:"), "Expected usage message");
}

#[test]
fn test_cli_run_missing_arg() {
    let output = Command::new(prim_bin())
        .args(["run"])
        .output()
        .expect("Failed to execute run without arg");

    assert!(!output.status.success(), "Run without arg should fail");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Usage:"), "Expected usage message");
}
