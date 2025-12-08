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
#[ignore]
fn test_cli_build_command() {
    let prim_root = staged_prim_root();
    let temp = target_tempdir();

    let test_program = "fn main() { println(42) }";
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
#[ignore]
fn test_cli_run_command() {
    let prim_root = staged_prim_root();
    let temp = target_tempdir();

    let test_program = "fn main() { println(123) }";
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
