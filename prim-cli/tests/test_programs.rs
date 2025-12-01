mod common;
use common::staged_prim_root;
use std::fs;
use std::path::Path;
use std::process::Command;

fn run_test_program(prim_path: &Path, expected_path: &Path) {
    // Read expected output
    let expected = fs::read_to_string(expected_path)
        .unwrap_or_else(|_| panic!("Failed to read expected file: {}", expected_path.display()))
        .trim()
        .to_string();

    // Run the prim program
    let prim_root = staged_prim_root();
    let output = Command::new("cargo")
        .args(["run", "--", "run", prim_path.to_string_lossy().as_ref()])
        .env("PRIM_ROOT", prim_root.to_string_lossy().as_ref())
        .output()
        .unwrap_or_else(|_| {
            panic!(
                "Failed to execute prim compiler for: {}",
                prim_path.display()
            )
        });

    // Handle different expected outcomes
    if let Some(message) = expected.strip_prefix("ERROR: ") {
        assert!(
            !output.status.success(),
            "Program {} should have failed but succeeded",
            prim_path.display()
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains(message),
            "Expected error message '{}' in stderr, got: {}",
            message,
            stderr
        );
    } else if expected == "PARSE_ERROR" {
        // Should fail to parse
        assert!(
            !output.status.success(),
            "Program {} should have failed to parse but succeeded",
            prim_path.display()
        );
    } else if expected == "COMPILE_ERROR" {
        assert!(
            !output.status.success(),
            "Program {} should have failed to compile but succeeded",
            prim_path.display()
        );
    } else if expected == "RUNTIME_ERROR" {
        // Should compile but fail at runtime
        assert!(
            !output.status.success(),
            "Program {} should have failed at runtime but succeeded",
            prim_path.display()
        );
    } else {
        // Should succeed and produce expected output
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            panic!(
                "Program {} failed unexpectedly.\nStdout: {}\nStderr: {}",
                prim_path.display(),
                stdout,
                stderr
            );
        }

        let actual = String::from_utf8_lossy(&output.stdout).trim().to_string();
        assert_eq!(
            actual,
            expected,
            "Program {} produced wrong output.\nExpected: '{}'\nActual: '{}'",
            prim_path.display(),
            expected,
            actual
        );
    }
}

include!(concat!(env!("OUT_DIR"), "/test_programs_cases.rs"));
