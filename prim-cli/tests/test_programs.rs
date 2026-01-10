mod common;
use common::staged_prim_root;
use prim::compile_and_run_with_root;
use std::fs;
use std::path::Path;

fn run_test_program(prim_path: &Path, expected_path: &Path) {
    let prim_root = staged_prim_root();

    // Read expected output
    let expected = fs::read_to_string(expected_path)
        .unwrap_or_else(|_| panic!("Failed to read expected file: {}", expected_path.display()))
        .trim()
        .to_string();

    // Compile and run in-process with explicit prim_root
    let result =
        compile_and_run_with_root(prim_path.to_str().expect("valid path"), Some(&prim_root));

    // Handle different expected outcomes
    if let Some(message) = expected.strip_prefix("ERROR: ") {
        match result {
            Ok(output) => {
                // Program ran, but we expected a compile error - check if it might be a runtime error
                if output.exit_code != 0 && output.stderr.contains(message) {
                    // Runtime error matched
                    return;
                }
                panic!(
                    "Program {} should have failed but succeeded with output: {}",
                    prim_path.display(),
                    output.stdout
                );
            }
            Err(err) => {
                let err_msg = err.to_string();
                assert!(
                    err_msg.contains(message),
                    "Expected error message '{}' in error, got: {}",
                    message,
                    err_msg
                );
            }
        }
    } else if expected == "PARSE_ERROR" {
        assert!(
            result.is_err(),
            "Program {} should have failed to parse but succeeded",
            prim_path.display()
        );
    } else if expected == "COMPILE_ERROR" {
        assert!(
            result.is_err(),
            "Program {} should have failed to compile but succeeded",
            prim_path.display()
        );
    } else if expected == "RUNTIME_ERROR" {
        match result {
            Ok(output) => {
                assert!(
                    output.exit_code != 0,
                    "Program {} should have failed at runtime but exited with code 0",
                    prim_path.display()
                );
            }
            Err(_) => {
                // Compile error is also acceptable for RUNTIME_ERROR expectation
                // (the test is saying "this should fail somewhere")
            }
        }
    } else {
        // Should succeed and produce expected output
        match result {
            Ok(output) => {
                if output.exit_code != 0 {
                    panic!(
                        "Program {} failed with exit code {}.\nStdout: {}\nStderr: {}",
                        prim_path.display(),
                        output.exit_code,
                        output.stdout,
                        output.stderr
                    );
                }

                let actual = output.stdout.trim().to_string();
                assert_eq!(
                    actual,
                    expected,
                    "Program {} produced wrong output.\nExpected: '{}'\nActual: '{}'",
                    prim_path.display(),
                    expected,
                    actual
                );
            }
            Err(err) => {
                panic!(
                    "Program {} failed unexpectedly: {}",
                    prim_path.display(),
                    err
                );
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/test_programs_cases.rs"));
