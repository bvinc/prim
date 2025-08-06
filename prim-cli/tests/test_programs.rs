use std::fs;
use std::path::Path;
use std::process::Command;

/// Generate individual tests for each .prim file in test_programs/
#[cfg(test)]
mod test_programs {
    use super::*;

    macro_rules! test_program {
        ($name:ident, $prim_file:expr, $expected_file:expr) => {
            #[test]
            fn $name() {
                run_test_program($prim_file, $expected_file);
            }
        };
    }

    // Include all test programs here - these will be individual tests
    test_program!(
        basic_hello,
        "test_programs/basic_hello.prim",
        "test_programs/basic_hello.expected"
    );
    test_program!(
        arithmetic,
        "test_programs/arithmetic.prim",
        "test_programs/arithmetic.expected"
    );
    test_program!(
        semicolon_termination,
        "test_programs/semicolon_termination.prim",
        "test_programs/semicolon_termination.expected"
    );
    test_program!(
        newline_termination,
        "test_programs/newline_termination.prim",
        "test_programs/newline_termination.expected"
    );
    test_program!(
        invalid_no_terminator,
        "test_programs/invalid_no_terminator.prim",
        "test_programs/invalid_no_terminator.expected"
    );
    test_program!(
        function_with_params,
        "test_programs/function_with_params.prim",
        "test_programs/function_with_params.expected"
    );
    test_program!(
        type_annotations,
        "test_programs/type_annotations.prim",
        "test_programs/type_annotations.expected"
    );
    test_program!(
        precedence,
        "test_programs/precedence.prim",
        "test_programs/precedence.expected"
    );
    test_program!(
        parentheses,
        "test_programs/parentheses.prim",
        "test_programs/parentheses.expected"
    );
    test_program!(
        comments,
        "test_programs/comments.prim",
        "test_programs/comments.expected"
    );
    test_program!(
        struct_basic,
        "test_programs/struct_basic.prim",
        "test_programs/struct_basic.expected"
    );
}

fn run_test_program(prim_file: &str, expected_file: &str) {
    let prim_path = Path::new(prim_file);
    let expected_path = Path::new(expected_file);

    // Check that both files exist
    assert!(
        prim_path.exists(),
        "Prim file does not exist: {}",
        prim_file
    );
    assert!(
        expected_path.exists(),
        "Expected file does not exist: {}",
        expected_file
    );

    // Read expected output
    let expected = fs::read_to_string(expected_path)
        .unwrap_or_else(|_| panic!("Failed to read expected file: {}", expected_file))
        .trim()
        .to_string();

    // Run the prim program
    let output = Command::new("cargo")
        .args(["run", "--", "run", prim_file])
        .output()
        .unwrap_or_else(|_| panic!("Failed to execute prim compiler for: {}", prim_file));

    // Handle different expected outcomes
    if expected == "PARSE_ERROR" {
        // Should fail to parse
        assert!(
            !output.status.success(),
            "Program {} should have failed to parse but succeeded",
            prim_file
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("parse") || stderr.contains("Parse") || stderr.contains("syntax"),
            "Program {} should have failed with parse error, got: {}",
            prim_file,
            stderr
        );
    } else if expected == "COMPILE_ERROR" {
        // Should fail to compile
        assert!(
            !output.status.success(),
            "Program {} should have failed to compile but succeeded",
            prim_file
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("compile") || stderr.contains("Compile") || stderr.contains("codegen"),
            "Program {} should have failed with compile error, got: {}",
            prim_file,
            stderr
        );
    } else if expected == "RUNTIME_ERROR" {
        // Should compile but fail at runtime
        // This would need more sophisticated handling depending on how runtime errors are reported
        assert!(
            !output.status.success(),
            "Program {} should have failed at runtime but succeeded",
            prim_file
        );
    } else {
        // Should succeed and produce expected output
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            panic!(
                "Program {} failed unexpectedly.\nStdout: {}\nStderr: {}",
                prim_file, stdout, stderr
            );
        }

        let actual = String::from_utf8_lossy(&output.stdout).trim().to_string();
        assert_eq!(
            actual, expected,
            "Program {} produced wrong output.\nExpected: '{}'\nActual: '{}'",
            prim_file, expected, actual
        );
    }
}

/// Auto-discovery test that finds all .prim files and ensures they have .expected files
#[test]
fn test_all_programs_have_expected_files() {
    let test_dir = Path::new("test_programs");
    if !test_dir.exists() {
        panic!("test_programs directory does not exist");
    }

    let entries =
        fs::read_dir(test_dir).unwrap_or_else(|_| panic!("Failed to read test_programs directory"));

    let mut prim_files = Vec::new();
    let mut expected_files = Vec::new();

    for entry in entries {
        let entry = entry.unwrap();
        let path = entry.path();
        if let Some(extension) = path.extension() {
            if extension == "prim" {
                prim_files.push(path.file_stem().unwrap().to_string_lossy().to_string());
            } else if extension == "expected" {
                expected_files.push(path.file_stem().unwrap().to_string_lossy().to_string());
            }
        }
    }

    // Check that every .prim file has a corresponding .expected file
    for prim_file in &prim_files {
        assert!(
            expected_files.contains(prim_file),
            "Missing .expected file for {}.prim",
            prim_file
        );
    }

    // Check that every .expected file has a corresponding .prim file
    for expected_file in &expected_files {
        assert!(
            prim_files.contains(expected_file),
            "Missing .prim file for {}.expected",
            expected_file
        );
    }

    println!(
        "Found {} test programs with expected outputs",
        prim_files.len()
    );
}
