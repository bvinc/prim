# Prim Language Test Suite

This directory contains the official test programs for the Prim language, along with their expected outputs.

## Test File Format

- `.prim` files contain Prim source code
- `.expected` files contain the expected output when the program runs
- If a `.expected` file contains `PARSE_ERROR`, the program should fail to parse
- If a `.expected` file contains `COMPILE_ERROR`, the program should fail to compile
- If a `.expected` file contains `RUNTIME_ERROR`, the program should compile but fail at runtime

## Current Test Programs

### Basic Functionality
- `basic_hello.prim` - Simple println with literal
- `arithmetic.prim` - Basic arithmetic with precedence
- `type_annotations.prim` - Explicit type annotations
- `precedence.prim` - Operator precedence (multiplication before addition)
- `parentheses.prim` - Parentheses override precedence
- `loop_break.prim` - `loop {}` with `break` exiting to subsequent statements

### Statement Termination
- `semicolon_termination.prim` - Multiple statements with semicolons
- `newline_termination.prim` - Multiple statements with newlines
- `invalid_no_terminator.prim` - Should fail: statements without terminators

### Functions
- `function_with_params.prim` - User-defined function with parameters and return type

## Running Tests

To validate the implementation against the specification:

```bash
# Run a specific test
cargo run -- run test_programs/basic_hello.prim

# Test that it matches expected output
cargo run -- run test_programs/basic_hello.prim | diff - test_programs/basic_hello.expected
```

## Test Coverage Goals

The test suite should eventually cover:
- [x] Basic expressions and arithmetic
- [x] Operator precedence and associativity
- [x] Variable declarations with and without type annotations
- [x] Function definitions and calls
- [x] Statement termination rules
- [ ] All numeric types (u8, i8, u16, i16, u32, i32, u64, i64, usize, isize, f32, f64)
- [ ] Type inference
- [ ] Error conditions (type mismatches, undefined variables, etc.)
- [ ] Complex nested expressions
- [ ] Multiple functions calling each other
- [ ] Edge cases and boundary conditions
