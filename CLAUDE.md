# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with the Prim programming language compiler.

## Project Overview

**Prim** is a programming language compiler implemented in Rust with a complete toolchain that compiles Prim source code to native x86-64 executables. The compiler uses the Cranelift code generation backend for professional-quality optimized machine code output.

### Language Features
- **Primitive types**: Integers (u8, u16, u32, u64, i8, i16, i32, i64), floats
- **Variables**: `let` bindings with optional type annotations
- **Expressions**: Arithmetic operations (+, -, *, ==), literals, variables
- **Functions**: `println()` function for output
- **Type inference**: Basic type inference for unspecified types

### Example Prim Program
```prim
let x: u32 = 5
let y: u32 = 10
let result = x + y * 2
println(result)  // outputs: 25
```

## Architecture

The compiler is structured as a Rust workspace with four main components:

```
prim/                    # Main compiler binary
├── prim-tok/           # Tokenizer/lexer crate
├── prim-parse/         # Recursive descent parser crate
├── prim-codegen/       # Cranelift code generation crate
└── tests/data/         # Test Prim programs
```

### Compilation Pipeline
1. **Tokenization** (`prim-tok`): Source code → tokens using zero-copy string slices
2. **Parsing** (`prim-parse`): Tokens → Abstract Syntax Tree (AST) via recursive descent
3. **Code Generation** (`prim-codegen`): AST → native object code via Cranelift IR
4. **Linking**: Object code → executable via GCC (links with C runtime for printf)

### Technical Details
- **Backend**: Cranelift compiler infrastructure for professional code generation
- **Target**: Linux x86-64 native executables
- **Output**: Direct object code generation (no intermediate assembly)
- **Runtime**: Links with C standard library for I/O operations

## Development Environment

This project uses Nix flakes for reproducible development environments:

- **Setup**: Run `nix develop` or use direnv with `.envrc`
- **Rust version**: 1.88+ (configured in flake.nix)
- **Tools**: rustc, cargo, clippy, rustfmt, gdb, gcc (for linking)

## Development Commands

### Building and Testing
```bash
# Build the entire workspace
cargo build --workspace

# Run clippy (no warnings allowed)
cargo clippy --all-targets --all-features

# Run tests
cargo test --workspace

# Format code
cargo fmt --all
```

### Using the Compiler
```bash
# Build a Prim program to executable
cargo run -- build tests/data/example.prim
# Creates executable: tests/data/example

# Compile and run a Prim program
cargo run -- run tests/data/example.prim
# Compiles and immediately runs the program

# Show help
cargo run -- help
```

### Test Programs
All example Prim programs are in `tests/data/`:
- `example.prim` - Complex expression (outputs 25)
- `add_test.prim` - Simple addition (outputs 5) 
- `simple.prim` - Basic test case
- And several others for various language features

## Code Quality

The project maintains high code quality standards:

- **Zero compiler warnings** (enforced - code must never have warnings)
- **Zero clippy warnings** (enforced - code must never have clippy warnings)
- **Consistent formatting** via rustfmt (code must always be formatted with `cargo fmt` before committing)
- **Comprehensive error handling** throughout the pipeline

### Pre-commit Requirements

Before committing any code changes, you MUST:

1. **Format the code**: Run `cargo fmt --all` to ensure consistent formatting
2. **Check for warnings**: Run `cargo build` and ensure zero compiler warnings
3. **Check clippy**: Run `cargo clippy --all-targets --all-features` and ensure zero clippy warnings
4. **Run tests**: Run `cargo test` and ensure all tests pass

Any commit that introduces warnings or clippy warnings will be rejected.

## Troubleshooting

### Common Issues
- **"undefined reference to printf"**: Ensure GCC is available for linking
- **Segmentation fault**: Usually indicates missing C runtime setup (should not happen with current implementation)
- **Parse errors**: Check Prim syntax matches supported language features

### Dependencies
The compiler requires:
- **Rust 1.88+** with Cargo
- **GCC** for linking with C runtime
- **Cranelift 0.112** crates for code generation

## Git Commit Guidelines

When making commits to this project, follow these conventions:

### Commit Format
- **No type prefixes** (no "feat:", "fix:", etc.)
- **Imperative mood**: "add feature" not "added feature"  
- **First line under 50 characters** when possible
- **Never include Claude signatures** or AI attributions
- **Descriptive body** for complex changes (wrap at 72 chars)

### Examples
```
Good:
add support for floating point literals
refactor error handling to use custom types
fix segfault in printf integration

Bad:
feat: add support for floating point literals  
Added support for floating point literals
🤖 Generated with Claude Code
```

### Multi-line Format
```
add comprehensive error handling system

- Replace all panic!() calls with Result returns
- Add dedicated error.rs files for each compiler crate  
- Implement Display trait for user-friendly messages
- Add test coverage for error conditions
```

## Implementation Notes

The current implementation provides a solid foundation for expanding the Prim language with additional features, better error messages, and potentially new target platforms.