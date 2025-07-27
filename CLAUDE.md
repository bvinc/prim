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
# Compile and run a Prim program
cargo run tests/data/example.prim

# This will:
# 1. Parse tests/data/example.prim
# 2. Generate output.o (object file)
# 3. Link with GCC to create ./output executable
# 4. Run ./output and show the result
```

### Test Programs
All example Prim programs are in `tests/data/`:
- `example.prim` - Complex expression (outputs 25)
- `add_test.prim` - Simple addition (outputs 5) 
- `simple.prim` - Basic test case
- And several others for various language features

## Code Quality

The project maintains high code quality standards:

- **Zero compiler warnings** (enforced)
- **Zero clippy warnings** (enforced, with `uninlined_format_args` allowed via workspace lints)
- **Consistent formatting** via rustfmt
- **Comprehensive error handling** throughout the pipeline

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

## Implementation Notes

The current implementation provides a solid foundation for expanding the Prim language with additional features, better error messages, and potentially new target platforms.