# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Prim** is a programming language compiler implemented in Rust that compiles Prim source code to native x86-64 executables using the Cranelift code generation backend.

### Design Goals

1. **Simplicity above all.** Less code, fewer concepts, fewer special cases. When in doubt, simplify.
2. **Green threads with copyable stacks.** The runtime will provide lightweight cooperative scheduling with growable, relocatable stacks (copied/moved like Go), enabling millions of concurrent tasks.
3. **No garbage collection.** Memory is managed through ownership, borrowing, and lifetimes.
4. **Strong types with a borrow checker.** Compile-time ownership and lifetime enforcement, similar to Rust but simpler.

### Current Language Features
- **Primitive types**: Integers (u8-u64, i8-i64, usize, isize), floats (f32, f64), booleans
- **Variables**: `let` bindings with optional type annotations, mutability via `let mut`
- **Expressions**: Arithmetic, comparison, literals, variables, field access, blocks
- **Structs**: User-defined types with named fields
- **Functions**: User-defined functions with parameters and return types
- **Control flow**: `if`/`else` expressions, `loop` with `break`, `while`
- **Pointers**: Const and mutable pointers with dereference
- **Modules**: Directory-based modules with selective imports
- **Type inference**: Basic type inference for unspecified types

### Not Yet Implemented
- Ownership and borrowing (move semantics, borrow checker)
- Lifetimes
- Enums and pattern matching
- Traits and methods (parsed but not compiled)
- Generics
- Green thread integration in the language
- Runtime type information for stack relocation

## Architecture

```
prim/
├── prim-cli/           # CLI binary (build/run commands)
├── prim-tok/           # Tokenizer/lexer (source → tokens)
├── prim-parse/         # Parser (tokens → AST)
├── prim-compiler/      # Name resolution, type checking, AST → HIR lowering
├── prim-hir/           # High-level IR definitions and type checker
├── prim-codegen/       # Cranelift code generation (HIR → object code)
├── prim-rt/            # Runtime library (static lib linked into executables)
├── prim-std/           # Standard library (Prim source files)
└── prim-cli/test_programs/  # Test .prim programs with .expected output files
```

### Compilation Pipeline
1. **Tokenization** (`prim-tok`): Source code → tokens using zero-copy string slices
2. **Parsing** (`prim-parse`): Tokens → AST via recursive descent
3. **Lowering** (`prim-compiler`): AST → HIR with name resolution and type checking
4. **Code Generation** (`prim-codegen`): HIR → native object code via Cranelift IR
5. **Linking**: Object code + runtime library → executable via system linker

## Development Commands

```bash
# Build everything
cargo build --workspace

# Run tests
cargo test --workspace

# Run a single crate's tests
cargo test -p prim-parse

# Clippy (must pass with zero warnings)
cargo clippy --workspace --all-targets --all-features -- -D warnings

# Format
cargo fmt --all
```

### Using the Compiler

```bash
# Build and stage distribution (sets up PRIM_ROOT structure)
./build.sh              # debug build
./build.sh --release    # release build

# Run from staged distribution
export PRIM_ROOT=target/debug/dist
target/debug/dist/bin/prim run path/to/file.prim

# Or run directly during development
cargo run -p prim -- build prim-cli/test_programs/basic_hello.prim
cargo run -p prim -- run prim-cli/test_programs/basic_hello.prim
```

The `PRIM_ROOT` environment variable tells the CLI where to find the standard library and runtime.

## Git Hooks

Enable the pre-commit hook to enforce formatting, clippy, and tests:

```bash
git config core.hooksPath .githooks
```

The hook runs: `cargo fmt --check`, `cargo clippy -D warnings`, `cargo test`.

## Pre-commit Requirements

Before committing:
1. `cargo fmt --all`
2. `cargo clippy --workspace --all-targets --all-features -- -D warnings` (zero warnings)
3. `cargo test --workspace`

## Git Commit Guidelines

- **No type prefixes** (no "feat:", "fix:", etc.)
- **Imperative mood**: "add feature" not "added feature"
- **First line under 50 characters**
- **No AI attributions or signatures**

Examples:
```
add support for floating point literals
fix segfault in printf integration
```
