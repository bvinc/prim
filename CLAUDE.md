# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Prim** is a programming language compiler implemented in Rust that compiles Prim source code to WebAssembly (wasm32 + WASI) modules, runnable via wasmtime.

### Design Goals

1. **A language that is useful for AI.** High assurances, high performance, fast
   compilation, and **local reasoning**: a function's signature states
   everything it can read, mutate, or consume, so any piece of code can be
   understood and verified in isolation, without tracing effects across the
   program. Memory safety, data-race freedom, and deterministic resource
   handling are enforced at compile time, so generated code can be trusted
   without a runtime safety net. No GC pauses and value semantics keep
   performance predictable. A small, simple compiler compiles quickly, so the
   AI code-and-verify loop stays tight.
2. **No garbage collection.** Memory is managed through ownership and
   compile-time move checking — not a GC. The programmer always knows when
   memory is freed; there are no pauses and no tracing.
3. **Green threads.** Lightweight cooperative scheduling built on the WebAssembly
   stack-switching (typed-continuations) proposal: each task is a continuation
   whose stack the engine grows and manages, enabling millions of concurrent
   tasks without OS thread overhead.
4. **Strong types with control of aliasing.** Ownership is enforced at compile
   time (use-after-move, use-after-free, and data races are rejected), with
   **second-class references** for cross-call access: `read`/`mut`/`own` are
   *binding modes* (`fn len(read v: Vec[T])`, `match mut e { Some(mut v) => ... }`,
   `f(own x)`), never types — no reference can be stored, returned, or escape
   its call, so shared mutable state exists only where it is explicitly allowed.

The memory model is specified in [`MEMORY_MODEL.md`](MEMORY_MODEL.md).

### Current Language Features
- **Primitive types**: Integers (u8-u64, i8-i64, usize, isize), floats (f32, f64), booleans, raw pointers (`*const T` / `*mut T`)
- **Bindings**: `let` with optional type annotations; `let mut` mutable locals; `let own` explicit move
- **Values**: structs, enums, strings, `Vec`, `Array` (const-generic); move semantics with structural copy (scalars + pointers)
- **Functions & methods**: parameters with `read`/`mut`/`own` modes (`self` receivers included), return types, type inference
- **Control flow**: `if`/`else`, `loop`, `while`, `for x in range`, `break`, `return`
- **Pattern matching**: `match` with `read`/`mut`/`own` arm bindings and inferred consumption
- **Traits**: dynamic dispatch (`dyn` trait objects), generic impls
- **Generics**: generic functions, structs, enums, and impls; const generics on `Array` and functions
- **Modules**: directory-based modules with selective imports
- **Memory**: ownership + moves, `Drop`/RAII with recursive field drops, second-class `read`/`mut`/`own` modes
- **Concurrency**: green threads — `spawn`, cooperative `yield`, a multi-task scheduler with blocking park/poll

### Not Yet Implemented
See [`KNOWN_ISSUES.md`](KNOWN_ISSUES.md) for the authoritative list. Highlights:
- Unmarked consumption (make `mut` the *only* call-site mark)
- Callbacks / first-class function values (borrow-in-a-scope via closures)
- Identities: arenas with generation-checked ids, `Shared[T]`, splitters
- Enum recursive drop (an enum's active variant payload is not yet dropped — sound, but leaky)
- Borrow-to-trait-view coercion (`read T` → `read Trait` parameter): re-wrapping a borrow in a trait object is fine when it stays second-class, but the checker currently rejects every coercion of a borrow
- Conditional trait impl bounds (`impl[T: Debug] Debug for Pair[T]`); `Debug`/`Display` derive covers structs, not yet generics

## Architecture

```
prim/
├── prim-cli/           # CLI binary (build/run commands, std resolution, tests)
├── prim-tok/           # Tokenizer/lexer (source → tokens)
├── prim-parse/         # Parser (tokens → AST)
├── prim-compiler/      # Name resolution, type checking, AST → HIR lowering
├── prim-wasm/          # HIR → wasm32+WASI module via wasm-encoder
├── prim-std/           # Standard library (Prim source, not a Rust crate)
└── prim-cli/test_programs/  # Test .prim programs with .expected output files
```

The Rust workspace has five members: `prim-cli` (binary `prim`), `prim-tok`,
`prim-parse`, `prim-compiler`, `prim-wasm`. `prim-std` is Prim source resolved
via `PRIM_ROOT` at compile time — it is not a Rust crate.

### Compilation Pipeline
1. **Tokenization** (`prim-tok`): Source code → tokens using zero-copy string slices
2. **Parsing** (`prim-parse`): Tokens → AST via recursive descent
3. **Lowering** (`prim-compiler`): AST → HIR with name resolution
4. **Type checking** (`prim-compiler::hir::typecheck`): infer and resolve types on HIR in place
5. **Code Generation** (`prim-wasm`): HIR → wasm binary; runtime helpers (println_*, allocator) are inlined as wasm functions using WASI fd_write directly
6. **Execution**: `prim run` shells out to `wasmtime run`; `prim build` writes the `.wasm` file

There is no C linker in the pipeline — `prim-wasm` emits wasm directly via
wasm-encoder, so only `wasmtime` (on `PATH`) is needed at run time.

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

The `PRIM_ROOT` environment variable tells the CLI where to find the standard
library (`prim-std/src/std` is staged under it); running compiled output also
requires `wasmtime` on `PATH`.

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
