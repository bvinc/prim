# Prim Language Test Suite

This directory contains the official test programs for the Prim language, along
with their expected outputs. The suite is **auto-discovered**: `prim-cli/build.rs`
scans this directory for every `.prim` file with a sibling `.expected`, and
generates one `#[test]` per pair (via `include!(.../test_programs_cases.rs)` in
`prim-cli/tests/test_programs.rs`). Adding a test is just adding the two files —
no registration.

## Test File Format

- `.prim` files contain Prim source code
- `.expected` files contain the expected output when the program runs, or a failure marker:
  - `PARSE_ERROR` — the program should fail to parse
  - `COMPILE_ERROR` — the program should fail to compile
  - `RUNTIME_ERROR` — the program should compile but fail at runtime
  - `ERROR: <message>` — the program should fail (compile or runtime) with an error containing `<message>`
- `ignored.txt` lists program stems (one per line) that are compiled but skipped by the runner.

## Running Tests

```bash
# Build + run the whole suite (each .prim program is a #[test])
cargo test -p prim --test test_programs

# All workspace tests
cargo test --workspace

# Run a specific program manually (staged PRIM_ROOT is handled for you in tests)
cargo run -p prim -- run test_programs/basic_hello.prim
```

The test harness stages `prim-std/src/std` into a temporary `PRIM_ROOT`
(`prim-cli/tests/common.rs`), so programs can `import std.*` exactly as a
staged CLI build would.

## Coverage

The suite exercises, among others:

- expressions, arithmetic, precedence, and literals
- `let` bindings and type inference/annotations
- structs, enums, `match` (read/mut/own arm bindings), tuples, arrays
- functions, methods, and `read`/`mut`/`own` parameter modes
- ownership: moves, `Drop`/RAII, borrow-escape/mut-alias/mode-mismatch rejections
- traits and dynamic dispatch, generics and monomorphization
- the allocator (basic, reuse, growth, stress, trees)
- green threads (`spawn`, `yield`, the runtime scheduler)
- modules and imports (multi-directory programs)
- error conditions (type mismatches, undefined variables, missing `main`, …)

The `.prim`/`.expected` files themselves are the source of truth for what is
covered; `KNOWN_ISSUES.md` tracks known gaps.
