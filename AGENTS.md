# Repository Guidelines

## Project Structure & Module Organization
- Root is a Rust workspace (`Cargo.toml`) with members: `prim-cli` (binary `prim`), `prim-tok`, `prim-parse`, `prim-compiler`, `prim-wasm`.
- `prim-std/` holds the standard library written *in Prim* (resolved via `PRIM_ROOT` at compile time) — it is not a Rust crate.
- Source lives under each crate’s `src/` (e.g., `prim-parse/src/lib.rs`). Shared error types often in `error.rs`.
- CLI integration tests are in `prim-cli/tests/`; sample programs + expected outputs in `prim-cli/test_programs/`.
- Specs and docs: `LANGUAGE_SPEC.md`, `MEMORY_MODEL.md` (the memory model), `KNOWN_ISSUES.md`, `README.md`.

## Build, Test, and Development Commands
- Build all crates: `cargo build` (from repo root).
- Run the CLI: `cargo run --bin prim -- help`.
- Compile and run a program: `cargo run --bin prim -- run path/to/file.prim`.
- Compile only: `cargo run --bin prim -- build path/to/file.prim` (writes a `.wasm` module next to the `.prim`).
- Test everything: `cargo test` (CLI integration + unit tests).
- Test only CLI suite: `cargo test -p prim` (in `prim-cli/`).
- Lint: `cargo clippy --all-targets --all-features`.
- Format: `cargo fmt`.

Note: `prim-wasm` emits wasm directly (no C linker in the pipeline); running compiled output requires `wasmtime` on `PATH`.

## Coding Style & Naming Conventions
- **Simplicity above all.** Simplicity in implementation is the highest
  priority: prefer the plain, obvious solution over the clever one, and write
  textbook, clean code. Avoid unnecessary abstractions, layers, and special
  cases; when in doubt, do the simplest thing that is correct.
- Language: Rust 2024 edition; 4-space indentation; `rustfmt` enforced (`cargo fmt`).
- Naming: `snake_case` for functions/files, `CamelCase` for types, `SCREAMING_SNAKE_CASE` for consts.
- Modules: keep crate boundaries clear; tokenization in `prim-tok`, parsing in `prim-parse`, name resolution/type checking/HIR lowering in `prim-compiler`, wasm codegen in `prim-wasm`, user-facing tooling in `prim-cli`.
- Lints: workspace Clippy config in root `Cargo.toml`; prefer fixing warnings over allow-listing.

## Testing Guidelines
- Framework: standard Rust `#[test]` with integration tests in `prim-cli/tests/`.
- Program tests: add a `.prim` and matching `.expected` under `prim-cli/test_programs/`. The build script (`prim-cli/build.rs`) auto-discovers every `.prim`/`.expected` pair and generates one `#[test]` per program — no manual registration. (A `.expected` of `PARSE_ERROR` / `COMPILE_ERROR` / `RUNTIME_ERROR` / `ERROR: <message>` asserts failure.)
- Run fast local checks: `cargo test -q`. Keep tests deterministic; assert on exact stdout where applicable.

## Commit & Pull Request Guidelines
- Commits: concise, present-tense, imperative (e.g., "add unary dereference operator"). Group logical changes; reference crates when helpful (e.g., `parse:`).
- PRs: include a clear summary, rationale, and before/after examples (show `.prim` snippet and expected output). Link related issues and note any follow-ups.

## Environment Tips
- Dev shells: optional Nix via `nix develop` or `direnv allow` (see `flake.nix`, `.envrc`).
- Toolchain: stable Rust (pinned by `rust-toolchain.toml` to 1.88), plus `wasmtime` on `PATH` to run compiled output.
- Remote builds: `.rex/` ships task files for a rex tool VM (`.rex/README.md`).
  `build.yaml`/`test.yaml` boot the named VM from the baked project image
  `prim-toolchain-rust188` (Rust 1.88 + wasmtime + warm caches pre-installed),
  so `rex run .rex/build.yaml --name prim` / `.rex/test.yaml` just work in any
  session of this project — no init step. Only run `.rex/init.yaml` (once, on
  a bare VM) if that image is missing or you need to rebuild it.
