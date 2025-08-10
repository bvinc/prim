# Repository Guidelines

## Project Structure & Module Organization
- Root is a Rust workspace (`Cargo.toml`) with members: `prim-cli` (binary), `prim-parse`, `prim-tok`, `prim-codegen`.
- Source lives under each crate’s `src/` (e.g., `prim-parse/src/lib.rs`). Shared error types often in `error.rs`.
- CLI integration tests are in `prim-cli/tests/`; sample programs + expected outputs in `prim-cli/test_programs/`.
- Specs and docs: `LANGUAGE_SPEC.md`, `README.md`.

## Build, Test, and Development Commands
- Build all crates: `cargo build` (from repo root).
- Run the CLI: `cargo run --bin prim -- help`.
- Compile and run a program: `cargo run --bin prim -- run path/to/file.prim`.
- Compile only: `cargo run --bin prim -- build path/to/file.prim` (produces an executable next to the `.prim`).
- Test everything: `cargo test` (CLI integration + unit tests).
- Test only CLI suite: `cargo test -p prim` (in `prim-cli/`).
- Lint: `cargo clippy --all-targets --all-features`.
- Format: `cargo fmt`.

Note: The CLI links with `gcc`; ensure GNU binutils are in `PATH`.

## Coding Style & Naming Conventions
- Language: Rust 2024 edition; 4-space indentation; `rustfmt` enforced (`cargo fmt`).
- Naming: `snake_case` for functions/files, `CamelCase` for types, `SCREAMING_SNAKE_CASE` for consts.
- Modules: keep crate boundaries clear; parsing in `prim-parse`, tokenization in `prim-tok`, codegen/JIT in `prim-codegen`, user-facing tooling in `prim-cli`.
- Lints: workspace Clippy config in root `Cargo.toml`; prefer fixing warnings over allow-listing.

## Testing Guidelines
- Framework: standard Rust `#[test]` with integration tests in `prim-cli/tests/`.
- Program tests: add `.prim` and matching `.expected` under `prim-cli/test_programs/`. Update `tests/test_programs.rs` to register the new pair; the auto-discovery test will verify file pairs exist.
- Run fast local checks: `cargo test -q`. Keep tests deterministic; assert on exact stdout where applicable.

## Commit & Pull Request Guidelines
- Commits: concise, present-tense, imperative (e.g., "add unary dereference operator"). Group logical changes; reference crates when helpful (e.g., `parse:`).
- PRs: include a clear summary, rationale, and before/after examples (show `.prim` snippet and expected output). Link related issues and note any follow-ups.

## Environment Tips
- Dev shells: optional Nix via `nix develop` or `direnv allow` (see `flake.nix`, `.envrc`).
- Toolchain: stable Rust, plus `gcc` for linking.
