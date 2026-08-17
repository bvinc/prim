Prim Standard Library

Layout:
- `src/std/` contains Prim modules arranged by directory.
- Import modules using dotted paths (e.g., `import std.string`).

Resolution:
- The CLI resolves the standard library through `PRIM_ROOT`: it expects the
  `src/std/` tree under the root, so `import std.io.println` maps to
  `$PRIM_ROOT/src/std/io/*.prim`. `./build.sh` stages `prim-std/src/std` into
  `target/{debug,release}/dist`; the test harness stages it into a temporary
  root (see `prim-cli/tests/common.rs`).

Status:
- Modules: `array`, `convert`, `fmt`, `io`, `mem` (dlmalloc allocator), `ops`,
  `option`, `ptr`, `result`, `rt` (green-thread runtime), `string`, `sys`,
  `time`, `vec`, `wasm`.
- The runtime (`std.rt`) provides cooperative green threads — `spawn`, `yield`,
  a multi-task scheduler, and blocking park/poll — built on wasm continuations.
- `std.mem` is a Prim-written dlmalloc port; `std.fmt` backs `println`/`Debug`.
