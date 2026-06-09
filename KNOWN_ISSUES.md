# Known Issues

A running list of known limitations and bugs, for future reference. Split into
real bugs (incorrect behavior) and deferred design (intentional gaps awaiting a
later feature).

## Bugs

- **Traps exit 0.** `panic`, divide-by-zero, and out-of-bounds access all trap
  the wasm module, but the process still exits 0. The `_start` stack-switching
  scheduler doesn't propagate wasm traps to the exit code. A halting program and
  a trapping program are indistinguishable from the outside.

## Deferred design

- **`free` does not reclaim memory.** The allocator is a bump allocator that
  grows linear memory via `memory.grow`; `free` is a no-op. Vec and String leak
  their old buffers on regrow. A real free-list allocator (or ownership-driven
  drop) is the flagship remaining runtime work.

- **`*const u8` read/write friction.** There's no ergonomic way to read/write
  through a `*const` pointer, which blocks a richer `String` API (push_str,
  concat, eq). Options: add `*const` read helpers, or make `String.data` a
  `*mut u8`.

- **Int-literal defaulting conflicts with pinned generics.** `let mut i = 0`
  leaves the literal as an undetermined integer that can conflict with a pinned
  generic type parameter — e.g. `push(v, i*i)` requires `0i32` to resolve. The
  inference doesn't unify a defaulted int against a pinned type arg.

- **No impls on generic instantiations.** `impl Opt for Option[i32]` is
  unsupported; impls only target the generic type, not a concrete instantiation.

- **Arrays don't codegen.** Array literals are parsed but ignored in codegen
  (`case_byte_array_literal`). No array support end to end yet.
