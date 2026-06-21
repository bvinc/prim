# Known Issues

A running list of known limitations and bugs, for future reference. Split into
real bugs (incorrect behavior) and deferred design (intentional gaps awaiting a
later feature).

## Bugs

- **OOM corrupts low memory instead of trapping.** `alloc` returns null
  (address 0) when `memory.grow` fails, and no consumer null-checks before
  storing: `Vec.push` and the four codegen box-allocation sites (struct/string/
  variant literal, dyn coercion) write through the null pointer into the
  always-mapped first page — println scratch buffers and string-literal data.
  The old bump allocator trapped on OOM; this silently corrupts static data and
  keeps running.

- **Allocation sizes wrap.** `request2size` has no `MAX_REQUEST` guard, so a
  request near 2^32 wraps to a tiny `nb` (even 0) and `alloc` hands back a
  16-byte chunk while corrupting the free lists. `alloc_array` compounds it with
  an unchecked `count * size_of[T]()` multiply, so a large element count silently
  becomes a small allocation that the caller overruns. Guard both against
  overflow and return null past the limit.

- **`sys_alloc` assumes it owns `memory.grow`.** It extends the top chunk by the
  grown bytes without checking that the new pages are contiguous with the old
  heap end. `std.wasm.memory.grow` is public API, so a user grow inserts pages
  between the heap and the next allocator grow, and the top chunk silently
  expands over the user's pages. The single-segment assumption is documented in
  the file header but enforced nowhere — compare the grow result against
  `top() + topsize()`.

- **Call parens not glued on the dot path.** Commit "require call parens to be
  glued to the callee" added the `glued_to_prev()` check to the plain-call and
  turbofish paths but missed the `.method` infix path. `let a = v.field`
  followed by `(expr)` on the next line is still silently absorbed as
  `v.field(expr)`.

- **`String.from_vec` aliases a live buffer.** It points the String at the Vec's
  buffer without invalidating the Vec. Now that `free` actually reclaims, a
  single `push` on the source Vec that triggers growth deallocates the buffer the
  String still references — dlmalloc immediately writes free-list pointers over
  its first bytes and recycles the chunk on the next `alloc`. Until the borrow
  checker exists, `from_vec` should zero the source Vec's ptr/len/cap (or copy).

- **Trait-object coercion is an untracked borrow.** `let g: Trait = s` (`Coerce`)
  builds a fat pointer `{vtable, data_addr}` that aliases `s`'s box, and the
  ownership checker treats it as a *borrow* of `s` — but with no lifetimes,
  nothing keeps `s` pinned for `g`'s lifetime. Moving `s` away (`consume(take s)`)
  or letting it drop while `g` is still live compiles cleanly and leaves `g`
  dangling: `let g: T = r; consume(take r); g.say()` runs `r`'s drop in `consume`,
  then `g.say()` reads the freed box. This is the trait-object case of the
  general "borrows can't be pinned without lifetimes" gap (Stage 3). A cheaper
  stopgap is to treat `Coerce` as a *move* of the source, turning the silent UAF
  into a use-after-move error — at the cost of leaking the value's `Drop` until
  `dyn` values are themselves drop-elaborated.

- **Traps exit 0.** `panic`, divide-by-zero, and out-of-bounds access all trap
  the wasm module, but the process still exits 0. The `_start` stack-switching
  scheduler doesn't propagate wasm traps to the exit code. A halting program and
  a trapping program are indistinguishable from the outside.

## Deferred design

- **No `*const` pointer operations.** `std.ptr` only provides `*mut` ops, so
  there's still no way to read/write through a `*const` pointer. `String.data`
  was made `*mut u8` to unblock reading string bytes (the `std.fmt` formatter),
  but `*const` read/write helpers are still missing for any genuinely-const
  pointer.

- **No impls on generic instantiations.** `impl Opt for Option[i32]` is
  unsupported; impls only target the generic type, not a concrete instantiation.

- **Arrays don't codegen.** Array literals are parsed but ignored in codegen
  (`case_byte_array_literal`). No array support end to end yet.

- **Drop/RAII (Stage 2) first cut — no recursive field drops.** A value is
  RAII-dropped (its `Drop::drop` runs, then its box is freed) when it implements
  `Drop` or transitively contains a field that does. But the drop glue does *not*
  yet recurse into fields: a composite that holds a `Drop`-implementing field but
  doesn't itself implement `Drop` has its own box freed, while the field's
  `Drop::drop` is **not** called and the field leaks. Compose by implementing
  `Drop` on the outer type for now; auto-recursion is the immediate follow-up.

- **Drop/RAII — `match` bindings leak.** Values bound by `match` arm payloads
  (and enums consumed by a `match`) are not yet dropped — sound (no
  double-free), just leaky. (Conditionally-moved resources are now a *compile
  error*, not a leak: drop placement is decided by a control-flow dataflow over
  a per-function CFG (`hir/cfg.rs`), and a value that implements `Drop` must be
  moved on all paths or none.)

- **Drop/RAII — std `Vec`/`String` buffers not auto-reclaimed.** Plain aggregates
  and the stdlib's `Vec`/`String` (whose backing buffers are raw `*mut`) don't
  implement `Drop`, so their boxes/buffers still leak (unchanged from before).
  Giving them `Drop` impls needs generic-impl support and the `from_vec`
  buffer-ownership fix; that is "full RAII," deliberately deferred.
