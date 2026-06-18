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

- **Traps exit 0.** `panic`, divide-by-zero, and out-of-bounds access all trap
  the wasm module, but the process still exits 0. The `_start` stack-switching
  scheduler doesn't propagate wasm traps to the exit code. A halting program and
  a trapping program are indistinguishable from the outside.

- **The scheduler busy-waits while all tasks sleep.** When every live task is
  parked on a future wake time (`time.sleep`), `std.rt.schedule` spins on
  `now_nanos()` until the soonest wake instead of blocking. It's correct but
  burns CPU for the whole sleep. The fix is a real timed wait (WASI
  `poll_oneoff` with a monotonic-clock subscription) when nothing is runnable.

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
