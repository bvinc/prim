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

- **Hardware traps exit 0.** wasmtime's stack-switching `resume` swallows a wasm
  trap raised inside the scheduler's continuation (and `proc_exit`/a main-stack
  trap after continuations have run hits a wasmtime assertion), so the engine
  cannot set a nonzero exit code from program code. `panic` works around this by
  writing an abort sentinel to stderr that the runner (`compile_and_run`) turns
  into a nonzero exit — so panics, and everything built on them (`Vec` bounds,
  `unwrap`), now exit nonzero. But **divide-by-zero and out-of-bounds memory
  access trap directly, bypassing `panic`, and still exit 0.** Guarding the
  arithmetic/memory ops to route through `panic` is the fix. Note the sentinel
  only takes effect through `prim run`; a raw `wasmtime` invocation still exits 0.

## Deferred design

- **Expression-scoped borrows (non-storable places).** TODO: a `place T` value
  usable only *within the expression that produces it* — never bound to a `let`
  or stored in a field. Because it can't escape, dereferencing it is safe
  *without lifetimes and without exposing raw pointers* — the return-position
  dual of `view`/`edit` (which are call-scoped *parameter* borrows). This is the
  intended safe form of accessors like `Array.get(i) -> place T` / a future
  `Option[place T]`, replacing the raw `*mut T` they return today (which can
  dangle — same hole as the `from_vec` issue). A first-class, *storable*
  borrow-checked reference is the larger lifetimes stage; this is the smaller,
  escape-free subset.

- **By-value aggregates — a direct struct/tuple literal at a scalar-ABI
  boundary still boxes.** Small POD aggregates cross parameter and return
  boundaries as wasm field values (no box) when they come from a scalarized
  local, an existing box, or a passed-through scalar-return call. But a *literal*
  written directly at the boundary — `return Point{..}` or `f(Point{..})` —
  still builds a transient heap box and loads its fields back out
  (`emit::emit_scalar_value`, the `StructLit`/`TupleLit` arm), because emitting
  the fields directly would need the scratch pre-walk
  (`walks::scalar_value_scratch`) to reserve field scratch in *declaration*
  order while the literal's field list is in *source* order — i.e. thread the
  struct layout into the pre-walk plus a box-pointer-slot skip. This is a missed
  optimization, not a correctness bug or a regression: the box count is never
  higher than before this work. Writing the literal into a local first
  (`let q = Point{..}; return q`) already avoids the box. See the
  `aggregate-unboxing` plan.

- **No `*const` pointer operations.** `std.ptr` only provides `*mut` ops, so
  there's still no way to read/write through a `*const` pointer. `String.data`
  was made `*mut u8` to unblock reading string bytes (the `std.fmt` formatter),
  but `*const` read/write helpers are still missing for any genuinely-const
  pointer.

- **No impls on generic instantiations.** `impl Opt for Option[i32]` is
  unsupported; impls only target the generic type, not a concrete instantiation.

- **Const generics are limited.** `const N: usize` params work on functions and
  on `impl Array[T, N]` methods (`a.get(i)` / `a.len()`), but not yet on structs
  or enums, and only `usize` const params are supported. `N` is usable as a value
  but const arithmetic (`N + 1`) is not. An array impl's params come from its
  target (`impl Array[T, N]`), since `Array` has no nominal definition.

- **Drop/RAII — recursive field drops cover structs and tuples, not yet enums.**
  Each concrete needs-drop type gets a synthesized `drop_T(ptr)` function
  (`prim-wasm`): it runs `T`'s own `Drop::drop`, recursively calls `drop_F` on
  each owned struct field / tuple element that needs dropping, then frees `T`'s
  box. So a composite holding a `Drop` field (even without its own `Drop` impl)
  now drops that field correctly. Arrays recurse too (each element is dropped).
  **Enums are not yet recursed:** a needs-drop enum has its own box freed but its
  active variant's payload is *not* dropped (sound, just leaky), because that
  needs discriminant dispatch — the next follow-up.

- **Drop/RAII — `take` in a `match` ends ownership; some payloads still leak.**
  Binding a non-`Copy` value in a `match` arm requires `take` (`Some { take r }`,
  positional `Some(take v)`, or whole-value `take rest`); a plain binding of a
  non-`Copy` value is a compile error (borrowing one out awaits lifetimes — use
  `take` to move it, or `_` to ignore it). A `take` binding consumes the
  scrutinee: the moved-out values are dropped at their arm's end (drop
  elaboration hosts them in the arm's block scope, `hir/drop_elab.rs`) and every
  box the arm did not move out is freed (`emit::emit_consume_cleanup`), including
  nested destructured boxes. A payload an arm *returns* is moved out and not
  double-freed. Remaining gaps, sound but leaky: an **un-taken needs-drop field**
  in a consuming arm, and the **live payload behind a wildcard arm over an enum**
  (its box is freed but the variant's payload is not dropped — same discriminant-
  dispatch gap as enum recursive drop above). Conditionally-moved resources are a
  *compile error*, not a leak (per-CFG dataflow: `Drop` values must be moved on
  all paths or none).

- **Drop/RAII — std `Vec`/`String` buffers not auto-reclaimed.** Plain aggregates
  and the stdlib's `Vec`/`String` (whose backing buffers are raw `*mut`) don't
  implement `Drop`, so their boxes/buffers still leak (unchanged from before).
  Giving them `Drop` impls needs generic-impl support and the `from_vec`
  buffer-ownership fix; that is "full RAII," deliberately deferred.
