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

- **Trait-object coercion of an *owned* value is an untracked borrow.**
  `let g: Trait = s` (`Coerce`) builds a fat pointer `{vtable, data_addr}` that
  aliases `s`'s box. Coercing a *borrow* (a `read`/`mut` parameter or match
  binding) is now rejected (`CoerceOfBorrow` — a second-class borrow cannot
  outlive the call/arm that holds it), but an **owned** source is still treated
  as a read: moving `s` away (`consume(own s)`) or letting it drop while `g` is
  live compiles cleanly and leaves `g` dangling — `g.say()` reads the freed
  box. The fix is to treat `Coerce` as a *move* of the source, turning the
  silent UAF into a use-after-move error — at the cost of leaking the value's
  `Drop` until `dyn` values are themselves drop-elaborated.

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

- **Debug derive — structs done, enums unblocked, generics pending.** `Debug`
  is auto-derived for non-generic structs whose fields are all `Debug` (a
  derivability fixpoint; non-`Debug`-field structs are skipped). `@dbg` routes
  through `Debug` and prints structs; the old hand-written `__println_*` builtins
  + `@dbg` scratch map are deleted (formatting now lives in `std.fmt`).
  Second-class `read`/`mut` match bindings now make **enum** `Debug`/`Display`
  implementable (`fn fmt(read self, f: mut Formatter) { match self {
  Some(read v) => ... } }`) — proven by `fmt_display_enum`. The derive itself
  still covers structs only. Remaining: (1) **generics**
  (`Option`/`Result`/`Vec`) — trait impls on generic types now monomorphize, so
  the remaining piece is **conditional impl bounds**
  (`impl[T: Debug] Debug for Pair[T]`): the derive's body calls each field's
  `Debug::fmt`, which needs `T: Debug` on the impl; (2) **wider coverage** —
  `Debug` for pointers/`Vec` (the "make more types Debug" goal), which depends on (1).

- **Second-class references — mode-only borrows; callbacks and unmarked
  consumption remain.** References are **second-class**: `read`/`mut`/`own`
  exist only as parameter modes, match-arm bindings, and call-site argument
  marks (`fn len(read v: Vec[T])`, `match mut e { Some(mut v) => ... }`,
  `f(mut x)`). There is no reference *type* — no `read T`/`mut T`, no borrow
  expressions, no returned references, no `struct view`/provenance, no loan
  checker. Inside a body a borrow parameter has the plain type `T`; the CFG
  move analysis already rejects storing or returning it, and `ownership.rs`
  adds the second-class guarantees: a borrow is unmovable (`BorrowEscapes` /
  `MoveOutOfBorrow`), never boxed into a trait object (`CoerceOfBorrow`), never
  `mut`-aliased at a call (`MutAlias`), never `mut`-borrowed through a `read`
  parameter (`MutOfRead`), and call-site modes must match declarations
  (`ModeMismatch`). Match-arm `read`/`mut` bindings are second-class exactly
  like parameters (unmovable for the arm's body); `own` bindings move; `mut`
  arm writes copy back into the scrutinee. Match consumption is always
  inferred from the arms: `match own e` is documentation (rejected if the arms
  don't move a payload), and a consuming match cannot borrow a payload out of
  the scrutinee (it is owned and freed by the match).

  Access is copies + whole-structure methods: `Vec.get`/`Array.get` return
  copies of copy-able (scalar, pointer, or inline) elements — enforced at
  monomorphization, see below; `Vec.set`/`Vec.push`/`Vec.swap` mutate in
  place, match arms read enum payloads via `read`/`mut` bindings. Remaining:
  - **Callbacks** — `v.with_mut(i, |e: mut T| ...)`-style borrow-in-a-scope
    needs first-class function values (deferred, out of scope for v1).
  - **Unmarked consumption** — the memory model wants `mut x` to be the *only*
    call-site mark, with consumption and reads unmarked (0007 locality). Today
    a call still marks the mode explicitly (`f(own x)`, `f(mut x)`) and the
    checker requires it to match the parameter's declared mode. Making
    consumption unmarked means deriving each argument's effect from the
    *callee's* parameter mode, which the CFG's move detection currently reads
    from the call-site arg-mode — a deeper change deferred here.
  - **Non-Copy element reads — enforced.** `Vec.get`/`Array.get` are
    deref-reads of the slot; a *boxed* element (a `Drop`-implementing, large,
    or recursive struct; any enum) is stored as a pointer in the slot, so the
    "copy" would alias the slot and double-free on drop. Monomorphization now
    rejects deref-reads of boxed aggregates (the plan's "bound `get` to
    `T: Copy`", enforced precisely): scalars/pointers and inline (≤16-byte,
    no-`Drop`) aggregates are copy-able; boxed ones are rejected with the
    offending instantiation named at the call site
    (`vec_get_noncopy`, `array_get_noncopy`, `vec_get_boxed_struct`,
    `vec_get_inline_struct` tests). `set`/`swap`/`own` moves and match arms
    remain the access path for non-Copy elements.
  - **`match mut` on Copy scrutinees** is rejected at typecheck (a Copy value
    has no place to mutate). `mut`-binding writes to scalar payloads and scalar
    aggregate fields are copied back into the scrutinee's place at arm end
    (codegen `emit_mut_write_backs`); a `mut` binding of an *aggregate* payload
    aliases the box directly, so its writes already land in the scrutinee.
    Rule 4 extends to `match mut`: the scrutinee's root may not be a `read`
    parameter or a `read`/bare arm binding (both at the call site and nested
    inside an arm), since exclusive write-back through a shared borrow would
    alias the caller's box (`MutOfRead`). And a `mut` arm binding itself
    requires the explicit `match mut` (enforced in typecheck) — under a
    bare/`read` match it would write back through a read-only access.

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

- **Const generics are limited.** `const N: usize` params work on functions, on
  the `@builtin type Array[T, const N: usize]` stub, and on `impl Array[T, N]`
  methods (`a.get(i)` / `a.len()`), but not yet on user structs or enums, and
  only `usize` const params are supported. `N` is usable as a value but const
  arithmetic (`N + 1`) is not.

- **`@builtin type` only covers `Array`; primitives stay on `PrimKind`.** The
  `@builtin type Name[params]` stub gives a builtin a nominal home for type
  params and impls. Only `Array` uses it; `u32` etc. keep the `PrimKind` owner
  (their names are reserved keywords, and being non-generic the stub buys little).
  Bare `type` (aliases / opaque user types) is parsed but rejected.

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

- **Drop/RAII — `String` still leaks its buffer.** `Vec` now has full RAII: its
  `Drop` runs each live element's destructor (via `ptr.drop_in_place`) and frees
  the buffer, so `Vec[Res]`/`Vec[String]`/`Vec[Vec[_]]` no longer leak; `from_vec`
  hands off ownership (no use-after-free). `String` is the remaining easy twin —
  it owns a `*mut u8` buffer with no element destructors, so a one-line
  `impl Drop for String { dealloc(self.data) }` would close its leak (mind the
  `from_vec` handoff: the `String` owns the buffer the source `Vec` gave up).
