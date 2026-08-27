# Known Issues

A running list of known limitations and bugs, for future reference. Split into
real bugs (incorrect behavior) and deferred design (intentional gaps awaiting a
later feature).

## Bugs

- ~~Trait-object coercion of an *owned* value is an untracked borrow.~~
  **Fixed.** `Coerce` now moves an owned source in move position (`let`, return,
  assignment, `own` argument, struct-literal field) and deep-copies a `Copy`
  source, so the trait object owns an independent box. A `read`/`mut` source is
  still rejected (`CoerceOfBorrow`). Trait objects are `needs_drop`, and every
  vtable carries a drop-glue slot so `drop_trait_Trait` runs the concrete type's
  destructor and frees both the box and the fat pointer.

- **Hardware traps exit 0.** wasmtime's stack-switching `resume` swallows a wasm
  trap raised inside the scheduler's continuation (and `proc_exit`/a main-stack
  trap after continuations have run hits a wasmtime assertion), so the engine
  cannot set a nonzero exit code from program code. `panic` works around this by
  writing an abort sentinel to stderr that the runner (`compile_and_run`) turns
  into a nonzero exit — so panics, and everything built on them (`Vec` bounds,
  `unwrap`), now exit nonzero. The allocator's out-of-memory abort
  (`prim_rt_oom`) writes the same sentinel, so OOM also exits nonzero. But
  **divide-by-zero and out-of-bounds memory access trap directly, bypassing
  `panic`, and still exit 0.** Guarding the arithmetic/memory ops to route
  through `panic` is the fix. Note the sentinel only takes effect through
  `prim run`; a raw `wasmtime` invocation still exits 0.

## Deferred design

- **Deref-read of an inline aggregate copies into a transient box that leaks.**
  A `*p` where `p: *mut T` and `T` is an inline aggregate (now every
  non-recursive struct/tuple/enum/array) copies the inline bytes into a fresh
  heap box (the independent copy the memory model requires), and that box is
  never freed — `drop_T` is only synthesized for `needs_drop` types, and a
  `Copy`/no-`Drop` aggregate has no destructor to reclaim it. Sound, but a
  per-read allocation: `Vec.get`/`Array.get` of an inline struct allocates on
  every call. A follow-up could represent inline-aggregate *values* without a
  box (full scalarization) or synthesize a free for non-`Drop` aggregate boxes.

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
  `MoveOutOfBorrow`), never *stored* in a trait object (`CoerceOfBorrow`), never
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
  - **Callbacks** — first-class function values now exist as **blocks**
    (`|e| { ... }`, non-capturing), so `get(v, i) |e| { ... }`-style
    element callbacks work. Borrow-in-a-scope callbacks that *capture*
    enclosing locals still need capturing closures (deferred).
  - **Unmarked consumption** — the memory model wants `mut x` to be the *only*
    call-site mark, with consumption and reads unmarked (0007 locality). Today
    a call still marks the mode explicitly (`f(own x)`, `f(mut x)`) and the
    checker requires it to match the parameter's declared mode. Making
    consumption unmarked means deriving each argument's effect from the
    *callee's* parameter mode, which the CFG's move detection currently reads
    from the call-site arg-mode — a deeper change deferred here.
  - **Borrow-to-trait-view coercion** — a `read T` argument may be coerced to a
    `read Trait` parameter (`T: Trait`): the trait-object view is itself
    second-class and dies with the call, so it stores nothing. The checker
    currently rejects *every* coercion of a borrow (`CoerceOfBorrow` fires at
    argument position too), so this is deferred; storing a borrow in a trait
    object stays rejected.
  - **Non-Copy element reads — enforced.** `Vec.get`/`Array.get` are
    deref-reads of the slot and are bounded by `T: Copy` at the signature level
    (`fn get[T: Copy, R](read v: Vec[T], i: usize, f: fn(T) -> R) -> R` /
    `fn get[T: Copy](read a: Array[T, N], i: usize) -> Option[T]`), so a
    `Drop`/non-`Copy` element — which would have to be moved out and could
    double-free — is a clean type error naming the missing `Copy` bound
    (`vec_get_noncopy`, `array_get_noncopy` tests). `set`/`swap`/`own` moves
    and match arms remain the access path for non-Copy elements.
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

- **Drop/RAII — recursive field drops cover structs, tuples, arrays, and enums.**
  Each concrete needs-drop type gets a synthesized `drop_T(ptr)` function
  (`prim-wasm`): it runs `T`'s own `Drop::drop`, recursively calls `drop_F` on
  each owned struct field / tuple element / array element that needs dropping
  **in place** (`ptr + offset`, no per-field box), and does *not* free the box —
  the drop site (`Stmt::Drop`, `drop_in_place`'s caller, match consume-cleanup)
  reclaims it, so inline nested fields are never freed twice. Enums use
  discriminant dispatch: the drop reads the tag and drops only the active
  variant's payload fields in place.

- **Drop/RAII — `own` in a `match` ends ownership.**
  A `read`/`mut` (or bare) arm binding borrows the payload for the arm body; an
  `own` binding moves it out of the scrutinee (`Some { own r }`, positional
  `Some(own v)`, or whole-value `own rest`). An `own` binding consumes the
  scrutinee: the moved-out values are dropped at their arm's end (drop
  elaboration hosts them in the arm's block scope, `hir/drop_elab.rs`) and every
  box the arm did not move out is dropped and freed (`emit::emit_consume_cleanup`),
  including nested destructured boxes and wildcard/omitted needs-drop fields. A
  payload an arm *returns* is moved out and not double-freed. Conditionally-moved
  resources are a *compile error*, not a leak (per-CFG dataflow: `Drop` values
  must be moved on all paths or none).

- **Drop/RAII — `Vec` and `String` both have full RAII.** `Vec`'s `Drop` runs
  each live element's destructor (via `ptr.drop_in_place`) and frees the buffer,
  so `Vec[Res]`/`Vec[String]`/`Vec[Vec[_]]` no longer leak; `from_vec` hands off
  ownership (no use-after-free). `String` owns a `*mut u8` buffer and frees it
  in its `Drop` (`impl Drop for String { free(self.data) }`), with the
  `from_vec` handoff respected (the `String` owns the buffer the source `Vec`
  gave up).
