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

- **Debug derive — structs only; enums and generics pending.** `Debug` is
  auto-derived for non-generic structs whose fields are all `Debug` (a
  derivability fixpoint; non-`Debug`-field structs are skipped). `@dbg` routes
  through `Debug` and prints structs; the old hand-written `__println_*` builtins
  + `@dbg` scratch map are deleted (formatting now lives in `std.fmt`). Remaining:
  (1) **enums** — a derived enum `fmt` must read payloads out of a `view self`
  match, which needs borrowing-out-of-match (lifetimes) for non-`Copy` payloads;
  (2) **generics** (`Option`/`Result`/`Vec`) — trait impls on generic types now
  monomorphize, so the remaining piece is **conditional impl bounds**
  (`impl[T: Debug] Debug for Pair[T]`): the derive's body calls each field's
  `Debug::fmt`, which needs `T: Debug` on the impl; (3) **wider coverage** —
  `Debug` for pointers/`Vec` (the "make more types Debug" goal), which depends on (2).

- **Borrows — read-default params, the `data`/`view` kind system, and
  return-provenance done; unmarked consumption + deeper derivation remain.**
  Parameters default to **`read`**: a bare `x: T` borrows (reading is the common
  case going in), `x: mut T` mutably borrows, and an owned parameter is written
  `x: own T`. Move-out bindings and match arms use `own` (`let own r = ...`,
  `Some(own v)`); the `take` keyword is retired.

  `read T` / `mut T` are real, tracked reference
  types: borrow expressions (`read place` / `mut place`), a function may *return*
  a borrow (provenance by elision — the sole borrowed parameter, detected even
  when the `Ref` is nested in the return type), a borrow may live **inside an
  aggregate** (`Option[read T]` constructs/matches/reads), and a **lexical loan
  checker** enforces shared-xor-mutable (can't mutate or re-borrow a value while
  it's borrowed; the loan lasts the holder's scope; release with an inner `{ }`).
  `Vec.at`/`at_mut` and `Array.get -> Option[read T]` return tracked borrows
  instead of raw `*mut T`. A borrowed value is usable where the borrowed type's
  bound is needed (`read i32` satisfies/dispatches `Display`).

  The **kind system** classifies every type `data` or `view` (`cfg::is_view`): a
  type is view-kinded iff it transitively holds a borrow. A struct/enum with a
  view-kinded field must be declared `struct view` / `enum view` (mandatory
  acknowledgment, checked at the definition — "nothing silently assigned"). A
  `view` struct/enum is first-class **within its frame** (construct, read fields,
  pass by `read`/`mut`).

  A view **may be returned** when the signature carries **provenance**: a
  trailing `from <param>` clause names the source, or elision infers it (the sole
  borrowed parameter). The caller then pins the named argument for the returned
  view's lifetime — `let w = make(read pt); pt = ...` while `w` is live is
  rejected. Returning a nominal `view` value with no provenance (built from a
  local, or ambiguous with several borrowed parameters and no `from`) is a
  `ViewEscapes` error. Remaining:
  - **Derivation, deeper** — `from origin(p)` (a composite view yielding views of
    its sources, for iterators) and the type-position placement form
    (`-> read(self) T`, multi-view returns) are not done; provenance is currently
    a single trailing `from <param>` plus elision. And the **callee obligation**
    isn't verified — a returned view is trusted to actually borrow the named
    parameter (returning `read local` under a decoy borrowed param would slip
    through, as it already does for `Ref` returns).
  - **Escaping into long-lived storage** — a view stored in a container that
    outlives its source (a `Vec[read T]` that escapes) still needs the
    return-provenance story extended to stored fields.
  - **Generic-param views** — `kind()` treats `Type::Param` as data pre-mono, so
    a generic that stores a borrow *through* a type parameter isn't yet caught
    (no current program constructs one; per-instantiation kinds want a post-mono
    re-check).
  - **Polish** — `read i32` in *arithmetic* (`a + read_i32`) isn't coerced yet
    (so `Vec.get` still returns a copy); field *assignment* through a `mut`
    borrow (`at_mut(v,i).f = x`); an inline borrow expr in constructor position
    (`Some(read x)` parses `read` as an arg mode — bind to a local first);
    aggregate *array* elements (inline stride) in `Array.get`.
  - **Unmarked consumption** — the memory-model wants `mut x` to be the *only*
    call-site mark, with consumption and reads unmarked (0007 locality). Today a
    call still marks the mode explicitly (`f(own x)`, `f(mut x)`) and the checker
    requires it to match the parameter's declared mode. Making consumption
    unmarked means deriving each argument's effect from the *callee's* parameter
    mode, which the CFG's move detection currently reads from the call-site
    arg-mode — a deeper change deferred here.
  - **NLL** — borrows are lexical; non-lexical liveness is a strict,
    backward-compatible upgrade on the same checker.
  - **Multiple borrowed params** — elision is single-source; more than one needs
    explicit `read from <param>` provenance.

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
