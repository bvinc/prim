# Value Types + the Unsafe Core

Status: **mostly implemented** (463 tests green, fmt/clippy clean):

- **Explicit `impl Copy`** (§2.4, "Resolved: explicit `impl Copy`") — parse,
  HIR registry, well-formedness validation (no `Drop`, all fields `Copy`,
  non-generic), and a program-aware `is_copy` threaded through ownership, cfg,
  typecheck, and codegen.
- **The unsafe core** (§1.2, §4 Phase 3's gating) — `unsafe fn` and
  `unsafe { ... }` blocks gating `*p` deref, integer↔pointer/typed
  reinterpretation, and allocator/memory primitives to an explicit `unsafe`
  context. Address computation (`null`, `addr`, wrapping pointer arithmetic)
  is safe. The std raw-core modules (`vec`, `array`, `string`, `mem`, `ptr`,
  `fmt`, `io`, `rt`, `wasm.memory`) expose safe wrappers and mark their raw
  internals `unsafe fn` / `unsafe { ... }`; the raw-pointer demo test programs
  do the same.
- **The inline representation** (§3.2) — every concrete aggregate
  (struct/tuple/enum/array, including `Drop` types) is an inline value: one
  heap box per *owned* value, with nested aggregates stored at their field
  offset (not behind a per-field sub-box pointer). `size_of`/`add`/container
  stride report the true byte size; `drop_T` drops fields in place and the
  drop site reclaims the box; enums are inline tagged unions whose drop
  dispatches on the discriminant; `Vec`/`Array` store elements inline;
  a moved-out field is deep-copied to an independent box (not aliased), at
  `let`, `own` match bindings, `return`, and `own` call arguments.

### Phase 1b — landed (deep-copy value semantics)

`let b = a` and `a = b` on a `Copy` aggregate now copy the *value*, not the
box pointer. The implementation deep-copies the box at copy sites
(`emit_copy_value`: allocate a fresh box, `memory.copy` the inline bytes),
so writing through one name never shows up through another. Covered by
`impl_copy_value_semantics.prim` and `impl_copy_nested_field.prim` (nested
`Copy` field destructuring).

### Phase 2 first slice — enum payload + un-taken-field drop — landed

Two known leaks are closed ahead of the full place/representation rewrite, both
in the current boxed model:

- `drop_T` for an **enum** now reads the discriminant and drops the active
  variant's payload fields (each a box pointer) before freeing the enum box, so
  `Option[Res]`/`Result[..]` payloads no longer leak.
- `emit_consume_cleanup` (a consuming `match`) now drops wildcard/omitted
  needs-drop fields in place instead of leaking them, and runs after the arm
  body so drop order is `arm body → moved bindings → un-taken fields`.

`drop_enum_payload_leak` and `drop_untaken_field_leak` moved out of the
known-leak column (`test_programs/ignored.txt`).

### String `impl Drop` — landed

`String` now owns its buffer: `impl Drop for String` frees `self.data` via
`std.mem.free`. This is the codegen-side of the locked decision that `String`
omits `impl Copy` because it must drop its buffer. `free` is null-safe, so a
default/empty `String` (null data) is fine.

### `T: Copy` bounds — landed (call-site enforcement)

`Copy` is now a real lang-item marker trait (`trait Copy {}` in `std.ops`,
prelude-visible), so `T: Copy` resolves as a bound. Typecheck gives it special
satisfaction rules — a type satisfies `T: Copy` iff `is_copy` says so
(scalars/pointers unconditionally; structs with explicit `impl Copy`), not via
the impl table. A `Copy` bound may be forwarded through a same-bound generic
param; a missing bound reports "forwarded type parameter must carry bound
Copy".

`Vec.get` is now `fn get[T: Copy](read v: Vec[T], i: usize) -> T`, so reading a
non-`Copy` element is a plain type error (`type S does not implement trait
Copy`) instead of a monomorphization heuristic.

`Array.get` followed the same route as a **free function**
(`std.array.get[T: Copy, const N: usize](read a, i) -> Option[T]`), replacing the
old inherent method. Inherent methods share the whole impl's type parameters
(`Array[T, N]`) and can't carry a per-method `T: Copy` bound, so the free form
parallels `Vec.get` and gives the same call-site type error. `Array.len` stays
an inherent method (it doesn't read elements).

With both `get` entry points bounded by `T: Copy`, the `check_deref_read`
monomorphization heuristic is **deleted** (it was the transitional guard that
rejected boxed-element deref-reads; the bounds now reject them at type-check
time).

`is_copy` is bound-aware: the shared copy-vs-move predicate now takes a
`CopyCtx` (the opt-in `Copy` set + the `Copy` trait + the function's type
parameters), so a `Copy`-bounded `Type::Param` is `Copy` *inside* a generic
body too. `fn f[T: Copy](read x: T) -> T { return x }` now compiles (the value
is duplicated, not moved out of the borrow). The move analysis (`ownership`),
the CFG, drop elaboration, and typecheck all consult the same `CopyCtx`;
post-mono passes (`inline`, `prim-wasm` emit) use `CopyCtx::concrete`.

Still to implement: the **deep codegen value migration** — non-flat aggregates
are still heap boxes and enums are still boxed tagged unions (so `let b = a` /
`a = b` of a *non-`Copy`* aggregate still moves the box pointer — correct for a
move); `InlinePolicy`/`stored_size`/`MAX_INLINE_BYTES` remain as transitional
machinery until Phase 2 lands. `impl Copy` is declared, validated, and
`Vec.get`/`Array.get` are bounded by `T: Copy`, but the boxed-aggregate
representation for non-`Copy` types is unchanged (Phase 2).

**Remaining work is one coupled block.** Phase 0 (true `size_of`), Phase 2
(stack places + inline enums + non-`Drop` unboxing), the Phase 3 leftovers
(`Vec.swap` value-swap, `InlinePolicy`/`stored_size` removal), and the Phase 4
`Option`/`Result` inline all change the *same* value representation and must
land together to stay coherent: `size_of`/container stride, `drop_elab`, top-
level value materialization, and enum lowering are all functions of the one
boxed-vs-inline decision. The `impl Copy` language feature above is complete
and independent; the representation change is a separate, larger codegen
rewrite.

This reconciles the compiler with the value semantics already specified in
[`MEMORY_MODEL.md`](MEMORY_MODEL.md) §1–§2 and §9, and adds the missing piece
that §9 gestures at but never states: the **unsafe core** where raw pointers
live.

Two coupled corrections to the current implementation:

1. **Compound types are values, not boxes.** The compiler today makes every
   struct/enum/tuple/array a heap box, so "the value" is an `i32` pointer to
   that box — and that pointer leaks out of `*p` and `let b = a`, turning the
   spec's moves into observable aliasing. Compound types must be inline values.
2. **Raw pointers are the unsafe escape hatch, not a safe operation.** Deref
   is a raw load/store that cannot be made safe in general; it belongs inside
   an explicit `unsafe` context (`unsafe fn` / `unsafe { ... }`), and the safe
   layer never performs it.

---

## 1. The two layers

### 1.1 Safe layer — values and second-class borrows

Unchanged from `MEMORY_MODEL.md`: one owner per value, moves/copies, `Drop`,
and `read`/`mut`/`own` as second-class binding modes. Compound types are
**values** with a real inline layout — a struct/enum/tuple/array is its bytes,
not a pointer to them.

### 1.2 Unsafe core — raw pointers

Raw pointers (`*mut T`, `*const T`) are the escape hatch. They are **not safe
and cannot be made safe**: a deref is a raw load/store whose soundness depends
on the global aliasing and lifetime state of linear memory, which a bare
`*mut T` deliberately does not carry. Making it safe would mean reinventing the
borrow checker — which is what `read`/`mut` already are.

The boundary is therefore an **`unsafe` marker at the point of use**, not a
proof:

- An `unsafe { ... }` block (or the body of an `unsafe fn`) may use the raw
  powers: `*p` (load/store), integer↔pointer or byte-region↔typed-pointer
  reinterpretation (`at`, `from_addr`), `drop_in_place`, the allocator, and
  raw byte I/O.
- Outside an `unsafe` context these are compile errors. Naming, storing, and
  passing `*mut T` as an *opaque value* stays allowed (pointers are `Copy`), so
  a `Vec` handle can be a safe value even though its `ptr` field is `*mut T`.
- **Computing an address is safe**: `null`, `addr`, and the wrapping arithmetic
  (`add`, `sub`, `offset`, `byte_add`, `byte_sub`, `byte_offset`) only produce
  or inspect a pointer's numeric address — they touch no memory and claim no
  pointee lives there (mirroring Rust's `ptr::null` / `addr` / `wrapping_add`,
  all safe). Safe code cannot deref the result anyway.
- The compiler still runs its normal checks inside `unsafe` bodies — move
  dataflow, `Drop` elaboration, bounds checks where present — so it catches
  local mistakes. But it does **not** claim to prove the raw derefs sound. The
  `unsafe` author carries that obligation, exactly like Rust's `unsafe`.

Raw-core modules (v1): `std.mem` (allocator), `std.ptr`, `std.vec`,
`std.array`, `std.string`. Everything else is safe code; the raw internals of
these modules are `unsafe fn`s and `unsafe { ... }` blocks behind safe APIs.

**Why this resolves the deref-read guard.** `check_deref_read` currently
exists because `Vec.get` (unsafe code) performs a raw deref, and the compiler
tried to vet it from outside by inspecting the element type. That is the wrong
layer. Under this model the guard is deleted and replaced by two ordinary
things: (a) raw deref is gated to an `unsafe` context, and (b) `get` carries a
`T: Copy` bound. `get[String]` fails to typecheck because `String` is not
`Copy` — not because a mono-time heuristic decided the deref "looked boxed".

---

## 2. Target semantics

Normative. (This is `MEMORY_MODEL.md` §1–§2 made concrete for aggregates.)

1. **Every type has one inline layout.** `size_of[T]()` is the byte size of a
   `T` value. Field offsets are relative to the value's start. A `T` in a
   container field/element is the same bytes as a `T` anywhere else. `size_of`
   is the *one* sanctioned representation query — no other part of the value's
   interface may reveal whether the compiler chose one representation or
   another.
2. **Boxing is invisible.** Whether a value is stored in a register, a stack
   place, or (in future, as an optimization) a heap box is the compiler's
   choice, unobservable to the program — except through `size_of`, and in this
   plan the value size is the physical inline size, so there is nothing to
   leak. `*p` and `let b = a` always operate on the *value*, never a pointer
   artifact.
3. **One owner per value.** Assignment and argument passing are moves for
   non-`Copy` types, copies for `Copy` types. A move is a fact about names
   (source dead afterward), not an operation on bytes.
4. **`Copy` is the single concept, opt-in.** A type implements `Copy` or it
   does not; there is no separate "trivial" notion and **no size cutoff**.
   Scalars and raw pointers are `Copy` (primitive, always). Compound types are
   **not** `Copy` by default: they are `Copy` only when explicitly declared
   (resolved below). The representation (boxed/inline) is never a factor.
5. **Element reads copy.** `Vec.get(i)` / `Array.get(i)` copy the element out
   and leave it in place; they are allowed only for `Copy` elements, expressed
   as a `T: Copy` bound — not a representation heuristic.
6. **Deref is an unsafe operation, not a value operation.** Safe code has no
   deref. Inside an `unsafe` context, `*p` is a raw load/store; `*p = v` is a
   raw store; moving a value out of a slot (e.g. `swap`, a future `pop`) is the
   `unsafe` author's careful raw read, with the normal move dataflow still
   tracking ownership of the *temporary* the read produces. Nothing here is a
   safe "deref-move" construct.
7. **Drop runs exactly once**, at the owner's last use / scope end, recursively
   through fields (`MEMORY_MODEL.md` §10).
8. **Enums are fixed-size tagged unions** (decided): a discriminant plus the
   max-variant payload, inline. `Option[i32]`/`Result` allocate nothing.
9. **Trait objects stay boxed** (decided): `dyn Trait` is a non-`Copy` fat
   pointer `{vtable, data_ptr}`; that indirection is intrinsic to dynamic
   dispatch, not a sneaky optimization.

Consequences:

- `get` needs no guard: `fn get[T: Copy](read v: Vec[T], i: usize) -> T`
  returns `*add[T](v.ptr, i)`; the `Copy` bound makes the raw read a sound
  copy, and the `unsafe` author guarantees the bounds check.
- `let b = a` for `a: Point` is a move (or copy if `Point: Copy`); for `a: i32`
  a copy. Exactly `MEMORY_MODEL.md` §2.
- The "boxed aggregate double-free" bug class ceases to exist: nothing is a
  boxed aggregate.
- `Vec.swap` is the ordinary three-step value swap (`let tmp = *p_i; *p_i =
  *p_j; *p_j = tmp`) written in unsafe code, sound for every `T`, including
  `Drop` types, because the move dataflow drops each temporary exactly once.

### Resolved: explicit `impl Copy`

`Copy` is opt-in. A compound type is `Copy` **only** when it writes
`impl Copy for T {}`. The compiler checks the declaration is well-formed:
`T` has no `Drop` impl and every field/element of `T` is itself `Copy`
(scalars and raw pointers are the base case). Scalars and raw pointers are
`Copy` without a declaration.

The reason it must be explicit is that **"all fields Copy" does not imply the
type is Copy** — ownership can hide behind a Copy field. `String` is the
canonical example:

```prim
struct String { data: *mut u8, len: usize, cap: usize }
```

`*mut u8` is `Copy` and `usize` is `Copy`, so an automatic rule would mark
`String` `Copy` — wrong, because `String` owns the buffer behind `data` and
must run `Drop` to free it. `String` therefore simply does *not* write
`impl Copy`, and the move checker treats it as move-only. Automatic structural
`Copy` (option A) gets this wrong; the opt-in declaration is what lets the
author say "this type is safe to duplicate" only when that is actually true.

`Point`, `Pair`, `Counter`, and any plain-data record become `Copy` by writing
`impl Copy for Point {}` (or a `#[derive(Copy)]`, if that sugar is added
later). `String`, `Vec`, and `dyn Trait` never write it.

---

## 3. Representation & ABI plan

The hard constraint: a wasm local holds one scalar (`i32`/`i64`/`f32`/`f64`),
never a multi-word struct. Value semantics therefore need one of two local
representations, chosen per type:

### 3.1 Scalarizable (flat) `Copy` aggregates → per-field wasm values

A **flat** type — every field scalar/pointer, no `Drop`, no recursion — is
represented as its leaf fields, one wasm value each. The machinery already
exists and generalizes cleanly:

- `flat_scalar_fields` (`prim-wasm/src/emit.rs:228`) already flattens a struct/
  tuple into `[ScalarField]`; it is currently gated on `policy.is_inline`
  (≤16 B, no `Drop`). **Remove the gate; gate on the explicit `Copy` impl
  instead.**
- `scalarizable_locals` / `scalar_abi_params` (emit.rs:314) already keep such
  values in wasm locals and pass them by value when used purely by field reads.
  **Extend the disqualification walk so any whole-value use (copy/move/return)
  is legal for `Copy` types** — the value is re-materialized from its fields —
  instead of disqualifying it.
- A struct/tuple **literal** of a flat type is emitted as field values on the
  stack, not `alloc` + stores (`emit_struct_lit`/`emit_tuple_lit` lose their
  box path for `Copy` types).
- **Copy** of a flat type = copy each leaf; **move** = same, source dead (no
  runtime cost). No heap traffic.

This makes `Point`, `Pair`, `Counter`, and a 20-byte flat struct all
zero-allocation values.

### 3.2 Non-scalarizable aggregates → a *place*, not a box

A value that is not flat (`Drop`, enum, array, recursion, or too large) lives
in a **stack place**: a region in linear-memory scratch (a per-frame stack; v1
may `alloc`/free it deterministically). A wasm local holds a *pointer to the
place*; what makes it a value type rather than today's box is that **ownership
and copy/move are value-level, and the pointer is §2.2-invisible**:

- A place has exactly one owner. `let b = a` *moves*: either the bytes copy
  into `b`'s place and `a`'s place dies, or `b` points at `a`'s place and `a`
  is dead (place hand-off). Never two live owners of one place.
- Copy is allowed only for `Copy` types; a non-flat type is not `Copy`, so no
  silent deep copy.
- Drop of a non-`Copy` value runs its destructor on the place, then reclaims
  the place.

### 3.3 Calls

- Flat `Copy` params/returns: scalar ABI (already implemented; generalized in
  3.1).
- `read`/`mut` params: pass a pointer to the caller's place (existing intent;
  `mut` writes through it). Unchanged, except places are now real.
- Large/owned non-`Copy` params: pass a pointer to the source place as the move
  (§9 "moves are semantic") — no deep copy, no heap box.
- Enum payloads under `match`: `read`/`mut` bindings point into the inline
  tagged-union payload; scalar write-back unchanged; aggregate bindings point
  at the inline payload region.

### 3.4 What still heap-allocates

Only genuinely dynamic storage, all inside `unsafe` contexts:

- `Vec`'s element buffer and `String`'s byte buffer (growable → heap).
- `dyn Trait` data (dynamic dispatch → heap).
- Nothing else. `Option`/`Result`/plain structs/tuples/enums/arrays are inline
  values (arrays are a fixed inline run of elements).

---

## 4. Phased migration

Each phase lands green on the full suite before the next. Ordering keeps the
deletion of old machinery as late as possible, after its replacement is proven.

### Phase 0 — true `size_of` — landed

`size_of[T]` now folds to `stored_size(T)` = the real inline byte size
(struct fields / tuple elements / enum max-variant payload / `N * elem` for
arrays, recursively), computed by `InlinePolicy` and shared with
`prim-wasm/src/layout.rs` field offsets and `Deref`/`DerefAssign` widths.
`size_of_basic`/`impl_copy_inline_large` expectations updated (real sizes).
  `PtrByteAdd`/`At`/field-offset path that assumed a pointer-sized value must
  switch to byte arithmetic against the new layout. Land together with the ABI
  work, not ahead of it.

### Phase 1 — flat `Copy` aggregates become true values

- Implement `impl Copy` (explicit, as resolved above); share the predicate
  across typecheck, cfg, and codegen. Delete `InlinePolicy` as a *size* notion;
  keep only "is this type flat and `Copy`" (a pure function of the type, no
  layout pass).
- `flat_scalar_fields` un-gated; struct/tuple literals of flat types emitted as
  scalar fields (no `alloc`); `let`/assign/args copy for `Copy`, move otherwise.
- **Breaks (tests flip):**
  - `vec_get_boxed_struct` / `array_get_*`-style rejections: a no-`Drop` 20-byte
    struct is now `Copy`, so these become *positive* tests (read works) — rename.
  - `size_of_basic` expectations (real sizes).
  - Drop tests for flat no-`Drop` structs no longer allocate; re-check
    `alloc_*`.

### Phase 2 — non-flat aggregates become inline values — landed

- Every concrete aggregate is now an **inline value**: one heap box per owned
  value (a *place*), with nested aggregates stored at their field offset.
  Struct/enum/tuple/array literals build their fields into the box; an enum
  literal builds the inline tagged union (`[u32 discr][max-variant payload]`).
  `Drop` types stay a box so their destructor runs, but their fields are inline.
- `drop_T` runs the value's `Drop::drop`, recursively drops owned fields **in
  place** (`ptr + offset`), and no longer frees the box — the drop site
  (`Stmt::Drop`, `drop_in_place`'s caller, match consume-cleanup) reclaims it,
  so inline nested fields are never freed twice.
- A moved-out field is deep-copied to an independent box (`emit_move_value` /
  `emit_field_copy_out`) at `let`, `own` match bindings, `return`, and `own`
  call args; `read`/`mut` borrows of a field still alias its bytes.
- **Tests flipped/added:** `drop_untaken_field_leak` and
  `drop_enum_payload_leak` (known leaks) now pass and left the known-leak
  column; `drop_partial_move_return.prim` covers return-position field moves;
  `impl_copy_inline_large` reports the 4-aligned size (20, was 24);
  `vec_drop`/`vec_drop_elements`/`vec_swap` still pass (order, not addresses).

### Phase 3 — the unsafe core

- Add `unsafe fn` and `unsafe { ... }` blocks; gate `*p` deref,
  `at`/`from_addr` reinterpretation, `drop_in_place`, and allocator calls
  behind an explicit `unsafe` context. Address computation (`null`, `addr`,
  wrapping `add`/`sub`/`byte_*`) stays safe.
- Move `std.vec`, `std.array`, `std.string`, `std.mem`, `std.ptr`'s raw
  internals behind `unsafe fn` / `unsafe { ... }`, keeping their safe APIs.
- **Delete `check_deref_read`** (`prim-compiler/src/hir/mono.rs`). `get`/
  `Array.get` gain a `T: Copy` bound; `get[String]` becomes an ordinary
  type error.
- ~~Rewrite `Vec.swap` as the three-step value swap~~ — with inline elements,
  the existing byte-swap of `size_of[T]` bytes *is* the value swap (it swaps
  each element's inline bytes), and `vec_swap` passes. `MAX_INLINE_BYTES` is
  deleted; `InlinePolicy`/`stored_size` now express the single inline layout
  (one place per value) rather than a transitional size heuristic.
- `String` gets a `Drop` that frees its buffer; its struct is a plain inline
  value (one allocation — the buffer — not two).

### Phase 4 — std and test polish

- `Option`/`Result` are now inline tagged unions (enums are inline); confirm
  `unwrap`/`is_some`/mapping helpers compile without the per-variant box.
  They cannot get `impl Copy` (enums aren't `Copy`-validated) — `Option[T]`/
  `Result[T,E]` are inline *move* values.
- Sweep `KNOWN_ISSUES.md`: close "boxed aggregate double-free" (gone),
  "transient box leak" (remaining only for a `*p` read of a `Copy` aggregate —
  a leak, not a soundness bug), "owned trait-object coercion is an untracked
  borrow" (owned `dyn` is now a move), "enum recursive drop" (payload drop runs
  on the inline union).
- Add regressions: `let b = a` on a `Drop` struct must move (no double-free);
  `swap` on `Vec[Drop]` (drop order); `Option[i32]` no-allocation (allocation
  counter if available); a safe-code attempt to deref a raw pointer must be a
  compile error.

---

## 5. Deletions & doc updates (checklist)

Delete:

- `prim-compiler/src/hir/mono.rs` — `check_deref_read` and its error text.
- `prim-wasm/src/layout.rs` — `InlinePolicy`, `MAX_INLINE_BYTES`, `stored_size`.
- `prim-std/src/std/vec/vec.prim` — byte-swap `swap` (replaced by value swap).
- `prim-wasm/src/emit.rs` — `emit_inline_deref_read` transient-box path.

Update:

- `MEMORY_MODEL.md` — §2 (`Copy` rule), §6 (aggregate bindings point at inline
  payload), §7 (element-read copy = `Copy` bound), §9 (representation =
  scalarize / stack place; add the unsafe-core paragraph), §12 conformance.
- `LANGUAGE_SPEC.md` — memory-model bullets (drop "boxed element" / "inline
  ≤16 B"; add `unsafe fn` / `unsafe { ... }` and raw-pointer gating).
- `KNOWN_ISSUES.md` — move the four closed items out of "bugs"/"deferred".

---

## 6. Non-goals

- No first-class references, no returned borrows, no `&T`/`&mut T`.
- No attempt to make raw pointer deref "safe"; the unsafe boundary is an
  `unsafe fn` / `unsafe { ... }` marker, not a proof system.
- No unwinding on panic (`MEMORY_MODEL.md` §10 unchanged).
- No change to `Shared[T]`/arenas/identities (§8, still design-only).

## 7. Risks

- **Enum inline unions touch the most code** (`EnumLayout`,
  `emit_variant_lit`, match-arm payload addressing). Do it in Phase 2 behind
  the existing enum test suite; keep `drop_enum_payload_leak` as the canary.
- **Stack-place management** (§3.2) is new allocation machinery; it must reuse
  the existing scratch/counter discipline and be torn down on every exit path
  (the drop-elab already walks every exit, so the place free rides on it).
- **Unsafe gating** must not accidentally forbid the *naming/storing*
  of `*mut T` (needed for `Vec`'s handle) while still forbidding deref/arith/
  reinterpret — a fine line to get right in the parser/typechecker.
- **Performance** of copying a large flat `Copy` struct by value — mitigated by
  scalarization for flats and place hand-off for non-flats; a large `Copy` type
  is flat and scalarized, so no `memcpy` is ever on the hot path.
