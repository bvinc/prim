# Plan: Second-Class References

**Status: accepted — P1–P5 complete (compiler, std, tests; suite green 289/0 on
`prim`, 0 workspace errors/warnings).** This plan replaces the current
borrow-as-a-type machinery (`read T` / `mut T` as `Type::Ref`, the
`data`/`view` kind system, provenance, the loan checker) with **second-class
references**: references exist only as a *parameter mode*, never as a type
modifier. It supersedes `MEMORY_MODEL.md` (§"supersedes" below).

---

## 1. The model in one sentence

> `read` and `mut` are properties of a **parameter** (how a value crosses a call
> boundary), not properties of a **type**. A reference exists only while a call
> is running. There is no value you can store, return, or name whose type is a
> reference.

The mental model is Pascal `var` parameters (and the PL notion of
*second-class* values): `fn bump(p: mut Point)` does not create a reference
value — it grants the callee exclusive access to the caller's place `p` for the
duration of the call. When the call returns, the access is gone. There is
nothing left behind.

## 2. Goals & non-goals

**Goals**

- Kill the entire borrow-as-type apparatus: `Type::Ref`, view kinds, `struct
  view` / `enum view`, provenance (`from` clauses, elision, origin), the claim
  ledger, the lexical loan checker, the four bans.
- Make "a borrow cannot outlive its frame" a **theorem of the type system**
  rather than a checked property: with no reference type, there is no reference
  value, and therefore nothing that *can* be stored, sent, or returned.
- Shrink the ownership checker to rules that are local to a call site and mostly
  already implemented.
- Fix the known soundness gap "trait-object coercion is an untracked borrow" by
  construction (coercing a borrow into a trait object becomes a compile error).
- Serve the project's #1 goal: simplicity. Fewer concepts, fewer special cases,
  less code.

**Non-goals**

- No new runtime costs, no GC — ownership, moves, and `Drop`/RAII stay exactly
  as they are today.
- Not a concession to untyped aliasing: mutation still requires exclusive
  access; the exclusivity rule just lives at call boundaries.
- Not introducing first-class function values or closures in this plan (see
  §7 for the follow-on that pairs with them).

## 3. Where `read` / `mut` / `own` may appear

`own` is unchanged — it is already mode-only. `read`/`mut` become mode-only too.

| Position | Today | New model |
|---|---|---|
| parameter | `x: read T`, `x: mut T`, `x: own T` | `read v: Vec[T]`, `mut v: Vec[T]`, `own v: Vec[T]` (mode left of the name) |
| receiver | `self`, `read self`, `mut self`, `own self` | **same** — already left of the name |
| call argument | `f(x)`, `f(read x)`, `f(mut x)`, `f(own x)` | **same** (mode) |
| return type | `-> read T`, `-> Option[read T]` | **error** |
| field type | `input: read String` | **error** |
| generic argument | `Option[read T]` | **error** |
| `let` annotation | `let r: read T = ...` | **error** |
| borrow expression | `let r = read p` | **error** |
| view declaration | `struct view Parser { ... }` | **error** |
| provenance | `-> Sel from x` | **error** |

Call-site marks are unchanged: `f(x)` passes `x` as a shared read (the default),
`f(mut x)` as an exclusive borrow, `f(own x)` moves. Note `f(read x)` already
parses as the read *argument mode* and is identical to `f(x)` — so removing the
borrow *expression* `read p` costs nothing at call sites.

### Syntax: the mode lives on the binding, never in the annotation

The mode keyword always sits **immediately left of the thing it qualifies**:
the parameter name at a declaration, the argument expression at a call. This is
already the rule everywhere except the old parameter syntax:

| Position | Syntax |
|---|---|
| parameter | `fn len(read v: Vec[T])`, `fn bump(mut p: Point)`, `fn eat(own p: Point)` |
| receiver | `fn fmt(mut self, f: mut Formatter)` — `self` is just a parameter named `self` |
| `let` binding | `let own r = o.inner`, `let mut x = 0i32` |
| match binding | `Some(own v)`, `own rest => ...` |
| call argument | `f(x)`, `f(mut x)`, `f(own x)` |

`read`/`mut`/`own` are therefore a single concept — **binding qualifiers** —
uniformly on the left everywhere, and a type annotation is always a plain type.
The parameter parser special case that today detects `self` by "peeking past an
optional mode keyword" disappears: `read self` and `read v` parse by the same
rule. Pascal `var x: T` is the precedent for second-class references with the
mode on the binding.

Under this rule, `let read x = ...` is a **parse error** (a borrow cannot live
in a local — references are second-class), while `let own` / `let mut` stay.
Note that a `let` always moves its (non-`Copy`) RHS into the new binding: bare
`let x = a` and `let own x = a` are identical, so `own` at a `let` site is the
explicit — redundant — spelling of the default (pinned by the
`let_bind_moves` test). This differs from call sites on purpose: a parameter's
declared mode is a contract a bare argument could mismatch, so moves there
require the explicit `f(own x)`; a `let` has no declared mode to mismatch
against, so move is the only coherent default.

Errors should be pointed and suggest the fix, e.g.:

- `read`/`mut` in a type position: *"references are second-class: `read`/`mut`
  may only mark a parameter or call argument"*.
- `let r = read p`: *"borrows are second-class: pass the place directly —
  `f(p)` borrows it for the call"*.
- `struct view X`: *"view types are removed under the second-class model"*.

## 4. Semantics: the rules that remain

**Borrow lifetime is exactly one call.** A `read`/`mut` argument claims its
place only for the duration of the call. Everything else falls out:

- *Re-borrowing* down a call chain is fine: `f(x)` → `g(x)` → `h(x)` nests
  frames; each callee's access dies with its frame. No bookkeeping.
- *Suspension* is fine and needs no rule: a `mut` param held across `yield`
  lives in the callee's frame, which the engine keeps intact; the caller's
  place is not shared with any other task, so nobody can race it.
- *No borrow crosses a task boundary*: `spawn` and channels carry owned data;
  a reference is not a value, so there is nothing to send. Concurrency safety
  becomes structural.
- *No borrow is stored*: the callee's param has the plain type `T`; "storing"
  it means moving it, and moving a borrowed param is already an error
  (`BorrowEscapes` / `MoveOutOfBorrow` in the CFG move analysis).
- *No borrow is returned*: `return x` for a borrowed param is a move of a
  borrowed value — already an error. And `-> read T` doesn't parse.

**The checker shrinks to five local rules** (all but one exist today):

1. **Move dataflow** (unchanged): `read`/`mut` params are tracked, unmovable,
   and un-field-movable. This is what enforces second-classness *inside* a
   body — no new check needed, because the type system gives the checker
   nothing extra to police.
2. **`MutAlias`** (unchanged): the same place may not be passed `mut` twice,
   or `mut` together with any other mode, in one call.
3. **`ModeMismatch`** (unchanged): a non-`Copy` *place* argument must be passed
   with the parameter's declared mode — so `f(x)` for an `own` parameter, or
   `f(x)` for a `mut` parameter, is an error unless marked.
4. **`MutOfRead`** (extended): you cannot `mut`-borrow a value reachable only
   through a `read` parameter — or a `read`/bare arm binding, and the same rule
   applies to a `match mut` scrutinee (its root may not be a read-only borrow),
   since exclusive write-back through a shared borrow aliases the caller's box.
5. **NEW — `Coerce` of a borrowed value is an error.** Boxing a value into a
   trait object (`let g: Trait = x`) stores a pointer to it; when `x` is a
   `read`/`mut` param, that would store a reference. Rejected with
   *"cannot box a second-class borrow; copy the value first"*.

**Deleted in full:** the loans pass (`check_loans`, `borrow_provenance`,
`MutateWhileBorrowed`, `BorrowConflict`), the kind system (`is_view`,
`ViewMarker`, `ViewEscapes`, the four bans), `struct view`/`enum view`,
provenance parsing and elision, `Type::Ref`/`RefKind`, `ExprKind::Borrow`, and
the `is_copy` special case for `read` borrows.

## 5. What stays identical

- Move semantics: `own` params, `let x = y` (bare `let` moves — `let own x = y`
  is the explicit, redundant spelling), match arms `Some(own v)`.
- `let mut x = ...` (mutable *binding* — different keyword position, untouched).
- `Drop`/RAII, recursive field drops, conditional-drop errors.
- `mut self` in-place mutation, `read self` receivers (mode-only already).
- Raw pointers `*mut T` (the trusted core's unmanaged escape) and `std.ptr`.
- Identities: arenas with generation-checked ids, `Shared[T]`, splitters-as-
  data. The heap-shaped story from `MEMORY_MODEL.md` §8 is untouched.
- Green threads: `spawn`, cooperative `yield`, the continuation scheduler.

## 6. The access story — the real cost

Without stored or returned references, these patterns must change:

- **Read an element:** `Vec.get(i) -> T` already returns a copy; it becomes the
  sanctioned read, bounded `impl[T: Copy]` — enforced at monomorphization
  (deref-read of a boxed element rejected; scalars, pointers, and inline
  aggregates allowed — see the P4 implementation log). `Array.get ->
  Option[read T]` becomes `-> Option[T]` for `Copy T` (or a callback, below).
- **Write an element:** `Vec.set(i, v)` (already exists). Add the missing
  whole-structure mut methods (`swap`, `sort`, ...) as `mut self` methods —
  mutation stays possible, just method-mediated instead of handle-mediated.
- **Hold a view across statements:** impossible by design. In-place element
  edits are index + method, or callback (below). Long-lived relationships use
  copies or identities — the same guidance the old model already ended with
  ("make it run with copies; make it fast with proofs").
- **Iterators that yield references** (`VecIter[T]`): gone. Index loops
  (`for i in 0..v.len { v.get(i) ... }`) are the v1 replacement.
- **View structs** (`struct view Parser { input: read String }`): gone. A
  parser over an input becomes copy-based, index-based, or identity-based.

**Phase-2 follow-on: callbacks (borrow in a scope).** The classic companion of
second-class references is a callback whose own parameters are second-class:
`v.each(|e: read T| ...)` / `v.with_mut(i, |e: mut T| ...)` — a borrow that
lives exactly as long as the callback call. This needs first-class function
values (Prim has none today: `spawn` resolves names specially) and eventually
capturing closures. It is deliberately *out of scope* for the first cut; it
does not change the model, only the ergonomics of in-place access. Flagged so
the v1 API doesn't paint us into a corner (keep `get`/`set`/`swap` small and
composable).

**Resolved: `mut` on a temporary is allowed.** `f(mut Point { ... })` passes the
temporary as the mutable value; the callee mutates it and the result is
discarded (no source place to write back to). Consistent with `mut` as
copy-in/copy-out (`T -> T`); a temporary simply has no place to copy back to.
This is today's behavior (temporaries ignore modes).

### Match: the built-in borrow-in-a-scope

`match` arms get the same second-class bindings as parameters — the mode
keyword left of the binding name:

```prim
match e { Some(read v) => v.n, None => 0 }          // v: read borrow, lives for the arm body
match mut e { Some(mut v) => v.n = v.n + 1 }        // exclusive; writes go back to the scrutinee
match own e { Some(own v) => v }                    // explicit consume
```

- `read v` / `mut v` in an arm are second-class exactly like a parameter:
  unmovable, dies at the arm's end, enforced by the existing move machinery.
  An arm is, semantically, a function from the scrutinee to the arm value,
  whose parameters are the arm bindings.
- Scrutinee modes: bare `match e` = read, with `own`-arm consumption still
  **inferred** (today's `match_consumes` — so existing std code like
  `unwrap`'s `match self { Some(own v) => v }` works unchanged). `match mut e`
  is required for `mut` arms — mutation stays explicit (enforced: a `mut` arm
  binding under a bare/`read`/`own` match is a typecheck error, since it would
  write back through a non-exclusive match). `match own e` is documentation of
  the inferred consume — the keyword never *forces* consumption (that is
  always derived from the arms), and it is rejected when the arms don't
  actually move a payload, so the documentation can't rot. Consuming matches
  may not *borrow* a payload out of the scrutinee (an arm that moves one
  variant's payload while another arm borrows another's is rejected: the
  scrutinee is owned and freed by the match).
- Today's `mut x`-as-*mutable-binding* pattern form is repurposed: `mut x` in a
  pattern now means the exclusive borrow (used by exactly one test program
  today; a mutable local in an arm is written `let mut x = ...` in the body).
- This is the fix for enum `Display`/`Debug`, which has been blocked on
  "reading payloads out of a match": `fn fmt(read self, f: mut Formatter) {
  match self { Some(read v) => f.write(v), None => f.write_str("None") } }`.

**Why this unblocks enum `Display`/`Debug` (the concrete state today).**
`trait Display { fn fmt(read self, f: mut Formatter) }` has impls for
primitives, `bool`, and `String`; `Debug` likewise, plus an auto-derive for
non-generic **structs** (`hir_builder::derive_debug`). There is **no**
`Display`/`Debug` for any enum that carries a payload — `Option`, `Result`,
anything user-defined. The reason is exactly the match gap: `fmt` receives
`self` as a `read` borrow of the enum, and to format a payload the body must
`match self { Some(v) => ... }`. Today the only legal non-Copy bindings are
`own v` (moves the payload *out of a borrowed value* → `MoveOutOfBorrow`
error) and `_` (payload inaccessible); bare `v` is `BindWithoutTake`. So a
borrowed enum's payload is unreadable, `println(option)` /
`@dbg(option)` / `debug_string(option)` all fail, and the auto-derive cannot
extend to enums. With `read v` / `mut v` arm bindings the payload borrow
becomes expressible, and the derive can synthesize
`match self { Some(read v) => write_debug(f, v), ... }` arms. This is the
"enum Debug pending" deferred item from `KNOWN_ISSUES.md`.
- Like `mut` on a temporary, `match mut <rvalue>` writes go nowhere and are
  allowed.

This is the same "borrow in a scope" idea as the callbacks follow-on, but
built into the language — no first-class function values needed for enum
payloads.

## 7. Losses, accepted with open eyes

1. **No returned references.** Accessors return copies (Copy types) or need
   callbacks/identities. This is the same loss the old model already accepted
   for heap storage, now applied to return position too.
2. **No views held across statements.** In-place edits are method- or
   callback-mediated. (Mitigation: `mut T` is semantically copy-in/copy-out, so
   a `mut self` method that rebuilds and assigns is always expressible.)
3. **No view structs.** Borrow-holding composites are unrepresentable; the
   "view for now" half of the old model's slogan disappears, leaving "identity
   for keeps".
4. **No reference-yielding iterators.** Index loops and future callbacks cover
   it.

Compensations: the checker largely disappears; concurrency is safe by
construction; the trait-object-of-borrow UAF is closed; several long-standing
"polish" gaps become moot (`read i32` in arithmetic, `read` in constructor
position, field assignment through a `mut` borrow, multiple borrowed params).

## 8. Compiler changes, crate by crate

- **prim-tok**: no token changes. Keep `read`/`mut`/`own`/`view` tokens so the
  parser can emit precise second-class errors.
- **prim-parse**:
  - Parameter lists parse `[read|mut|own] name : type` — the mode is a keyword
    before the name, the type is always a plain type. `self` is just a parameter
    (the old receiver-detection special case is deleted). Call-site argument
    modes (`parse_pass_mode`) are unchanged.
  - `parse_type` rejects leading `read`/`mut` (and bare `view`) everywhere —
    there is no reference type at all.
  - Delete borrow-expression parsing (`read p` / `mut p` as an `Expr`) and
    `ExprKind::Borrow` lowering; emit the "borrows are second-class" error.
    `let read x = ...` is a parse error; `let own` / `let mut` stay.
  - Delete `struct view`/`enum view` and the `from <param>` provenance clause
    (as errors, not silent removals).
  - Patterns gain mode bindings: `read v` (new), `mut v` (repurposed from
    mutable-binding to exclusive borrow), `own v` (unchanged). Scrutinee mode
    keyword: `match mut e` (new); bare `match e` keeps inferred `own`
    consumption.
- **prim-compiler / HIR**:
  - Delete `Type::Ref` + `RefKind`, `ExprKind::Borrow`, `Struct.is_view` /
    `Enum.is_view`, and provenance fields.
  - `cfg.rs`: delete `is_view` and borrow-expr arms; keep `effect`, `is_copy`
    (back to scalars + pointers), `root_symbol`, and the move dataflow.
  - `ownership.rs`: delete the loans pass, provenance/elision, `ViewEscapes`,
    `ViewMarker`, `is_nominal_view`; keep move dataflow + the three call rules;
    add the `Coerce`-of-borrow rule. Match arm bindings are treated like
    parameters: `read`/`mut` bindings are tracked and unmovable (a `mut`
    binding's writes copy back to the scrutinee place at arm end), `own`
    bindings move (consuming the scrutinee, as `match_consumes` infers today).
  - `typecheck.rs`: delete `Ref` unification/coercion sites, `Borrow` typing,
    the `Ref`-unwrapping in bound checks and Display satisfaction; delete
    view-kind computation.
  - `drop_elab.rs`, `mono.rs`: delete `Ref` substitution cases (a borrow is
    never dropped — trivially true when no borrow type exists).
- **prim-wasm / emit.rs**: delete `Ref` layout handling (`Ref` → inner is the
  only special case; it disappears).
- **prim-std**:
  - `Vec`: delete `at`/`at_mut`; bound `get` to `T: Copy`; keep `set`, `push`,
    `len`, `Drop`. (Everything else in `vec.prim` is already mode-only.) The
    `T: Copy` bound is enforced at monomorphization: `get` is a deref-read of
    the slot, and reading a *boxed* element would copy the box pointer (the
    result aliases the slot → double free on drop). The check rejects any
    deref-read of a boxed aggregate, precisely: scalars/pointers and inline
    (≤16-byte, no-`Drop`) aggregates are copy-able; `Drop`-implementing, large,
    recursive structs and all enums are rejected, with the offending
    instantiation (`get[S]`) named at the call site. (Implementation log.)
  - `Array`: `get -> Option[read T]` becomes `-> Option[T]` (Copy bound).
  - `fmt`: unchanged — `fn fmt(read self, f: mut Formatter)` is already
    mode-only. (Confirm no `Ref` hides in `option.prim`/`result.prim`.)
- **tests**: ~20 of ~306 programs encode the old model (`borrow_*`,
  `view_*`, `enum_view_*`, `array_get_ref`, `vec_at_borrow`, `move_out_of_view`,
  `edit_of_view_param`, `drop_borrowed_not_dropped`, `struct_pattern_mut`).
  Delete or convert each to a "rejected under second-class" test (`.expected`
  = `ERROR: ...`). Add new positives: re-borrow chains, `mut self` in-place
  edits, `Coerce`-of-borrow rejection, `MutAlias`/`ModeMismatch`/`MutOfRead`
  (keep `mode_mismatch` — it still applies), moving-a-borrow rejection,
  storing-a-borrow rejection, `match` with `read v` / `mut v` arm bindings and
  `match mut e` scrutinee modes, and enum `Display` via `read v` arms.

## 9. Interaction notes

- **Traits / dynamic dispatch:** `fmt(read self, f: mut Formatter)` is
  unaffected (modes on trait method params already flow through `DynCall`).
  Only boxing a *borrow* is new-rejected; boxing an owned value is unchanged.
- **Generics:** a generic `fn f[T](x: read T)` borrows `x` regardless of `T`;
  `T` is treated as non-`Copy` pre-mono exactly as today, so nothing changes.
  `Type::Param` can no longer smuggle a borrow (the old per-instantiation-kind
  gap disappears with the kind system).
- **`MEMORY_MODEL.md`:** on acceptance, rewrite it to the second-class model
  (roughly: this plan's §1–§7, plus the retained §8 identity story and the
  panic/destructor guarantees). `KNOWN_ISSUES.md` loses the borrow-adjacent
  entries (untracked trait-object borrow, borrow polish items, view escapes).

## 10. Phases

1. **P0 — this plan** (iterate here first).
2. **P1 — parser + typecheck rejections:** second-class errors for `read`/`mut`
   in type position, borrow exprs, `view` decls, `from` clauses. Delete
   `Type::Ref`/`ExprKind::Borrow` from HIR.
3. **P2 — checker deletions:** remove loans/view/provenance passes; add the
   `Coerce` rule.
4. **P3 — emit/mono cleanup** (mechanical).
5. **P4 — std migration** (Vec/Array; confirm fmt).
6. **P5 — test migration** (delete old-model programs, add rejection + positive
   second-class tests).
7. **P6 — docs** (rewrite `MEMORY_MODEL.md`, update `LANGUAGE_SPEC.md`,
   `KNOWN_ISSUES.md`, `README.md`).

Each phase keeps the tree green: `cargo fmt`, `cargo clippy -D warnings`,
`cargo test` gate every commit (pre-commit hook).

### Implementation log (P1–P5)

- **P1 done.** `prim-parse`: `Type::Ref`/`RefKind`/`ExprKind::Borrow` deleted;
  `parse_type` rejects `read`/`mut`; params parse `[read|mut|own] name : type`
  (bare = `read`; `self` may omit annotation, typed `Self`); `from` clause and
  `view` modifier rejected; `match` parses an optional scrutinee mode;
  patterns get a `PatternCtx` (Let vs Match) — `read`/`mut` prefixes are errors
  in `let`, legal in match arms; `own [mut] x` moves. `prim-compiler` HIR:
  `Function::provenance`, `Struct/Enum::is_view`, `Type::Ref`/`RefKind`,
  `ExprKind::Borrow` deleted everywhere (`hir_builder` lowering, `cfg.rs`
  (incl. `is_view`), `typecheck` (Ref substitution/coercion/auto-deref),
  `mono`, `drop_elab`, `emit.rs`, `walks.rs`).
- **P2 done.** `ownership.rs` rewritten: loans/provenance/view-marker/
  view-escape passes deleted; move dataflow kept; call rules 4/5/7 kept
  (`MutOfRead`, `MutAlias`, `ModeMismatch`); **`CoerceOfBorrow`** added
  (boxing a `read`/`mut` parameter or arm binding into a trait object is
  rejected); match-arm `read`/`mut`/bare non-`Copy` bindings are collected as
  unmovable borrows (moving one out of an arm is `BorrowEscapes`). `typecheck`
  rejects `match mut` on Copy scrutinees (a Copy value has no place to
  mutate). **Rule 4 extended to `match mut`** (review fix): the `BorrowWalk`
  tracks a `read_borrows` set (read params + `read`/bare arm bindings in
  scope) and rejects a `match mut` whose scrutinee root is in it — verified to
  mutate the caller before the fix (`match_mut_of_read_param`,
  `match_mut_of_read_binding` tests); the call-argument rule-4 check now uses
  the same set, closing the nested read-arm-binding gap there too. **`mut`
  bindings require `match mut`** (review fix): typecheck now rejects a `mut`
  arm binding under a bare/`read`/`own` match (`pattern_has_mut_binding`) —
  previously a bare `match` with a `mut` binding compiled and wrote through the
  box, mutating the caller through a documented read-only match
  (`match_mut_binding_requires_mut{, _tuple}` tests).
- **P3 done.** `cfg.rs` Match takes the scrutinee `mode`; `mono`/`drop_elab`/
  `emit.rs`/`walks.rs` Match arms carry the mode; `emit_match` consumes on
  `PassMode::Own`; all `Ref`/`Borrow` leftovers removed. Workspace compiles
  with 0 errors, 0 warnings. **Consumption is always inferred from the arms**
  (review fix): `cfg.rs`, `drop_elab.rs` and `emit.rs` no longer let
  `PassMode::Own` force consumption — `match own e` is documentation, so a
  `match own` with only borrow arms used to move the scrutinee and drop the
  "borrow" binding at arm end (drop-elaboration seeded it as owned while the
  move checker classified it as an unmovable borrow — verified: the write
  landed in a scratch local that was then discarded / `S::drop` ran on a
  borrow). Now the mode never changes consumption; typecheck rejects
  `match own` whose arms don't move a payload, and rejects any *borrow*
  binding in a consuming match (the scrutinee is owned and freed by the match)
  (`match_own_requires_moving_arm`, `borrow_arm_in_consuming_match`,
  `match_own_consumes`, `match_borrow_keeps_scrutinee` tests). **`match mut` scalar write-back** (review fix):
  `emit_test_bind` records each arm's `mut` bindings of *scalar* payloads
  (`MutWriteBack`: the binding's local, source slot, and type) and `emit_match`
  copies them back into the scrutinee's place at arm end (`emit_mut_write_backs`)
  — previously `match mut s { Some { mut v } => v = 99 }` over an i32 payload
  compiled but silently discarded the write (verified: caller read 1), while
  aggregate payloads already aliased the box. Scalar payloads, scalar struct
  fields, and tuple elements now write back (`match_mut_scalar_writeback`,
  `match_mut_inline_field_writeback` tests); skipped when the scrutinee is
  consumed (boxes freed — no place to write back to).
- **P4 done.** `prim-std`: `Vec.at`/`at_mut` deleted (returned borrows);
  `Array.get -> Option[T]` (was `Option[read T]`, returns a copy); all params
  converted to mode-left (`read v: Vec[T]` → `read v: Vec[T]`); `fmt`/
  `option`/`result`/`sys`/`time`/`rt`/`io` confirmed mode-only already.
  **`T: Copy` bound on `get` enforced** (review fix): `mono.rs` now rejects
  deref-reads of boxed aggregates after substitution (`check_deref_read` — a
  boxed element is stored as a pointer in the slot, so the "copy" would alias
  it and double-free on drop). The inline-vs-boxed decision mirrors the wasm
  `InlinePolicy` (`hir/inline.rs`: ≤16 bytes, no transitive `Drop`, no
  recursion; enums/arrays always boxed) and the error is attributed to the
  call site that instantiated the read (`get[S]` / `get[S, 2]`).
  `Vec.get`/`Array.get` on boxed elements is rejected; scalars, pointers, and
  inline structs keep working (`vec_get_noncopy`, `array_get_noncopy`,
  `vec_get_boxed_struct` (20B), `vec_get_inline_struct` (16B) tests).
- **P5 done.** 19 old-model test programs deleted (`borrow_*`, `view_*`,
  `enum_view_*`, `vec_at_*`, `let/match_bind_without_take`, ...); 50 programs'
  params mechanically converted to mode-left. New tests: `match_read_binding`
  (bare + explicit `read` arm bindings, scrutinee reusable after), 
  `match_mut_writeback` (`match mut e` + `mut p` binding writes back),
  `match_move_out_of_borrow` (moving an arm borrow is `BorrowEscapes`),
  `fmt_display_enum` (the enum `Display` payoff — `match self {
  Some(read v) => ... }`), `coerce_of_borrow`, `mut_alias`, `mut_of_read_param`
  (rejection rules 4/5/Coerce). `mode_mismatch` kept, `struct_pattern_mut`
  (repurposed `mut x` field binding) kept. `let_bind_without_take` was
  replaced by `let_bind_moves` (review fix): the old pointed
  "write the explicit move" error is gone — bare `let x = a` now *is* the
  move, and the new test pins that a later use of `a` errors with "use of
  moved value" (`let own x = a` is the explicit, redundant spelling).
  Full workspace: **423 passed / 0 failed** (was 438; −19 deleted +7 new;
  6 ignored).

## 11. Open questions

None — all decisions locked (see §12).

## 12. Decision log

- **Naming: keep `read` / `mut` / `own`.** `read` = Ada/C# `in` (value flows
  in, read-only); `mut` = Ada `in out` / C# `ref` / Swift `inout` / Pascal
  `var` (flows in, writable, flows back); `own` is a distinct *ownership*
  axis — a move, not a data-flow direction — so `out` (write-only, in every
  prior-art language) would be the wrong name. (2025-08-15)

- **Parameter mode moves left of the name** (`read v: Vec[T]` not
  `v: read Vec[T]`). Rationale: the mode is a property of the binding, not the
  type, and every other binding position (receivers, `let`, match arms, call
  arguments) already puts the mode left of what it qualifies; the old
  right-of-colon form was the one place that made `read T` look like a type.
  Also deletes the `self`-detection parser special case. (2025-08-15)
- **Keep explicit `own`.** Unmarked consumption (`MEMORY_MODEL.md` §3) stays
  out of scope; `f(own x)` remains the explicit move mark. Orthogonal to this
  experiment. (2025-08-15)
- **`mut` on a temporary is allowed** (`f(mut Point { ... })`); the temporary is
  the mutable value and the result is discarded. (2025-08-15)
- **No returned references, no concessions.** `Vec.at`/`at_mut` are deleted;
  `-> read T` does not exist. The strict model is what makes the checker
  disappear. (2025-08-15)
- **`MEMORY_MODEL.md` is superseded** in place by this plan's model on
  acceptance (it is the design of record; the compiler must match one
  document). (2025-08-15)
- **Match uses the same modes as parameters** (§6): arm bindings
  `read v` / `mut v` / `own v` are second-class, exactly like a parameter's;
  bare `match e` = read with inferred `own`-arm consumption; `match mut e` is
  required for `mut` arms (enforced — see the implementation log); `match own
  e` is documentation — it never forces consumption (always inferred from the
  arms) and is rejected when the arms don't move a payload. Match is the
  language's built-in *borrow-in-a-scope*. (2025-08-15)
- **Function values are a future feature, not v1.** v1 access is copies +
  whole-structure methods + match borrow-in-a-scope; `v.with_mut(i, fn(e)
  {...})`-style callbacks wait for first-class function values (a separate
  feature that Prim does not have today — `spawn` resolves names specially).
  (2025-08-15)

## 13. Prior art for parameter modes

The second-class parameter-mode model has deep, battle-tested prior art — and
Prim's would be the same model. In every prior-art language, a mode describes
**which data-flow channels are open between caller and callee** — value in,
value out — and the mode is a property of the formal parameter, never of the
type:

- **Ada** — the canonical case: `in` (default): the formal is a read-only view
  of the actual; the callee cannot write it. `in out`: a read-write view;
  writes propagate back to the caller's variable. `out`: a write-only view;
  the callee assigns it and the caller's variable receives the final value
  (the actual's prior value is not read). Composite `in`/`in out` are passed
  by reference, scalars by value/copy-out — but the semantics are the same.
- **Fortran** — `intent(in)`: read-only; the actual may be an expression, not
  just a variable. `intent(inout)`: read-write; the actual must be a definable
  variable. `intent(out)`: write-only; on entry the actual argument becomes
  **undefined** (and an allocatable is deallocated) — the old value is
  discarded.
- **C#** — `in`: read-only ref; the callee cannot modify it (the argument may
  be an expression — copied to a temp if needed). `ref`: read-write; the
  argument must be a definitely-assigned variable. `out`: write-only; the
  callee must assign it before returning; the argument need not be initialized.
- **Swift** — `inout`: read-write, passed with an explicit `&` at the call
  site; semantically copy-in/copy-out, the caller's variable must be
  initialized. (No `in`/`out` — `in` is just a value param, `out` doesn't
  exist.)
- **Pascal / Modula-2 / Nim** — `var` parameters: read-write by reference,
  always a variable.
- **Rust** — the counter-example: `&T` / `&mut T` are first-class *types*, the
  thing this experiment moves away from.

The taxonomy in one line: **`in` = the value flows in and may be read; `in out`
= the value flows in, may be read and written, and flows back out; `out` = the
value flows out only (callee produces it, caller's old value is discarded).**

**Where Prim's model differs — and why `out` is the wrong name for `own`:**

Prim's three modes are not all on the same axis:

- `read` = Ada/C# `in`: value flows in, read-only. ✓ same concept.
- `mut` = Ada `in out` / C# `ref` / Fortran `inout` / Swift `inout` / Pascal
  `var`: value flows in, may be written, flows back. ✓ same concept.
- `own` = **nothing in that taxonomy.** It is not a data-flow direction — it is
  an *ownership* transfer. The value flows in and is **consumed**: the caller
  no longer owns it, the callee may destroy it, and it leaves the callee only
  if the callee explicitly returns it (through the separate return channel).
  `out` is the opposite shape (callee *produces* a value the caller didn't
  have); `own` *takes away* a value the caller had. The nearest relatives are
  C++/Rust **move-by-value** arguments, not any `out` parameter.

So `read`/`mut`/`own` are clearer than `in`/`inout`/`out` for two reasons:
`read` and `mut` describe the *access* precisely, and `own` names the
ownership axis honestly instead of borrowing `out`'s wrong (write-only)
intuition. Recommendation: keep `read`/`mut`/`own`.
