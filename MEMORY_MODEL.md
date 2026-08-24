# Prim Memory Model

The design of record for how Prim manages memory: **second-class references**.
`read` / `mut` / `own` exist only as *binding modes* — parameter declarations,
match-arm bindings, and call-site argument marks — never as types. The
compiler must match this document.

The whole model in one sentence: *reading is the default going in, owning is
the default coming out, mutation is never a default anywhere, and a borrow
lives exactly one call.*

There are no lifetime annotations. There is no region algebra. There is no
borrow checker as a separate pass. Because no reference is a value, a borrow's
extent is structural: it is the duration of a single call.

---

## 1. Principles

1. **Value semantics.** Every value has one owner. Assignment and argument
   passing transfer ownership (a move); they never create observable aliases.
   Two live names never observe the same mutable state.
2. **No garbage collection. Deterministic destruction.** Every value is
   destroyed at a statically known point — last use, scope end, or its owner's
   destruction — running its destructor (`Drop`).
3. **Exclusivity is the law.** Mutating a place requires exclusive access to
   it. Exclusive access exists only for the duration of a call; outside a call
   there is nothing to hold it.
4. **Representation is not observable.** Values have no address and no pointer
   equality. What a `read`/`mut` parameter compiles to is the compiler's
   choice (§9).
5. **References are second-class.** A borrow is a property of a *binding* — how
   a value crosses one call boundary — not a property of a type. There is no
   value you can store, return, send, or name whose type is a reference.

## 2. Values: ownership, moves, copies

**Move is the default.** `let b = a` transfers ownership; `a` is dead afterward
and any later use is a compile error. A move is a fact about names, not an
operation on values — it needs no trait and no hook, and the compiler may
implement it as a pointer hand-off rather than a copy.

**Copy is opt-in and explicit.** A scalar or a raw pointer is copyable
unconditionally: it is duplicated on assignment and argument passing, and the
source stays live.

```prim
let a = 1i32
let b = a
println(a)          // fine: i32 is copyable, `a` is still live
```

An aggregate (struct, enum, string, vector, array) is copyable only when it
declares `impl Copy for T {}` — an explicit statement that the type is safe to
duplicate bitwise. The compiler validates the declaration: `T` has no `Drop`
impl and every field/element of `T` is itself `Copy`. Scalars and raw pointers
are the base case and need no declaration. A type that needs a deep duplicate
writes an ordinary named method that does not claim to be invisible.

Explicitness matters because "all fields `Copy`" does not imply the type is
`Copy` — ownership can hide behind a `Copy` field. `String` is the canonical
example: `*mut u8` and `usize` are both `Copy`, so an automatic structural rule
would mark `String` `Copy` — wrong, because `String` owns the buffer behind
`data` and must run `Drop` to free it. `String` therefore simply does not write
`impl Copy`. `Point`, `Pair`, `Counter`, and any plain-data record become `Copy`
by writing `impl Copy for Point {}`; `String`, `Vec`, and `dyn Trait` never do.

**Element reads copy.** `Vec.get(i)` and `Array.get(i)` copy the element out of
the container and leave it in place. They are allowed only for `Copy` elements,
expressed as a `T: Copy` bound on the method
(`fn get[T: Copy](read v: Vec[T], i: usize) -> T`), not as a representation
heuristic. Reading a non-`Copy` element — a `Drop`-implementing struct, an enum,
`String`, `Vec`, or `dyn Trait` — is a plain type error at the call site
(`type S does not implement trait Copy`), because the copy would alias the
stored value and double-free on drop. Non-copyable elements are accessed by move
(`own`), whole-structure methods (`set`, `swap`), or a `match` binding (§6).

**Nothing is ever silently duplicated.** Copyability is the explicit `impl Copy`
declaration for aggregates, plus the unconditional scalar/pointer rule. An
aggregate is never copied by accident.

**Destructors take owned `self`.** `Drop` is an ordinary consuming function:
whatever it does not move out is destroyed automatically.

```prim
impl Drop for Vec {
    fn drop(mut self) { ... }
}
```

Drop is elaborated per type: a composite holding a `Drop` field runs that
field's destructor (recursively, through structs, tuples, arrays, and the
active variant's payload of an enum) before the drop site reclaims the value's
place. See §10.

## 3. Binding modes: `read`, `mut`, `own`

Three modes. Keywords, not sigils — these are permissions on a binding, not
pointers. A mode always sits **immediately left of the thing it qualifies**: the
parameter name at a declaration, the binding name in a match arm, the argument
expression at a call.

```prim
T           owned value
read v: T   shared view, for the duration of one call   (read claim)
mut v: T    exclusive view, for the duration of one call (write claim)
own v: T    owned value, moved in
```

Where a mode may appear:

| Position                    | Syntax                          | Default (bare) |
|-----------------------------|---------------------------------|----------------|
| parameter, receiver         | `fn len(read v: Vec[T])`, `fn bump(mut p: Point)`, `fn eat(own p: Point)` | `read` |
| match-arm binding           | `Some(read v)`, `Some(mut v)`, `Some(own v)` | `read` (borrow); `own` consumption is inferred |
| call argument               | `f(x)`, `f(mut x)`, `f(own x)`  | `read` |
| `let` binding               | `let mut x = ...`, `let own x = ...` | owned (move) |

`read` / `mut` / `own` are **never** a type. They cannot appear in a return
type, a field type, a generic argument, a `let` annotation, or as a borrow
expression (`let r = read p`). Each is a pointed compile error: *"references are
second-class"*.

**`read` and `mut` exist for one call only.** `fn len(read v: Vec[T])` does not
create a reference value — it grants the callee read access to the caller's
place `v` for the duration of the call. When the call returns, the access is
gone. Nothing is left behind. `mut` is the same with write access: `fn
bump(mut p: Point)` means the callee may read and write `p`, and its writes are
visible to the caller when the call returns.

**`own` is a move, not an access.** `fn eat(own p: Point)` transfers ownership:
the caller no longer owns `p`, the callee may destroy it, and it leaves the
callee only through the return channel. `own` is the *ownership* axis; `read`
and `mut` are the *access* axis. (`own` is a move, not a data-flow direction,
which is why it is not named `out`.)

**At call sites, `mut` is the mandatory mark.**

```prim
render(config)        // read — unmarked
tune(mut config)      // mutation is visible; the mark
archive(own config)   // consumption — explicit today
```

Mutation is marked because it is the one effect that is *not* recoverable from
later checking: after `tune(mut config)` the name is alive and *different*, and
nothing downstream reveals it. (Consumption today is also explicit — `f(own x)`
— and the checker requires call-site modes to match the parameter's declared
mode. Making consumption unmarked, so `mut` is the *only* call-site mark, is a
documented deferred change: see §12.)

**A borrow parameter has the plain type `T` inside the body.** There is no
reference type to unwrap. Storing it, returning it, or moving out of it are all
rejected by the move checker (§5), because a borrowed parameter is a *place in
the caller* wearing a plain type — moving it would move out of someone else's
binding. Re-borrowing it is not storage — see rule 5.

**A `let` always moves (or copies).** `let x = a` transfers ownership of a
non-copyable `a`; `let own x = a` is the explicit, redundant spelling of the
same move. `let mut x = ...` is a mutable *owned* local — `mut` there qualifies
the binding's mutability, a different (and unrelated) keyword position. `let
read x` is a parse error: a local cannot hold a second-class borrow.

## 4. Second-classness: what it buys

Because no reference is a value, *"a borrow cannot outlive its frame"* is a
**theorem of the type system**, not a checked property. There is no reference
type, therefore no reference value, therefore nothing that *can* be stored,
returned, sent through a channel, captured, or placed in a field.

- **Re-borrowing** down a call chain is fine: `f(x)` → `g(x)` → `h(x)` nests
  frames; each callee's access dies with its frame. No bookkeeping.
- **Suspension** is fine and needs no rule: a `mut` parameter held across a
  `yield` lives in the callee's frame, which the green-thread engine keeps
  intact; the caller's place is not shared with any other task, so nobody can
  race it.
- **No borrow crosses a task boundary**: `spawn` and channels carry owned data;
  a reference is not a value, so there is nothing to send. Concurrency safety
  is structural.
- **No borrow is stored or returned**: the callee's parameter has the plain
  type `T`; storing or returning it is a move of a borrowed value — already an
  error.

Rust answers "how long is this reference valid?" per instance, so the answer
must be a variable that every container and signature re-declares (lifetimes,
variance, outlives bounds). Prim imposes one universal answer — *no longer than
the call* — so the type system needs no parameter at all, and the checker
shrinks to rules local to a single call site.

## 5. The checker

The borrow checker as a separate pass is gone. What remains is the **move
dataflow** (a CFG analysis that tracks initialization, moves, and drop points)
plus five rules, all local to a body or a call site:

1. **Move dataflow** (unchanged): `read`/`mut` parameters are tracked and
   unmovable — moving one (or a field of one) out of the body is
   `BorrowEscapes` / `MoveOutOfBorrow`. This is what enforces second-classness
   *inside* a body.
2. **`MutAlias`**: the same place may not be passed `mut` twice, or `mut`
   together with any other mode, in one call.
3. **`ModeMismatch`**: a non-copyable *place* argument must be passed with the
   parameter's declared mode — `f(x)` for an `own` parameter, or `f(x)` for a
   `mut` parameter, is an error unless marked.
4. **`MutOfRead`**: you cannot `mut`-borrow a value reachable only through a
   `read` parameter or a `read`/bare arm binding — including a `match mut`
   whose scrutinee is rooted at one — because exclusive write-back through a
   shared borrow would alias the caller's box.
5. **A reference is never stored.** The move checker already rejects storing a
   borrow in a `let`, a field, a return, or a channel: a `read`/`mut` parameter
   is unmovable, so it can't leave its frame. Trait objects add one explicit
   case — a trait object is a pointer to its value, so coercing a borrow into a
   trait object and *keeping the object* (`let g: Trait = x`, a field, a return)
   stores a reference and is rejected (*"cannot box a borrow into a trait
   object"*). But a borrow may be re-wrapped into a trait object that *stays
   second-class*: passing a `read T` where a `read Trait` parameter is expected
   (with `T: Trait`) is allowed — the trait-object view is a borrow of a borrow
   and dies with the call. Re-borrowing is free; storing is forbidden.

Checks run one function at a time, using signatures for everything they call.
There is no interprocedural analysis, no constraint solving, and no region
algebra.

## 6. Match: borrow-in-a-scope

`match` arms get the same second-class bindings as parameters — the mode keyword
left of the binding name:

```prim
match e { Some(read v) => v.n, None => 0 }   // v: read borrow, lives for the arm body
match mut e { Some(mut v) => v.n = v.n + 1 } // exclusive; writes copy back to the scrutinee
match own e { Some(own v) => v }             // explicit consume
```

- `read v` / `mut v` in an arm are second-class exactly like a parameter:
  unmovable, dying at the arm's end. An arm is, semantically, a function from
  the scrutinee to the arm value, whose parameters are the arm bindings.
- Scrutinee modes: bare `match e` = read, with `own`-arm consumption still
  **inferred** (whether any arm moves a payload out). `match mut e` is required
  for `mut` arms — mutation stays explicit (a `mut` arm binding under a
  bare/`read`/`own` match is a typecheck error, since it would write back
  through a non-exclusive match). `match own e` is documentation of the
  inferred consume — it never *forces* consumption, and is rejected when the
  arms don't actually move a payload, so the documentation can't rot.
- A consuming match may not *borrow* a payload out of the scrutinee: the
  scrutinee is owned and freed by the match, so a `read`/`mut` binding in a
  consuming match is rejected.
- `match mut` on a copyable scrutinee is rejected at typecheck (a `Copy` value
  has no place to mutate). `mut`-binding writes to scalar payloads (and scalar
  aggregate fields) are copied back into the scrutinee's place at arm end;
  a `mut` binding of an aggregate payload aliases the box directly.

Match is the language's built-in *borrow-in-a-scope*: it is what lets a
borrowed enum's payload be read (`fn fmt(read self, ...) { match self {
Some(read v) => ... } }`), with no first-class function values required.

## 7. Access: copies plus whole-structure methods

Without stored or returned references, element access is method-mediated:

- **Read an element**: `Vec.get(i) -> T` / `Array.get(i) -> Option[T]` return a
  copy (copyable elements only, §2).
- **Write an element**: `Vec.set(i, v)`, `Vec.push(v)`, `Vec.swap(i, j)` mutate
  in place, plus `mut self` methods for whole-structure operations.
- **Hold a view across statements**: impossible by design. In-place edits are
  index-plus-method, or a `match` binding (§6). Long-lived relationships use
  copies or identities (§8).
- **Reference-yielding iterators**: gone. Index loops are the v1 replacement.

The classic companion — a callback whose parameters are themselves
second-class (`v.with_mut(i, |e: mut T| ...)`, a borrow that lives exactly as
long as the callback call) — needs first-class function values and is a
documented follow-on (§12). It does not change the model, only the ergonomics of
in-place access.

## 8. Identities: the third leg

Values are data; `read`/`mut` are temporary, call-scoped access; **identities**
are durable names for things. *Borrows are for stack-shaped time; identities are
for heap-shaped time.* Long-lived relationships are explicit data, never
pointers — the discipline of every database and every OS file descriptor.

This half of the model is the retained forward-looking design (not yet
implemented — see §12):

- **Arenas.** `Arena[Node]` owns storage; `NodeId` is plain data with a
  generation, so a stale id is a caught error rather than corruption.
  Graph-shaped structure — parent links, cross-links, DAGs — lives here.
- **`Shared[T]`.** The declared escape hatch for shared mutation: exclusivity
  checked at runtime, visible in the signature, poisoned if a task panics
  mid-mutation. Duplication is an explicit `.share()`, never copy.
- **Trees, two regimes.** Hierarchy-only trees (nodes own children) are plain
  values — the call stack is the claim stack. A tree with parent pointers or
  sharing is arena plus ids, by construction.

The guidance when the checker objects stands: **copy the extracted thing** — the
element, not the container — and continue. *Make it run with copies; make it
fast with proofs.*

## 9. Representation and the unsafe core

Because programs cannot observe representation, the compiler owns it. A wasm
local holds one scalar (`i32`/`i64`/`f32`/`f64`), never a multi-word struct, so
value semantics need one of two local representations, chosen per type:

- **Flat `Copy` aggregates — scalarized.** A *flat* type — every field a
  scalar/pointer, no `Drop`, no recursion — is represented as its leaf fields,
  one wasm value each. A struct/tuple literal of a flat type is emitted as
  field values on the stack (no allocation); a copy copies each leaf, and a
  move is the same with the source dead (no runtime cost). `Point`, `Pair`,
  `Counter`, and a 20-byte flat record are all zero-allocation values.
- **Every other aggregate — a *place*, not a box.** A value that is not flat
  (`Drop`, enum, array, recursion, or too large) lives in a *place*: a region
  of linear-memory scratch, with a wasm local holding a pointer to it. What
  makes it a value type rather than a heap box is that ownership, copy, and
  move are value-level and the pointer is invisible (§1): a place has exactly
  one owner, `let b = a` *moves* (bytes copy into `b`'s place and `a`'s place
  dies, or `b` takes `a`'s place — never two live owners), copy is allowed only
  for `Copy` types, and drop runs the destructor on the place then reclaims it.
  Concrete aggregates — struct, tuple, enum, array, including `Drop` types — are
  inline values: nested aggregates sit at their field offset (not behind a
  per-field sub-box), enums are fixed-size tagged unions (a discriminant plus
  the max-variant payload, so `Option[i32]`/`Result` allocate nothing), and
  arrays are a fixed inline run of elements.

Calls follow the same split: flat `Copy` params/returns use the scalar ABI;
`read`/`mut` params pass a pointer to the caller's place (with `mut` writing
through it); large/owned non-`Copy` params pass a pointer to the source place as
the move — no deep copy, no heap box. `read`/`mut` match bindings point into the
inline tagged-union payload.

Only genuinely dynamic storage heap-allocates: `Vec`'s element buffer, `String`'s
byte buffer, and `dyn Trait` data (its `{vtable, data_ptr}` indirection is
intrinsic to dynamic dispatch). Everything else — `Option`/`Result`/plain
structs/tuples/enums/arrays — is an inline value.

Semantically, `mut T` *is* copy-in/copy-out — `fn bump(c: mut Counter)` means
`Counter -> Counter`. In-place mutation is an optimization the program cannot
detect. This is the master property the checker exists to preserve: **every
accepted program can be rewritten to copy-in/copy-out form with identical
meaning.** The hypotheses are `Shared[T]`, FFI, and the unsafe core below.

### 9.1 The unsafe core: raw pointers

Raw pointers (`*mut T`, `*const T`) are the escape hatch. They are **not safe
and cannot be made safe**: a deref is a raw load/store whose soundness depends
on the global aliasing and lifetime state of linear memory, which a bare
`*mut T` deliberately does not carry. Making it safe would mean reinventing the
borrow checker — which is what `read`/`mut` already are.

The boundary is therefore an **`unsafe` marker at the point of use**, not a
proof:

- An `unsafe { ... }` block, or the body of an `unsafe fn`, may use the raw
  powers: `*p` (load/store), integer↔pointer or byte-region↔typed-pointer
  reinterpretation (`at`, `from_addr`), `drop_in_place`, the allocator, and raw
  byte I/O. Calling an `unsafe fn` likewise requires an `unsafe` context.
- Outside an `unsafe` context these are compile errors. Naming, storing, and
  passing `*mut T` as an *opaque value* stays allowed (pointers are `Copy`), so
  a `Vec` handle can be a safe value even though its `ptr` field is `*mut T`.
- **Computing an address is safe**: `null`, `addr`, and the wrapping arithmetic
  (`add`, `sub`, `offset`, `byte_add`, `byte_sub`, `byte_offset`) only produce
  or inspect a pointer's numeric address — they touch no memory and claim no
  pointee lives there (mirroring Rust's `ptr::null` / `addr` / `wrapping_add`).
  Safe code cannot deref the result anyway.
- The compiler still runs its normal checks inside `unsafe` bodies — move
  dataflow, `Drop` elaboration, bounds checks where present — so it catches
  local mistakes. But it does **not** claim to prove the raw derefs sound; the
  `unsafe` author carries that obligation, exactly like Rust's `unsafe`.

The raw-core std modules (`std.mem`, `std.ptr`, `std.vec`, `std.array`,
`std.string`) mark their raw internals `unsafe fn` / `unsafe { ... }` behind
safe APIs. Everything else is safe code.

## 10. Panics and destruction

Destructors run at statically known points — last use, scope end, or the
owner's destruction — on the normal path. A `panic` aborts the program: it
prints the message and a sentinel, then traps, and `prim run` turns the sentinel
into a nonzero exit. **Allocation failure aborts the same way**: `std.mem.alloc`
never returns null — a request that cannot be served (memory exhausted, or a
size that would overflow the 32-bit heap) aborts with an out-of-memory message
and the same nonzero-exit sentinel. In a no-GC runtime an allocation that
cannot be served has no recovery path, and returning null would only push a
null-dereference onto every caller; aborting is the honest, corruption-free
behavior. There is **no unwinding** and destructors do **not** run on the
panic path.

Invariants may be broken inside an exclusive `mut` claim, so unwinding to run
destructors would have to run them over half-updated values. Prim aborts
instead, and confines the loss to the panic case (which is a failed program,
not a recoverable control-flow path). Resources held in values are reclaimed at
the owner's normal drop points; a panic is the process ending, not a leak to
manage. (`Shared[T]`, when it lands, will need a poison rule — already part of
its design in §8.)

## 11. Guarantees and accepted losses

Safe code cannot exhibit: use-after-free, use-after-move, double-free, data
races, iterator invalidation, aliased mutation, observation of a half-updated
value, a borrow outliving its call, or a borrow escaping into storage, a
channel, a return, or a stored trait object. Memory is reclaimed at statically known
points. The only runtime costs are generational id checks and `Shared[T]`
exclusivity checks — each elidable by proof, each at a source location you can
point at.

Accepted losses, with open eyes:

1. **No returned references.** Accessors return copies (copyable types) or need
   callbacks/identities.
2. **No views held across statements.** In-place edits are method- or
   callback-mediated. (Mitigation: `mut T` is semantically copy-in/copy-out, so
   a `mut self` method that rebuilds and assigns is always expressible.)
3. **No view structs.** A borrow-holding composite is unrepresentable; a parser
   over an input becomes copy-based, index-based, or identity-based.
4. **No reference-yielding iterators.** Index loops (and future callbacks) cover
   it.

Compensations: the checker largely disappears; concurrency is safe by
construction; storing a borrow in a trait object is rejected by construction;
several long-standing borrow-polish gaps become moot.

## 12. Conformance

Implemented today: `read`/`mut`/`own` as second-class binding modes with
read-default parameters and `self` receivers; call-site `mut`/`own` marks;
`let` moves with `let mut`/`let own` bindings; match-arm `read`/`mut`/`own`
bindings with inferred consumption and `match mut` write-back; the move
dataflow plus the five checker rules (§5); `Drop`/RAII with recursive field
drops over structs, tuples, arrays, and enum payloads; explicit `impl Copy`
(validated opt-in) with `T: Copy`-bounded element reads; inline aggregate
values (one place per owned value) with an explicit `unsafe` core gating raw
pointers; a dlmalloc port in `std.mem` that aborts on OOM instead of returning
null (allocation failure is fatal, §10) and verifies memory-grow contiguity;
green threads
(`spawn`, cooperative `yield`, a multi-task scheduler with blocking
park/poll).

Not yet implemented, in no particular order (details and current bugs in
`KNOWN_ISSUES.md`):

- **Unmarked consumption** (§3) — `mut` as the *only* call-site mark, with each
  argument's effect derived from the callee's parameter mode.
- **Callbacks** (§7) — `v.with_mut(i, |e: mut T| ...)`, which needs first-class
  function values.
- **Identities** (§8) — arenas with generation-checked ids, `Shared[T]`,
  splitters. Design only.
- **Borrow-to-trait-view coercion** (§5) — passing a `read T` where a
  `read Trait` parameter is expected (with `T: Trait`) is intended but not yet
  accepted: the checker currently rejects *every* coercion of a borrow,
  including the harmless argument-position case. Storing a borrow in a trait
  object stays rejected.
- **Owned trait-object coercion is an untracked borrow** (bug) — coercing an
  *owned* value into a trait object aliases its box; moving or dropping the
  source while the object is live is a use-after-free. The owned case must be
  treated as a move.

`KNOWN_ISSUES.md` carries the detail and the current bug list.
