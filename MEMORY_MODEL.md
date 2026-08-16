# Prim Memory Model

**Status: superseded.** The borrow-as-a-type model in this document (`read T` /
`mut T` reference types, the `data`/`view` kind system, provenance, the loan
checker) has been replaced by **second-class references**: `read`/`mut`/`own`
exist only as parameter modes, match-arm bindings, and call-site argument marks —
never as types. See `PLAN_SECOND_CLASS_REFS.md` (§1–§7) for the current model.
This file is retained for history.

The whole model in one sentence: *reading is the default going in, owning is the
default coming out, mutation is never a default anywhere, and a borrow may never
outlive the frame it came from.*

There are no lifetime annotations. There is no region algebra. A borrow's extent
is not a variable to be inferred — it is structural, and the type system carries
exactly one inferred bit about it: the **kind**.

---

## 1. Principles

1. **Value semantics.** Every value has one owner. Assignment and argument
   passing transfer or claim; they never create observable aliases. Two live
   names never observe the same mutable state.
2. **No garbage collection. Deterministic destruction.** Every value is
   destroyed at a statically known point — last use, scope end, or its owner's
   destruction — running its destructor.
3. **Exclusivity is the law.** Mutating a place requires exclusive access to it.
   While a shared claim on a place is live the place is immutable; while an
   exclusive claim is live the place is unreachable by anyone else.
4. **Representation is not observable.** Values have no address and no pointer
   equality. What a `read T` compiles to is the compiler's choice (§9).
5. **Aliasing is deleted, not managed.** A function can touch only what its
   signature names, so every function's frame is its signature.

## 2. Values: move, copy, trivial

**Move is the default.** `let b = a` transfers ownership; `a` is dead afterward
and any later use is a compile error. A move is a fact about names, not an
operation on values — it needs no trait and no hook, and the compiler may
implement it as a pointer hand-off rather than a copy.

**Copies are explicit** and never inserted by the compiler:

```prim
let backup = config.copy()
```

`Copy` is a trait with one method. An empty impl means "copy field by field";
a written body overrides it, which is how types with owned buffers copy deeply.

```prim
trait Copy {
    fn copy(read self) -> Self
}

impl Copy for Config {}          // field-wise, compiler-generated

impl[T: Copy] Copy for Vec[T] {  // deep: O(n), honest, explicit at call sites
    fn copy(read self) -> Vec[T] { ... }
}
```

> **The copy law.** `x.copy()` yields a value observationally equivalent to `x`
> and independent of it.

There is no `clone`. The shallow/deep split is an artifact of pointer languages;
value semantics collapses it into one operation whose implementations vary under
one law. Duplication that produces *observable* aliasing can never be `Copy` —
it gets an ordinary named method (`shared.share()`, `file.dup()`) that does not
claim to be invisible.

**Trivial types are exempt from move-kill.**

```prim
impl Trivial for Point {}

let a = Point { x = 1i32, y = 2i32 }
let b = a
println(a.x)        // fine: `a` is still live
```

Eligibility is checked at the impl: every field is `Trivial`, no `Drop`, not
`Consume`, not view-kinded. `Trivial` implies `Copy`. There is **no size cap** —
the justification for the exemption is that these types own nothing and protect
nothing, so "the source is dead" would be a rule without a purpose. A cap would
be an arbitrary cliff that a purely quantitative edit could fall off, and it
cannot survive generics: `Pair[Point, Point]` satisfies every field-wise
condition but would breach any fixed byte bound.

**Must-consume types** opt into linearity. A `Consume` value cannot be silently
dropped; it must be consumed exactly once on every non-panic path.

```prim
impl Consume for Txn {}
```

Every `Consume` type must still implement `Drop` for the unwind path (§10).

**Nothing is ever silently assigned.** `Copy`, `Trivial`, and `Consume` are
opt-in and checked against the fields. `view` is a structural fact and therefore
mandatory acknowledgment (§4). An unmarked struct is not an absence of
information — it is a verified claim that the type is plain, movable, storable
data. Adding a field to an unmarked struct can never change its contract; adding
a disqualifying field to a declared one is an error at the declaration, naming
the field chain.

**Destructors take owned `self`.** A destructor is an ordinary consuming
function: it may destructure and move fields out. Whatever it does not move is
destroyed automatically.

```prim
impl Drop for Vec {
    fn drop(mut self) { ... }
}
```

## 3. Access modes

Three modes. Keywords, not sigils — these are permissions, not pointers.

```prim
T           owned
read T      shared view      (read claim)
mut T       exclusive view   (write claim)
```

Where each keyword appears is not a table to memorize. It follows from two rules
and one invariant:

> **Rule 1 — input/output.** Positions where a value flows *in* to be used —
> parameters, receivers, match scrutinees — default to `read`. Positions where a
> value flows *out* or comes to rest — returns, fields, generic arguments, local
> bindings — default to `own`.
>
> **Rule 2 — locality.** A mark is required only when the behavior is neither
> locally evident nor recoverable from later checking.
>
> **Invariant — `mut` is never a default. Anywhere.**

| Position                     | bare `T` means |
|------------------------------|----------------|
| parameter, receiver          | `read`         |
| match scrutinee              | `read`         |
| return, field, generic arg   | owned          |
| local binding                | owned (move)   |

```prim
fn peek(p: Point) -> i32          { return p.x }      // read
fn bump(p: mut Point)             { p.x = p.x + 1i32 }
fn eat(p: own Point) -> i32       { return p.x }      // consumes

impl Point {
    fn get(self) -> i32       { return self.x }       // read self
    fn shift(mut self)        { self.x = self.x + 1i32 }
    fn into_x(own self) -> i32 { return self.x }
}
```

**At call sites, `mut x` is the only mark.**

```prim
render(config)        // read — unmarked
tune(mut config)      // mutation is visible; the one mandatory mark
archive(config)       // consumption — unmarked
sock.close()          // own receiver — unmarked, same rule
```

Consumption is unmarked because it is recoverable: if the name is used later,
that use is a precise compile error at the place the mistake actually is; if it
is never used again, the consumption changed nothing a reader needs. Mutation is
not recoverable — after `tune(mut config)` the name is alive and *different*,
and nothing downstream reveals it. So mutation is marked and consumption is not.

Marks that *declare* stay; marks that *echo a signature* go. `match` therefore
keeps explicit modes, because no signature exists there — the scrutinee mode is
itself the declaration:

```prim
match tok { ... }            // read (default)
match mut tok { ... }        // mutate payloads in place
match own tok { ... }        // consume the scrutinee
```

Likewise a binding that introduces a view says so:

```prim
let r = read grid[i]
let s = mut grid[j]
```

## 4. Kinds: `data` and `view`

Every type is one of two kinds.

- **`data`** — owns everything it holds, or refers to things by identity (§8).
  May go anywhere: fields, heap, channels, other tasks, disk.
- **`view`** — transitively contains at least one claim on someone else's place.
  Frame-bound.

`read T` and `mut T` are view-kinded axiomatically; a composite is view-kinded if
any component is.

**The four bans.** These replace region algebra entirely. A view-kinded value
cannot:

1. be stored into a data-kinded type,
2. travel in a channel message, or enter any task that may outlive the source
   frame,
3. be captured by an escaping closure,
4. outlive the frame it derives from.

Views *do* survive blocking calls — green threads suspend whole stacks intact —
and *do* flow into scoped spawns and fork/join, where the child joins before the
scope exits and the source outlives the consumer by construction.

**Why this works where lifetimes were needed.** Rust answers "how long is this
reference valid?" per instance, so the answer must be a variable that every
container and signature re-declares, which drags in variance, outlives bounds,
and HRTB. Prim imposes one universal answer — *no longer than the deriving
frame* — so the type system needs one bit instead of a parameter. Composite
views inherit a **restriction**, not a **region parameter**. Restrictions compose
silently; parameters compose loudly.

**Acknowledgment.** A type with a field whose type *syntactically* contains
`read` or `mut` must be declared `view`, or its definition is an error:

```prim
struct view Parser { input: read String, pos: usize }
enum view Tok { Ref(read Point), Eof }
```

**Kind through generics.** When view-ness comes from a *type parameter* rather
than a syntactic borrow, no acknowledgment is required and the kind is computed
per instantiation:

```prim
struct Map[I, F] { inner: I, f: F }   // no `view` marker, and none needed
```

`Map[VecIter[T], F]` is view-kinded; `Map[Counter, F]` is data-kinded. This is
not a hole in "nothing silently assigned": the author of `Map` declared nothing
about `I` and so has nothing assigned to them, and the instantiator who chose a
view wrote the `read` themselves, in their own code, in view. Requiring a
conditional marker here would make every generic container carry a clause that
merely restates its own field list.

Kind is therefore checked twice: syntactically at each definition, and again per
instantiation after monomorphization.

## 5. View structs and enums

A view type is first-class *within its frame*: construct it, read its fields,
pass it by `read` or `mut`, nest it, use it as a generic argument.

```prim
struct Point { x: i32, y: i32 }
struct view Window { p: read Point }

fn width(w: read Window) -> i32 { return w.p.x }

fn main() {
    let pt = Point { x = 3i32, y = 7i32 }
    let w = Window { p = read pt }
    println(width(read w))
}
```

Constructing a view type makes all of its claims at once. This is what makes the
choice between borrowing and recording visible in the type: a `Expr` holding
`read String` slices is view-kinded and cannot outlive its input, while an `Expr`
holding `{ start, len }` spans is data and can be stored forever. *View for now,
identity for keeps.*

## 6. Derivation: returning a view

A function may return a view only when its signature says where the view came
from. The source is written **in position**, on the view introducer itself:

```prim
fn get(self, i: usize) -> read(self) T
fn choose(a: read Point, b: read Point) -> read(a) Sel
fn pick(a: mut Vec[T], b: mut Vec[T]) -> (mut(a) T, mut(b) T)
```

The introducer names the **strength** of the claim and the parentheses name its
**source**. That reads uniformly whether the returned type is a bare borrow or a
nominal view type: `read(p) T` is a shared view of `p`, `read(a) Sel` a `Sel`
whose claim on `a` is shared, `mut(a) Writer` one whose claim is exclusive. There
is no second form for "owned value of a view type" — a view of a view collapses
(§7), and a view-kinded value is frame-bound either way, so the distinction would
carry no information.

For a nominal view type the introducer is redundant with the type's own
declaration and is written only when it carries a source; `read(p) T` keeps its
keyword always, since dropping it would leave an owned `T`.

Placement is the only notation; there is no trailing clause. A return may hold
several view positions, and only a per-position source can say where each one
comes from — `pick` above has no trailing spelling at all.

**Elision.** The source may be omitted when exactly one candidate exists: a sole
borrowed parameter, or failing that a borrowed receiver. So `Vec.get` is normally
written `-> read T` and `Vec.iter` as `-> VecIter[T]`. `choose` and `pick` must
name their sources, and are precisely why the notation exists.

Written out, with the source doing visible work at the call site:

```prim
struct Point { x: i32, y: i32 }
struct view Sel { p: read Point }

fn choose(a: read Point, b: read Point) -> read(a) Sel {
    return Sel { p = read a }
}

fn main() {
    let mut p = Point { x = 1i32, y = 2i32 }
    let mut q = Point { x = 3i32, y = 4i32 }
    let s = choose(p, q)

    q.x = 99i32        // ok: the signature says `s` claims `a`, not `b`
    p.x = 99i32        // error: `s` claims `p`, and `s` is live below
    println(s.p.x)
}
```

Both parameters are borrowed for the duration of the call; only `a` stays pinned
afterward. Without `read(a)` the caller would have to pin both — or read the
body, which is the thing signatures exist to prevent.

**Several views from one source.** Each view position is an independent root in
the caller's ledger, held under a single claim on the source that ends at the
last use of the last root:

```prim
impl Vec {
    fn get2(mut self, i: usize, j: usize) -> (mut(self) T, mut(self) T) { ... }
}

fn swap_two(v: mut Vec[Item], i: usize, j: usize) {
    let (a, b) = v.get2(i, j)
    let t = a.n
    a.n = b.n          // independent roots: no conflict with each other
    b.n = t
}                      // both die here; the claim on `v` ends
```

Nothing declares the two results disjoint, because nothing needs to: §1's
exclusivity law already says two live `mut` views are exclusive of each other, so
the signature asserts it by existing. The obligation lands where it belongs — on
the body:

```prim
fn halves(p: mut Pair) -> (mut(p) A, mut(p) B) {
    return (mut p.a, mut p.b)        // ok: siblings, statically disjoint
}

fn get2(mut self, i: usize, j: usize) -> (mut(self) T, mut(self) T) {
    return (mut *add[T](self.ptr, i), mut *add[T](self.ptr, j))   // rejected
}
```

`halves` checks and stays ordinary safe code. `get2` does not — index paths
conservatively overlap (§7) — so it is writable only below the seal (§9), where
its module declares the model "returns disjoint parts of `p`" and carries an
attestation, backed by a runtime `i != j` check that a discharged obligation
elides. Splitters are therefore not a language feature; they are the ordinary
notation plus a body the checker sends to the seal.

Rust reaches the same split by another road. `split_at_mut` type-checks at call
sites only because `&mut` is a first-class value — the caller records one loan
and two independent locals and never asks whether they overlap — while the body
is `unsafe`, audited by hand, with nothing in the signature saying so. Prim roots
views in paths instead, so a body that cannot prove disjointness is rejected by
the ordinary checker and has to move somewhere the trust is declared and tracked.

**Origin derivation.** `read(p) T` claims the place `p`, which is right for
returning a view *into* an argument. It is wrong for composite views that yield
views of *their* sources: an iterator's item would claim the iterator, blocking
the next advance. For those, the result claims the *origins* of `p`:

```prim
struct view VecIter[T] { v: read Vec[T], pos: usize }

impl Vec {
    fn iter(self) -> VecIter[T] { return VecIter { v = read self, pos = 0usize } }
}

impl VecIter {
    fn next(mut self) -> Option[read(origin self) T] {
        if self.pos < self.v.len {
            let x = self.v.at(self.pos)
            self.pos = self.pos + 1usize
            return Option.Some(x)
        }
        return Option.None
    }
}
```

This is not region machinery. The caller already records that `it` claims `v` —
that record *is* the invalidation lock — so "the result claims the origins of
this parameter" is a lookup in bookkeeping that already exists.

```prim
let mut it = v.iter()
let a = it.next()     // a claims v, not it
let b = it.next()     // fine: no live claim on it
use(a, b)             // held elements, through the iterator
v.push(9i32)          // error: a and b claim v
```

Adapters compose with no extra annotation, since a wrapper's item origins are a
subset of the wrapper's own.

**Callee obligation.** A returned view's provenance must trace to a
**view-kinded** component of the named source. Under `read(origin p)`, returning
a view of a **data-kinded** field of `p` is an error — `p` may die while its
origins live, so an origin-claimed view may only point at origin-owned data:

```prim
fn next(mut self) -> Option[read(origin self) usize] {
    return Option.Some(read self.pos)     // error: `pos` is data-kinded
}
```

The rule is stated on *kind*, not on ownership. `VecIter.pos` is illegal because
it is data, not because it is an owned field — `Map.inner` is also an owned
field, and deriving through it is exactly what makes adapters work.

**Caller obligation.** Extend the claims on the named places until the returned
view dies, using the signature alone.

Derivation facts are not variables. Nothing is quantified and nothing propagates
through generics.

## 7. Claims and the checker

A view is always derived from a **place** — a path such as `a.field[i].x` rooted
in a local or parameter. Creating a view registers a **claim**: static,
flow-sensitive accounting with zero runtime cost. No flags, no locks, no
refcounts.

It is a *claim* checker, not a borrow checker: nothing is borrowed as a value,
and claims on places are tracked in a ledger.

**Overlap.** Ancestor and descendant paths overlap; sibling fields are disjoint;
two index projections into the same container overlap conservatively, unless an
`i != j` obligation has been discharged. Enum payloads of different variants
overlap conservatively in v1.

**Rules.**

- A shared claim makes its path read-only; shared claims coexist.
- An exclusive claim makes its path unreachable by anyone else.
- A claim's extent runs from creation to the **last use** of its holder, not to
  scope end.
- Weakening is allowed — an exclusive claim may yield shared views of itself.
  Strengthening is impossible. Views do not nest; a view of a view collapses.
- Copying a view-kinded value registers a *fresh* claim held by the copy.

**Passes.** The checker runs after type checking, on one function at a time,
using signatures for everything it calls. There is no interprocedural analysis
and no constraint solving.

1. **Liveness** (backward) — last use of every binding.
2. **Moves** (forward) — initialization, moves, partial moves, consumption.
   `Consume` values must reach consumed state on every non-panic exit. Drop
   points fall out here.
3. **Ledger** (forward) — claim creation and the conflict checks above.
4. **Derivation** (forward) — provenance of every view-valued expression, and
   the callee obligation at each return.
5. **Kind escape** — the four bans, at store, send, capture, and return sites.

**Call timing.** A call's claims — including the receiver's — activate at the
invocation point, after every argument subexpression has evaluated. So
`v.push(v.len())` is legal: the read claim from `v.len()` has already ended when
`push`'s exclusive claim begins. A view created *explicitly* in argument
position (`g(read v[0])`) is a binding claim and lives for the callee's use of
it, not merely for the call.

**Signatures, and one exception.** Claims propagate across calls through
signatures alone — with `read(origin p)` the single case where the effect is a
function of the caller's own ledger as well. It remains intraprocedural; it
means signature identity alone does not determine a call's claim effect, which
incremental recheck must account for.

**Granularity is an API decision.** Public fields let callers form paths and get
path-granular claims (`mut cfg.retries` beside `read cfg.name` — siblings,
disjoint). Private fields behind methods yield only `self`-rooted views, hence
whole-object claims — which *is* the iterator-invalidation guarantee, and is
correct for abstract structures whose invariants span the whole. Inside the
defining module, private fields are visible and implementation code gets full
path granularity.

**Diagnostics.** Every claim error names three points — the operation, the claim
that blocks it, and where that claim ends — plus fix-its: reorder, `.copy()`,
scope split, splitter API, or a declaration change anchored at the definition.

## 8. Identities: the third leg

Values are data; views are stack-shaped temporary access; **identities** are
durable names for things. *Views are for stack-shaped time; identities are for
heap-shaped time.* Long-lived relationships are explicit data, never pointers —
the discipline of every database and every OS file descriptor.

- **Arenas.** `Arena[Node]` owns storage; `NodeId` is plain data with a
  generation, so a stale id is a caught error rather than corruption.
  Graph-shaped structure — parent links, cross-links, DAGs — lives here. The
  cost: `arena[id]` claims the whole arena, so two simultaneous exclusive
  element accesses need a splitter, a proof, or sequencing.
- **`Shared[T]`.** The declared escape hatch for shared mutation: exclusivity
  checked at runtime, visible in the signature, poisoned if a task panics
  mid-mutation. Duplication is `.share()`, never `Copy`.
- **Trees, two regimes.** Hierarchy-only trees (nodes own children) are plain
  values — the call stack is the claim stack. A tree with parent pointers or
  sharing is arena plus ids, by construction.
- **Disjoint element access** goes through a splitter — `v.get2(i, j)`,
  `v.split_at(k)` — which is an ordinary multi-view signature (§6) whose body
  lives below the seal, runtime-checked and elidable by a discharged `i != j`.

## 9. Representation

Because programs cannot observe representation, the compiler owns it. `read i64`
travels in a register; `mut T` is a pointer or copy-in/copy-out, whichever wins;
slices are `(base, len)`; views into packed or computed fields materialize and
write back; large owned parameters pass by pointer because moves are semantic.

Semantically, `mut T` *is* copy-in/copy-out — `fn bump(c: mut Counter)` means
`Counter -> Counter`. In-place mutation is an optimization the program cannot
detect. This is why representation freedom is a theorem rather than a feature,
and it is the master property the checker exists to preserve: **every accepted
program can be rewritten to copy-in/copy-out form with identical meaning.** The
hypotheses are `Shared[T]`, FFI, and the small trusted core (allocator, buffer
primitive, splitters), each of which declares a model it is separately obliged
to refine.

## 10. Panics and destructors

A panic unwinds the task's stack, running destructors, to the task boundary and
no further; the parent observes the failure through the join.

**Invariants are not assumed on the unwind path.** A type invariant may be
broken inside an exclusive claim (§1); if a panic arrives there, the value is
destroyed mid-update. Destructors must therefore be invariant-agnostic — they
release resources and must not assume the type's invariant holds. This is the
price of unwinding at all, and it is confined to destructor bodies.

`Consume` types are consumed explicitly on normal paths and run their `Drop` on
the unwind path, so a rollback happens and a session peer sees a failure rather
than a hang. `Shared[T]` mutated during a panic is poisoned. A panic during
unwinding aborts.

## 11. Guarantees and accepted losses

Safe code cannot exhibit: use-after-free, use-after-move, double-free, data
races, iterator invalidation, aliased mutation, observation of a half-updated
value, a view outliving its source, or a leaked must-consume resource. Memory is
reclaimed at statically known points. The only runtime costs are generational id
checks, `Shared[T]` exclusivity checks, and splitter disjointness checks — each
elidable by proof, each at a source location you can point at.

Four things are inexpressible, accepted with open eyes:

1. **Views in heap or shared storage.** Use copies or identities. Rust's own
   ecosystem converged on identity handles voluntarily.
2. **Arena-interior pointer webs.** Generational ids cost an indirection and a
   check per hop. This is the one loss worth mourning.
3. **Per-region claim granularity.** Extracting one component of a multi-source
   view claims all its sources. Keep the sources as separate bindings, or copy.
4. **`'static` borrows.** No globals exist; unmourned.

Retained despite first appearances: scoped parallel borrows, views held across
blocking suspension, and request-scoped view collections as locals.

When the checker objects, the first move is to **copy the extracted thing** —
the element, not the container — and continue. Copies are semantically
invisible, most conflict copies are small, dead-source copies compile to moves,
and the rest are profiler-findable. *Make it run with copies; make it fast with
proofs.*

## 12. Conformance

Implemented today: `read`/`mut`/`own` with read-default parameters; `read`/`mut`
as tracked types; borrows inside aggregates; the `data`/`view` kind system with
`struct view` / `enum view` acknowledgment; single-source return provenance with
elision; a lexical claim checker enforcing shared-xor-mutable; `Drop` with
recursive field drops.

Not yet implemented, in dependency order:

1. **Unmarked consumption** (§3) — call sites still write `f(own x)`; the
   argument's effect must come from the callee's parameter mode instead.
2. **Placement notation** (§6) — the compiler parses a trailing `from p` clause
   and permits one view position per return. Placement supersedes it, and
   several view positions per return follow.
3. **Callee obligation** (§6) — a returned view is currently *trusted* to borrow
   its named source. Requires the provenance pass.
4. **Origin derivation** (§6) — needs the origin table.
5. **Per-instantiation kinds** (§4) — `Type::Param` is treated as data before
   monomorphization, so a generic holding a borrow through a parameter escapes
   the check.
6. **`Copy` / `Trivial` / `Consume`** (§2) — copyability is structural today
   (scalars and pointers), not declared.
7. **Non-lexical claims** (§7) — claims end at scope, not last use. A strict,
   backward-compatible tightening.
8. **Enum payload drops, arenas, `Shared[T]`, splitters** (§8, §10).

`KNOWN_ISSUES.md` carries the detail and the current bug list.
