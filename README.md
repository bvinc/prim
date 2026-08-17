Prim is a programming language that values simplicity, safety, and a useful and rich type system. It draws inspiration from both Rust and Go.

## Design Goals

1. **A language that is useful for AI.** High assurances, high performance, fast
   compilation, and **local reasoning**: a function's signature states
   everything it can read, mutate, or consume, so any piece of code can be
   understood and verified in isolation, without tracing effects across the
   program. Memory safety, data-race freedom, and deterministic resource
   handling are enforced at compile time, so generated code can be trusted
   without a runtime safety net. No GC pauses and value semantics keep
   performance predictable. A small, simple compiler compiles quickly, so the
   AI code-and-verify loop stays tight.

2. **No garbage collection.** Memory is managed through ownership and
   compile-time move checking — not a GC. The programmer always knows when
   memory is freed; there are no pauses and no tracing.

3. **Green threads.** Lightweight, cooperatively-scheduled green threads built
   on the WebAssembly stack-switching (typed-continuations) proposal. Each task
   is a continuation whose stack the engine grows and manages, enabling
   millions of concurrent tasks without OS thread overhead.

4. **Strong types with control of aliasing.** Ownership is enforced at compile
   time (use-after-move, use-after-free, and data races are rejected). Aliasing
   is controlled by **second-class references**: `read`/`mut`/`own` are binding
   modes (`fn len(read v: Vec[T])`, `match mut e { Some(mut v) => ... }`,
   `f(own x)`), never types — no reference can be stored, returned, or escape
   its call, so shared mutable state exists only where it is explicitly
   allowed.

### Current Status

The compiler implements basic types, structs, enums, traits with dynamic
dispatch, generics, functions, control flow, modules, type inference, and
compile-time ownership (second-class `read`/`mut`/`own` binding modes,
CFG-based move checking, drop elaboration). The runtime provides a Prim-written
allocator, basic I/O, and cooperative green threads — `spawn`, `yield`, a
multi-task scheduler, and blocking park/poll — built on wasm continuations.

Primitive integer types: u8, i8, u16, i16, u32, i32, u64, i64, usize, isize.
Primitive floating point types: f32, f64.

## Let expressions

```
let x: u32 = 0
let x: u32 = 0u32
let x = 0u32
```

## Structs

```
struct Point {
    x: f64,
    y: f64,
}
```

## Functions

```
fn double(x: u32) -> u32 {
    let prod = x*x
    prod // return can be omitted
}
```

## Methods

```
impl Point {
    fn x(read self) -> f64 {
        self.x
    }
}
```

## Control Flow

```
if x == 5 {
    println("It is 5")
}

loop {
    println(x)
    break
}
```

## Git Hooks

To enforce formatting, linting, and tests on each commit, this repo includes a pre-commit hook under `.githooks/pre-commit` that runs:

- `cargo fmt --all -- --check`
- `cargo clippy --workspace --all-targets --all-features -D warnings`
- `cargo test --workspace --all-targets`

## Running / Staging

The CLI resolves the standard library and runtime via `PRIM_ROOT`. For reliable local runs, use `./build.sh` to stage a runnable tree under `target/{debug,release}/dist` and run `bin/prim` from there (or set `PRIM_ROOT` yourself to point at the staging root).

Enable it for your local clone:

```
git config core.hooksPath .githooks
```

Now `git commit` will fail if formatting or Clippy checks fail.
