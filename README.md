Prim is a programming language that values simplicity, safety, and a useful and rich type system. It draws inspiration from both Rust and Go.

## Design Goals

1. **Simplicity above all.** The language, compiler, and runtime should be as simple as possible. Fewer concepts, fewer special cases, less code.

2. **Green threads with moveable stacks.** The runtime provides lightweight, cooperatively-scheduled green threads. Stacks are growable and relocatable (copied/moved as needed, like Go), enabling millions of concurrent tasks without OS thread overhead.

3. **No garbage collection.** Memory is managed through ownership, borrowing, and lifetimes — not a GC. The programmer always knows when memory is freed.

4. **Strong types with a borrow checker.** Ownership and lifetime tracking are enforced at compile time, preventing use-after-free, data races, and dangling references without runtime cost.

### Current Status

The compiler implements basic types, structs, functions, control flow, modules, and type inference. The runtime provides allocation, basic I/O, and a transitional threading API. Ownership, borrowing, lifetimes, and green threads are not yet implemented.

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
    fn x(&self) -> f64 {
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
