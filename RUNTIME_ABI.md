Prim Runtime ABI
=================

This document defines the stable C ABI between Prim-compiled code and the runtime (`prim-rt`).

Goals
- Simple, portable C ABI surface (no Rust-specific mangling or layout).
- Keep symbol names and calling conventions stable as the language evolves.
- Support future green-thread scheduler without breaking existing callers.

Conventions
- Calling convention: C (`extern "C"`).
- Symbol prefix: `prim_rt_...` for runtime functions; compiler emits `prim_main` as program entry.
- Integers: pointers and `usize` are 64-bit on 64-bit targets; current toolchain targets 64-bit.
- Booleans: `i8` with 0 = false, non-zero = true.
- Strings/slices: to be specified later; current ABI exposes only scalar printing.

Process Entry
- `main(argc: i32, argv: *mut *mut u8) -> i32`
  - Provided by `prim-rt`; calls `prim_main()` exported by codegen.

Memory Management
- `prim_rt_alloc(size: usize, align: usize) -> *mut u8` (unsafe)
  - Allocates `size` bytes aligned to `align`. Returns null on failure.
- `prim_rt_free(ptr: *mut u8, size: usize, align: usize)` (unsafe)
  - Frees memory allocated by `prim_rt_alloc` with identical layout.

I/O Utilities
- `prim_rt_println_i64(x: i64)`
- `prim_rt_println_bool(b: i8)`
  - Convenience helpers for basic output; intended to replace direct libc calls in codegen.

Process Control
- `prim_rt_exit(code: i32) -> !`
  - Terminates the process with exit code.

Scheduling (Transitional API)
These provide a forward-compatible surface for a future green-thread scheduler. The initial
implementation uses OS threads and a simple handle table. The ABI will remain stable while the
internals can evolve to a M:N scheduler.

- `prim_rt_spawn(entry: unsafe extern "C" fn(*mut u8) -> i32, arg: *mut u8) -> u64`
  - Spawns a new lightweight thread to run `entry(arg)`. Returns a thread id (tid).
- `prim_rt_join(tid: u64) -> i32`
  - Blocks until the given thread finishes; returns its `i32` exit code.
- `prim_rt_yield()`
  - Hint to yield execution to another runnable thread.
- `prim_rt_sleep_ms(ms: u64)`
  - Sleep current thread for `ms` milliseconds.

Error Handling
- All functions are C-ABI. Functions documented as unsafe require callers to uphold their safety contracts.
- Future: introduce panic/reporting hooks (`prim_rt_panic(const char*, usize)`) once strings are specified.

Versioning
- Backward-compatible additions only. Breaking changes require bumping a runtime ABI version constant/symbol.

