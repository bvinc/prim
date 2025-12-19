//! prim-rt: Minimal runtime for Prim.
//!
//! Exposes C-ABI allocation helpers for the compiler/runtime integration.
//! The rest of the runtime should live in Prim itself.

use std::alloc::{Layout, alloc, dealloc};
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::mem::ManuallyDrop;
use std::os::fd::FromRawFd;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};
use std::thread::{self, JoinHandle};
use std::time::Duration;

#[inline]
fn layout_from(size: usize, align: usize) -> Option<Layout> {
    Layout::from_size_align(size, align).ok()
}

/// Allocate `size` bytes with `align` alignment using Rust's global allocator.
///
/// # Safety
/// - `align` must be a power of two and >= 1.
/// - The returned pointer must be freed with [`prim_rt_free`] using the exact
///   same `size` and `align`.
/// - On allocation failure, returns null; callers must handle a null pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prim_rt_alloc(size: usize, align: usize) -> *mut u8 {
    match layout_from(size, align) {
        Some(layout) => {
            let ptr = unsafe { alloc(layout) };
            if ptr.is_null() { null_mut() } else { ptr }
        }
        None => null_mut(),
    }
}

/// Free memory previously returned by `prim_rt_alloc` with the same `size` and `align`.
///
/// # Safety
/// - `ptr` must have been allocated by [`prim_rt_alloc`].
/// - `size` and `align` must be identical to the allocation call.
/// - Passing an invalid pointer, wrong layout, or double-free is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prim_rt_free(ptr: *mut u8, size: usize, align: usize) {
    if ptr.is_null() {
        return;
    }
    if let Some(layout) = layout_from(size, align) {
        unsafe { dealloc(ptr, layout) };
    }
}

/// Print a signed 64-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_i64(x: i64) {
    println!("{x}");
}

/// Print an unsigned 8-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_u8(x: u8) {
    println!("{x}");
}

/// Print a signed 8-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_i8(x: i8) {
    println!("{x}");
}

/// Print an unsigned 16-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_u16(x: u16) {
    println!("{x}");
}

/// Print a signed 16-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_i16(x: i16) {
    println!("{x}");
}

/// Print an unsigned 32-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_u32(x: u32) {
    println!("{x}");
}

/// Print a signed 32-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_i32(x: i32) {
    println!("{x}");
}

/// Print an unsigned 64-bit integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_u64(x: u64) {
    println!("{x}");
}

/// Print an unsigned pointer-sized integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_usize(x: usize) {
    println!("{x}");
}

/// Print a signed pointer-sized integer followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_isize(x: isize) {
    println!("{x}");
}

/// Print a 32-bit float followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_f32(x: f32) {
    println!("{x}");
}

/// Print a 64-bit float followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_f64(x: f64) {
    println!("{x}");
}

/// Print a boolean (0 = false, non-zero = true) followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_println_bool(b: i8) {
    let v = b != 0;
    println!("{v}");
}

/// Terminate the process with the given exit code.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_exit(code: i32) -> ! {
    std::process::exit(code)
}

// --- Transitional threading API (OS-thread backed) ---

fn next_tid() -> u64 {
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

fn handles() -> &'static Mutex<HashMap<u64, JoinHandle<i32>>> {
    static HANDLES: OnceLock<Mutex<HashMap<u64, JoinHandle<i32>>>> = OnceLock::new();
    HANDLES.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Spawn a new thread to run `entry(arg)`; returns a thread id.
///
/// # Safety
/// - `entry` must be a valid function pointer following the C ABI and may be unsafe to call.
/// - `arg` must remain valid for the duration of the thread's execution.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prim_rt_spawn(
    entry: unsafe extern "C" fn(*mut u8) -> i32,
    arg: *mut u8,
) -> u64 {
    let tid = next_tid();
    let arg_val = arg as usize;
    let handle = thread::spawn(move || unsafe { entry(arg_val as *mut u8) });
    handles().lock().unwrap().insert(tid, handle);
    tid
}

/// Join a previously spawned thread by id. Returns its exit code.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_join(tid: u64) -> i32 {
    if let Some(handle) = handles().lock().unwrap().remove(&tid) {
        handle.join().unwrap_or(101)
    } else {
        // Unknown thread id
        102
    }
}

/// Yield execution.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_yield() {
    thread::yield_now();
}

/// Sleep for `ms` milliseconds.
#[unsafe(no_mangle)]
pub extern "C" fn prim_rt_sleep_ms(ms: u64) {
    thread::sleep(Duration::from_millis(ms));
}

// --- IO ---

/// Write up to `len` bytes from `buf` to the file descriptor `fd`.
/// Returns the number of bytes written, or -1 on error.
///
/// # Safety
/// - `buf` must be valid for reads of `len` bytes.
/// - `fd` must be a valid, open file descriptor. This function does not take ownership of `fd`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prim_rt_write(fd: i32, buf: *const u8, len: usize) -> isize {
    if len == 0 {
        return 0;
    }
    if buf.is_null() {
        return -1;
    }

    let slice = unsafe { std::slice::from_raw_parts(buf, len) };
    // Construct a File without taking ownership of the fd (avoid closing on drop)
    let file = unsafe { File::from_raw_fd(fd) };
    let mut file = ManuallyDrop::new(file);
    match (*file).write(slice) {
        Ok(n) => n as isize,
        Err(_) => -1,
    }
}

/// Read up to `len` bytes from the file descriptor `fd` into `buf`.
/// Returns the number of bytes read, 0 on EOF, or -1 on error.
///
/// # Safety
/// - `buf` must be valid for writes of `len` bytes.
/// - `fd` must be a valid, open file descriptor. This function does not take ownership of `fd`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prim_rt_read(fd: i32, buf: *mut u8, len: usize) -> isize {
    if len == 0 {
        return 0;
    }
    if buf.is_null() {
        return -1;
    }

    let slice = unsafe { std::slice::from_raw_parts_mut(buf, len) };
    let file = unsafe { File::from_raw_fd(fd) };
    let mut file = ManuallyDrop::new(file);
    match (*file).read(slice) {
        Ok(n) => n as isize,
        Err(_) => -1,
    }
}

// --- std.mem intrinsics (symbols matched by codegen via qualified calls) ---

/// Copy `n` bytes from `src` to `dst` without overlap handling.
///
/// # Safety
/// - `dst` and `src` must be valid for reads/writes of `n` bytes.
/// - The regions must not overlap; use `std__mem__move` for overlapping copies.
/// - Pointers must be properly aligned for the copy width.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std__mem__copy(dst: *mut u8, src: *const u8, n: usize) -> usize {
    unsafe { std::ptr::copy_nonoverlapping(src, dst, n) };
    n
}

/// Move `n` bytes from `src` to `dst`, allowing overlap.
///
/// # Safety
/// - `dst` and `src` must be valid for reads/writes of `n` bytes.
/// - Pointers must be properly aligned for the copy width.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std__mem__move(dst: *mut u8, src: *const u8, n: usize) -> usize {
    unsafe { std::ptr::copy(src, dst, n) };
    n
}

/// Set `n` bytes at `dst` to `val`.
///
/// # Safety
/// - `dst` must be valid for writes of `n` bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std__mem__set(dst: *mut u8, val: u8, n: usize) -> usize {
    unsafe { std::ptr::write_bytes(dst, val, n) };
    n
}

/// Return the length of a null-terminated byte string starting at `ptr`.
///
/// # Safety
/// - `ptr` must be a valid pointer to a null-terminated sequence of bytes.
/// - Behavior is undefined if no terminator is found within accessible memory.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std__mem__len(ptr: *const u8) -> usize {
    let mut len = 0usize;
    unsafe {
        let mut p = ptr;
        while *p != 0 {
            len += 1;
            p = p.add(1);
        }
    }
    len
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_and_free_basic() {
        unsafe {
            let size = 64usize;
            let align = 8usize;
            let ptr = prim_rt_alloc(size, align);
            assert!(!ptr.is_null());
            // Touch memory to ensure it's writable
            *ptr = 42;
            prim_rt_free(ptr, size, align);
        }
    }
}

// C runtime entrypoint that calls into Prim's generated entry (`prim_main`).
// Always present for production builds; hidden during Rust unit tests to avoid
// multiple `main` definitions in the test harness.
#[cfg(not(test))]
#[unsafe(no_mangle)]
pub extern "C" fn main(_argc: i32, _argv: *mut *mut u8) -> i32 {
    unsafe {
        unsafe extern "C" {
            fn prim_main() -> i32;
        }
        // Safety: we trust codegen to export a correct `prim_main` symbol.
        prim_main()
    }
}
