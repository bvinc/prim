//! prim-rt: Minimal runtime for Prim.
//!
//! Exposes C-ABI allocation helpers for the compiler/runtime integration.
//! The rest of the runtime should live in Prim itself.

use std::alloc::{Layout, alloc, dealloc};
use std::ptr::null_mut;

#[inline]
fn layout_from(size: usize, align: usize) -> Option<Layout> {
    Layout::from_size_align(size, align).ok()
}

/// Allocate `size` bytes with `align` alignment using Rust's global allocator.
///
/// Safety: Caller must ensure `align` is a power of two and >= 1, and must
/// free with `prim_rt_free` using the exact same `size` and `align`.
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
/// Safety: `ptr` must be a pointer previously returned by `prim_rt_alloc` with
/// the exact `size` and `align`. Double free or mismatched values are UB.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prim_rt_free(ptr: *mut u8, size: usize, align: usize) {
    if ptr.is_null() {
        return;
    }
    if let Some(layout) = layout_from(size, align) {
        unsafe { dealloc(ptr, layout) };
    }
}

// std.mem intrinsics (symbols matched by codegen via qualified calls)

#[unsafe(no_mangle)]
pub extern "C" fn std__mem__copy(dst: *mut u8, src: *const u8, n: usize) -> usize {
    unsafe { std::ptr::copy_nonoverlapping(src, dst, n) };
    n
}

#[unsafe(no_mangle)]
pub extern "C" fn std__mem__move(dst: *mut u8, src: *const u8, n: usize) -> usize {
    unsafe { std::ptr::copy(src, dst, n) };
    n
}

#[unsafe(no_mangle)]
pub extern "C" fn std__mem__set(dst: *mut u8, val: u8, n: usize) -> usize {
    unsafe { std::ptr::write_bytes(dst, val, n) };
    n
}

#[unsafe(no_mangle)]
pub extern "C" fn std__mem__len(ptr: *const u8) -> usize {
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
