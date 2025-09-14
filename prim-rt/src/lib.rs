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
