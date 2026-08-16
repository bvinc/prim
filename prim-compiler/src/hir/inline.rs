//! Storage-representation policy: which aggregates are stored *inline* in
//! their container (a struct/tuple slot, an array element) vs. *boxed* (a
//! heap pointer in the slot).
//!
//! This is the compiler-side mirror of `prim-wasm/src/layout.rs`
//! (`InlinePolicy`). The wasm side is the source of truth for layout; this
//! copy exists so the semantic check in `mono` — a deref-read of a *boxed*
//! aggregate copies the box pointer itself, so the result would alias the
//! slot and double-free on drop — can run before codegen. Keep the two in
//! sync; boundary tests pin the rules so a drift is caught by the suite:
//! `vec_get_inline_struct` (16B, allowed) vs `vec_get_boxed_struct` (20B,
//! rejected) pin the byte cutoff, `vec_get_noncopy` / `array_get_noncopy`
//! pin the no-`Drop` rule.

use super::{DropInfo, Program, StructId, Type};

/// Largest aggregate (bytes) stored inline in a container rather than as a
/// box pointer. Mirrors `prim-wasm/src/layout.rs` `MAX_INLINE_BYTES`.
pub const MAX_INLINE_BYTES: u32 = 16;

/// Whether a value of `ty` is stored *inline* in its container: a struct or
/// tuple with no transitive `Drop` (a needs-drop value owns a box, so it
/// can't live inside another) whose layout fits `MAX_INLINE_BYTES` and is
/// non-recursive. Scalars and pointers are not aggregates (always copyable);
/// enums and arrays are always boxed.
pub fn is_inline(program: &Program, drop_info: &DropInfo, ty: &Type) -> bool {
    matches!(ty, Type::Struct(..) | Type::Tuple(..))
        && !drop_info.needs_drop(ty)
        && inline_size_opt(program, drop_info, ty, &mut Vec::new())
            .is_some_and(|n| n <= MAX_INLINE_BYTES)
}

/// Inline byte size of an inline-able aggregate (== its container-less
/// layout size), or `None` when the type can't be inlined (recursive — no
/// finite size). `visiting` guards against self-reference. Mirrors
/// `layout.rs::InlinePolicy::inline_size_opt`.
fn inline_size_opt(
    program: &Program,
    drop_info: &DropInfo,
    ty: &Type,
    visiting: &mut Vec<StructId>,
) -> Option<u32> {
    match ty {
        Type::Struct(sid, _) => {
            if visiting.contains(sid) {
                return None;
            }
            visiting.push(*sid);
            let s = program.structs.get(sid.0 as usize)?;
            let mut offset = 0u32;
            for field in &s.fields {
                let size = field_size_guarded(program, drop_info, &field.ty, visiting);
                offset = align_up(offset, size);
                offset += size;
            }
            visiting.pop();
            Some(align_up(offset.max(1), 8))
        }
        Type::Tuple(elems) => {
            let mut offset = 0u32;
            for ety in elems {
                let size = field_size_guarded(program, drop_info, ety, visiting);
                offset = align_up(offset, size);
                offset += size;
            }
            Some(align_up(offset.max(1), 8))
        }
        _ => None,
    }
}

/// `field_size`, threading the recursion guard: a field that is un-inline-able
/// (the enclosing type, a needs-drop type, or one over the cutoff) falls back
/// to its stored width — 4 for a boxed-aggregate pointer, the scalar's
/// natural width otherwise.
fn field_size_guarded(
    program: &Program,
    drop_info: &DropInfo,
    ty: &Type,
    visiting: &mut Vec<StructId>,
) -> u32 {
    if matches!(ty, Type::Struct(..) | Type::Tuple(..))
        && !drop_info.needs_drop(ty)
        && let Some(n) = inline_size_opt(program, drop_info, ty, visiting)
        && n <= MAX_INLINE_BYTES
    {
        n
    } else {
        ty.size_bytes()
    }
}

fn align_up(offset: u32, align: u32) -> u32 {
    (offset + align - 1) & !(align - 1)
}
