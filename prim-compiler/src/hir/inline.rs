//! Storage-representation policy: every concrete aggregate type is an *inline*
//! value — a struct/tuple/enum/array is its bytes, never a pointer to a
//! separate per-field allocation. A top-level value is materialized as one
//! heap box (its "place"); nested aggregates live inside that box at their
//! field offset, not behind a 4-byte sub-box pointer.
//!
//! This module is the single source of truth for layout size and alignment,
//! shared by the `size_of` intrinsic fold, pointer arithmetic (`add`/`sub`),
//! container stride (`Vec`/`Array`), field layout (`prim-wasm/src/layout.rs`),
//! and codegen's inline-vs-scalar distinction (`emit.rs`). The only types that
//! are *not* inline are self-referential ones (no finite size) — those fall
//! back to a box pointer.

use super::{MatchArm, Program, Type};
use crate::hir::cfg::is_copy;

/// The concrete length of an array length-type. After monomorphization every
/// array length is a `ConstInt`; anything else is a compiler bug.
fn array_const_len(len: &Type) -> usize {
    match len {
        Type::ConstInt(v) => *v as usize,
        other => panic!("array length should be a ConstInt after monomorphization, got {other}"),
    }
}

fn align_up(offset: u32, align: u32) -> u32 {
    debug_assert!(
        align.is_power_of_two(),
        "alignment {align} not a power of 2"
    );
    (offset + align - 1) & !(align - 1)
}

/// Identity of an aggregate currently being sized, to detect self-reference.
#[derive(Clone, PartialEq)]
enum AggKey {
    Struct(u32),
    Enum(u32),
}

/// Storage-representation policy for concrete (post-mono) types. Layout is a
/// pure function of the type graph: a `Drop` type is still an inline value
/// (its destructor runs in place; the drop site reclaims the box).
pub struct InlinePolicy<'a> {
    program: &'a Program,
}

impl<'a> InlinePolicy<'a> {
    pub fn new(program: &'a Program) -> Self {
        InlinePolicy { program }
    }

    /// The underlying program (type definitions, `Copy` registry, etc.).
    pub fn program(&self) -> &Program {
        self.program
    }

    /// Whether a `match` over `arms` consumes (moves) its scrutinee.
    pub fn match_consumes(&self, arms: &[MatchArm]) -> bool {
        crate::hir::cfg::match_consumes(&self.program.copy_types, arms)
    }

    /// Whether a value of `ty` is `Copy` (a scalar/raw pointer, or an
    /// explicit-`Copy` struct). Shared so codegen can decide copy-vs-move /
    /// clone-vs-borrow without threading the registry separately.
    pub fn is_copy(&self, ty: &Type) -> bool {
        is_copy(&self.program.copy_types, ty)
    }

    /// Whether `ty` is an *aggregate* carried as a place with an inline byte
    /// layout (struct/tuple/enum/array with a finite, non-recursive size).
    /// Scalars and raw pointers are wasm values (not places), so this is false
    /// for them; a self-referential value (no finite size) falls back to a
    /// pointer and is false.
    pub fn is_inline(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Struct(..) | Type::Tuple(..) | Type::Enum(..) | Type::Array(..)
        ) && self.inline_size_opt(ty, &mut Vec::new()).is_some()
    }

    /// Inline byte size of an aggregate (== its container-less layout size).
    /// Only meaningful when `is_inline` is true; other types fall back to
    /// `size_bytes` defensively (callers gate on `is_inline` first).
    pub fn inline_size(&self, ty: &Type) -> u32 {
        self.inline_size_opt(ty, &mut Vec::new())
            .unwrap_or_else(|| ty.size_bytes())
    }

    /// The number of bytes a value of `ty` occupies in a container slot: an
    /// aggregate's inline byte layout, or a scalar's natural width. This is the
    /// storage stride `size_of`, `add`/`sub` element scaling, and `Deref`/
    /// `DerefAssign` use — one inline layout for every type.
    pub fn stored_size(&self, ty: &Type) -> u32 {
        match ty {
            Type::Struct(..) | Type::Tuple(..) | Type::Enum(..) | Type::Array(..) => {
                self.inline_size(ty)
            }
            _ => ty.size_bytes(),
        }
    }

    /// Natural alignment of `ty`: the max alignment of its scalar leaves.
    /// Aggregates align to their most-aligned field (enums to 8 — payload
    /// always follows the 8-byte discriminant header); a self-referential
    /// aggregate contributes a pointer (4) instead of recursing forever.
    pub fn align_of(&self, ty: &Type) -> u32 {
        self.align_of_guarded(ty, &mut Vec::new())
    }

    fn align_of_guarded(&self, ty: &Type, visiting: &mut Vec<AggKey>) -> u32 {
        match ty {
            Type::U8 | Type::I8 | Type::Bool => 1,
            Type::U16 | Type::I16 => 2,
            Type::U32 | Type::I32 | Type::F32 | Type::Usize | Type::Isize => 4,
            Type::U64 | Type::I64 | Type::F64 => 8,
            Type::Pointer { .. } | Type::Trait(_) => 4,
            Type::Struct(sid, _) => {
                let key = AggKey::Struct(sid.0);
                if visiting.contains(&key) {
                    return 4;
                }
                visiting.push(key);
                let a = self
                    .program
                    .structs
                    .get(sid.0 as usize)
                    .map(|s| {
                        s.fields
                            .iter()
                            .map(|f| self.align_of_guarded(&f.ty, visiting))
                            .max()
                            .unwrap_or(1)
                    })
                    .unwrap_or(4);
                visiting.pop();
                a
            }
            Type::Tuple(elems) => elems
                .iter()
                .map(|t| self.align_of_guarded(t, visiting))
                .max()
                .unwrap_or(1),
            Type::Enum(..) => 8,
            Type::Array(elem, _) => self.align_of_guarded(elem, visiting),
            // Post-mono types only; `Param`/`IntVar`/`FloatVar`/`Undetermined`
            // are a fallback pointer width.
            _ => 4,
        }
    }

    /// Inline size, or `None` if the type can't be inlined (recursive — no
    /// finite size). `visiting` guards against self-reference; a field whose
    /// type is the enclosing type falls back to a pointer.
    fn inline_size_opt(&self, ty: &Type, visiting: &mut Vec<AggKey>) -> Option<u32> {
        match ty {
            Type::Struct(sid, _) => {
                let key = AggKey::Struct(sid.0);
                if visiting.contains(&key) {
                    return None;
                }
                visiting.push(key);
                let s = self.program.structs.get(sid.0 as usize)?;
                let mut offset = 0u32;
                for field in &s.fields {
                    let size = self.field_size(&field.ty, visiting)?;
                    offset = align_up(offset, self.align_of_guarded(&field.ty, visiting));
                    offset += size;
                }
                visiting.pop();
                Some(align_up(
                    offset.max(1),
                    self.align_of_guarded(ty, &mut Vec::new()),
                ))
            }
            Type::Tuple(elems) => {
                let mut offset = 0u32;
                for ety in elems {
                    let size = self.field_size(ety, visiting)?;
                    offset = align_up(offset, self.align_of_guarded(ety, visiting));
                    offset += size;
                }
                Some(align_up(
                    offset.max(1),
                    self.align_of_guarded(ty, &mut Vec::new()),
                ))
            }
            Type::Enum(eid, _) => {
                let key = AggKey::Enum(eid.0);
                if visiting.contains(&key) {
                    return None;
                }
                visiting.push(key);
                let e = self.program.enums.get(eid.0 as usize)?;
                let mut max_payload = 0u32;
                for variant in &e.variants {
                    let mut offset = 0u32;
                    for field in &variant.fields {
                        let size = self.field_size(&field.ty, visiting)?;
                        offset = align_up(offset, self.align_of_guarded(&field.ty, visiting));
                        offset += size;
                    }
                    max_payload = max_payload.max(align_up(offset, 8));
                }
                visiting.pop();
                // u32 discriminant (8-aligned) followed by the max-variant payload.
                Some(8 + max_payload)
            }
            Type::Array(elem, len) => {
                let n = array_const_len(len);
                let esize = self.field_size(elem, visiting)?;
                Some(align_up(
                    (n as u32) * esize,
                    self.align_of_guarded(elem, visiting),
                ))
            }
            _ => None,
        }
    }

    /// Size of a *field* of the enclosing aggregate: an inline aggregate's true
    /// size (recursing), or a scalar/pointer's natural width. Returns `None`
    /// only for a self-referential aggregate.
    fn field_size(&self, ty: &Type, visiting: &mut Vec<AggKey>) -> Option<u32> {
        match ty {
            Type::Struct(..) | Type::Tuple(..) | Type::Enum(..) | Type::Array(..) => {
                self.inline_size_opt(ty, visiting)
            }
            _ => Some(ty.size_bytes()),
        }
    }
}

/// Free-function convenience.
pub fn is_inline(program: &Program, ty: &Type) -> bool {
    InlinePolicy::new(program).is_inline(ty)
}

/// See [`InlinePolicy::stored_size`].
pub fn stored_size(program: &Program, ty: &Type) -> u32 {
    InlinePolicy::new(program).stored_size(ty)
}
