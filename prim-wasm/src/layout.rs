//! Static memory layout (named offsets) and struct layout computation.
//!
//! The compiled wasm module reserves a fixed prologue of linear memory for
//! scratch buffers + canned strings used by the println builtins. After that
//! comes (optionally) prefix bytes for `@dbg` sites and bytes for string
//! literals; the bump heap starts beyond all of it (see `lib.rs`).

use prim_compiler::hir;
use std::collections::HashMap;
use wasm_encoder::{Function, Instruction, MemArg};

// === Static memory layout ===
//
// [0..4)      iovec.buf
// [4..8)      iovec.buf_len
// [8..12)     nwritten / fd_read result count
// [12..128)   free (was the hand-written println digit/string scratch)
// [128..136)  clock_time_get output (u64 nanos)
// [136..140)  poll_oneoff nevents output (u32); subscription/event buffers are
//             heap-allocated by the scheduler
// [144..)     string literal bytes (dynamic)
// [HEAP..)    bump heap

pub(crate) const CLOCK_SCRATCH: i32 = 128;
pub(crate) const POLL_NEVENTS: i32 = 136;
pub(crate) const STATIC_DATA_START: u32 = 144;

/// MemArg for 4-byte loads/stores at offset 0 (alignment hint = 4).
pub(crate) const MEM32: MemArg = MemArg {
    offset: 0,
    align: 2,
    memory_index: 0,
};

// === Struct layout ===

/// Computed memory layout for a single struct.
#[derive(Clone)]
pub(crate) struct StructLayout {
    pub size: u32,
    pub fields: HashMap<hir::InternSymbol, (u32, hir::Type)>,
}

/// Computed memory layout for one enum variant's payload.
#[derive(Clone)]
pub(crate) struct VariantLayout {
    pub fields: HashMap<hir::InternSymbol, (u32, hir::Type)>,
}

/// Computed memory layout for one enum. Values are pointers to:
///   offset 0: u32 discriminant
///   offset 8: variant payload, sized for the largest variant
#[derive(Clone)]
pub(crate) struct EnumLayout {
    pub size: u32,
    pub variants: Vec<VariantLayout>,
}

fn align_up(offset: u32, align: u32) -> u32 {
    (offset + align - 1) & !(align - 1)
}

/// Largest aggregate (bytes) stored inline in its container rather than as a
/// pointer to its own heap box.
const MAX_INLINE_BYTES: u32 = 16;

/// Decides which aggregate types are stored *inline* in their container (their
/// bytes laid out at the field offset) versus *boxed* (a pointer to a separate
/// allocation, the historical default). Inlining a field collapses a nested box
/// into the parent. Only small, non-`Drop`, non-recursive struct/tuple types
/// qualify: a `needs_drop` value must own a box (its `drop_T` frees that box, so
/// it cannot live inside another), and recursive types have no finite inline
/// size. The single source of truth consulted by both layout and codegen.
pub(crate) struct InlinePolicy<'a> {
    program: &'a hir::Program,
    drop_info: &'a hir::DropInfo<'a>,
}

impl<'a> InlinePolicy<'a> {
    pub(crate) fn new(program: &'a hir::Program, drop_info: &'a hir::DropInfo<'a>) -> Self {
        InlinePolicy { program, drop_info }
    }

    /// Whether a value of this type is stored inline in its container.
    pub(crate) fn is_inline(&self, ty: &hir::Type) -> bool {
        matches!(ty, hir::Type::Struct(..) | hir::Type::Tuple(..))
            && !self.drop_info.needs_drop(ty)
            && self
                .inline_size_opt(ty, &mut Vec::new())
                .is_some_and(|n| n <= MAX_INLINE_BYTES)
    }

    /// Inline byte size of an inline-able type (== its container-less layout
    /// size). Only valid for types where `is_inline` is true.
    pub(crate) fn inline_size(&self, ty: &hir::Type) -> u32 {
        self.inline_size_opt(ty, &mut Vec::new())
            .unwrap_or_else(|| ty.size_bytes())
    }

    /// The bytes a field of this type occupies in its container: an inline
    /// aggregate's full size, else the natural width (4 for a boxed-aggregate
    /// pointer, the scalar width otherwise).
    pub(crate) fn field_size(&self, ty: &hir::Type) -> u32 {
        if self.is_inline(ty) {
            self.inline_size(ty)
        } else {
            ty.size_bytes()
        }
    }

    /// Inline size, or `None` if the type can't be inlined (recursive — no
    /// finite size). `visiting` guards against self-reference.
    fn inline_size_opt(&self, ty: &hir::Type, visiting: &mut Vec<hir::StructId>) -> Option<u32> {
        match ty {
            hir::Type::Struct(sid, _) => {
                if visiting.contains(sid) {
                    return None;
                }
                visiting.push(*sid);
                let s = self.program.structs.get(sid.0 as usize)?;
                let mut offset = 0u32;
                for field in &s.fields {
                    let size = self.field_size_guarded(&field.ty, visiting);
                    offset = align_up(offset, size);
                    offset += size;
                }
                visiting.pop();
                Some(align_up(offset.max(1), 8))
            }
            hir::Type::Tuple(elems) => {
                let mut offset = 0u32;
                for ety in elems {
                    let size = self.field_size_guarded(ety, visiting);
                    offset = align_up(offset, size);
                    offset += size;
                }
                Some(align_up(offset.max(1), 8))
            }
            _ => None,
        }
    }

    /// `field_size`, but threading the recursion guard (so a field that is the
    /// enclosing type falls back to a pointer instead of recursing forever).
    fn field_size_guarded(&self, ty: &hir::Type, visiting: &mut Vec<hir::StructId>) -> u32 {
        if matches!(ty, hir::Type::Struct(..) | hir::Type::Tuple(..))
            && !self.drop_info.needs_drop(ty)
        {
            if let Some(n) = self.inline_size_opt(ty, visiting) {
                if n <= MAX_INLINE_BYTES {
                    return n;
                }
            }
        }
        ty.size_bytes()
    }
}

/// Walk a struct's declared fields, assign natural-aligned offsets, and
/// return the total size rounded up to 8 bytes.
pub(crate) fn compute_struct_layout(s: &hir::Struct, policy: &InlinePolicy) -> StructLayout {
    let mut offset = 0u32;
    let mut fields = HashMap::new();
    for f in &s.fields {
        let size = policy.field_size(&f.ty);
        offset = align_up(offset, size);
        fields.insert(f.name, (offset, f.ty.clone()));
        offset += size;
    }
    let size = align_up(offset.max(1), 8);
    StructLayout { size, fields }
}

/// Layout for a tuple's elements: total heap size and each element's
/// `(byte offset, type)`, indexed positionally. Same natural-alignment rules
/// as structs, computed on demand since tuples have no `StructId`.
pub(crate) struct TupleLayout {
    pub size: u32,
    pub elems: Vec<(u32, hir::Type)>,
}

pub(crate) fn compute_tuple_layout(elems: &[hir::Type], policy: &InlinePolicy) -> TupleLayout {
    let mut offset = 0u32;
    let mut out = Vec::with_capacity(elems.len());
    for ty in elems {
        let size = policy.field_size(ty);
        offset = align_up(offset, size);
        out.push((offset, ty.clone()));
        offset += size;
    }
    let size = align_up(offset.max(1), 8);
    TupleLayout { size, elems: out }
}

pub(crate) fn compute_enum_layout(e: &hir::Enum, policy: &InlinePolicy) -> EnumLayout {
    let mut variants = Vec::with_capacity(e.variants.len());
    let mut max_payload = 0u32;

    for variant in &e.variants {
        let mut offset = 0u32;
        let mut fields = HashMap::with_capacity(variant.fields.len());
        for field in &variant.fields {
            let size = policy.field_size(&field.ty);
            offset = align_up(offset, size);
            fields.insert(field.name, (offset, field.ty.clone()));
            offset += size;
        }
        max_payload = max_payload.max(align_up(offset, 8));
        variants.push(VariantLayout { fields });
    }

    EnumLayout {
        size: 8 + max_payload,
        variants,
    }
}

/// Build a `MemArg` for a load/store at the given field offset, with the
/// natural alignment hint for the field's size.
pub(crate) fn mem_arg(offset: u32, ty: &hir::Type) -> MemArg {
    let align = match ty.size_bytes() {
        1 => 0,
        2 => 1,
        4 => 2,
        _ => 3,
    };
    MemArg {
        offset: offset as u64,
        align,
        memory_index: 0,
    }
}

/// Emit a store instruction for a value of the given type at `base + offset`.
/// Assumes `base` pointer and value-to-store are already on the wasm stack.
pub(crate) fn emit_field_store(f: &mut Function, ty: &hir::Type, offset: u32) {
    let arg = mem_arg(offset, ty);
    match ty {
        hir::Type::Bool | hir::Type::I8 | hir::Type::U8 => {
            f.instruction(&Instruction::I32Store8(arg));
        }
        hir::Type::I16 | hir::Type::U16 => {
            f.instruction(&Instruction::I32Store16(arg));
        }
        hir::Type::I64 | hir::Type::U64 => {
            f.instruction(&Instruction::I64Store(arg));
        }
        hir::Type::F32 => {
            f.instruction(&Instruction::F32Store(arg));
        }
        hir::Type::F64 => {
            f.instruction(&Instruction::F64Store(arg));
        }
        _ => {
            f.instruction(&Instruction::I32Store(arg));
        }
    };
}

/// Emit a load instruction for a value of the given type at `base + offset`.
/// Assumes `base` pointer is already on the wasm stack.
pub(crate) fn emit_field_load(f: &mut Function, ty: &hir::Type, offset: u32) {
    let arg = mem_arg(offset, ty);
    match ty {
        hir::Type::Bool | hir::Type::U8 => {
            f.instruction(&Instruction::I32Load8U(arg));
        }
        hir::Type::I8 => {
            f.instruction(&Instruction::I32Load8S(arg));
        }
        hir::Type::U16 => {
            f.instruction(&Instruction::I32Load16U(arg));
        }
        hir::Type::I16 => {
            f.instruction(&Instruction::I32Load16S(arg));
        }
        hir::Type::I64 | hir::Type::U64 => {
            f.instruction(&Instruction::I64Load(arg));
        }
        hir::Type::F32 => {
            f.instruction(&Instruction::F32Load(arg));
        }
        hir::Type::F64 => {
            f.instruction(&Instruction::F64Load(arg));
        }
        _ => {
            f.instruction(&Instruction::I32Load(arg));
        }
    };
}
