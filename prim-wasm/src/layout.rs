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
// [8..12)     nwritten
// [12..13)    '\n'
// [13..34)    digit scratch (21 bytes, enough for i64::MIN)
// [34..38)    "true"
// [38..43)    "false"
// [43..44)    '.'
// [48..128)   float fractional digit scratch
// [128..)     @dbg prefixes + string literal bytes (dynamic)
// [HEAP..)    bump heap

pub(crate) const NEWLINE_OFFSET: i32 = 12;
pub(crate) const DIGIT_BUF_END: i32 = 34;
pub(crate) const TRUE_OFFSET: i32 = 34;
pub(crate) const FALSE_OFFSET: i32 = 38;
pub(crate) const DOT_OFFSET: i32 = 43;
pub(crate) const FLOAT_SCRATCH: i32 = 48;
pub(crate) const STATIC_DATA_START: u32 = 128;
pub(crate) const HEAP_PTR_GLOBAL: u32 = 0;

/// MemArg for byte-wide loads/stores (alignment hint = 1).
pub(crate) const MEM8: MemArg = MemArg {
    offset: 0,
    align: 0,
    memory_index: 0,
};

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

/// Byte size of a field of the given type in linear memory.
fn field_size(ty: &hir::Type) -> u32 {
    match ty {
        hir::Type::Bool | hir::Type::I8 | hir::Type::U8 => 1,
        hir::Type::I16 | hir::Type::U16 => 2,
        hir::Type::I64 | hir::Type::U64 | hir::Type::F64 | hir::Type::FloatVar => 8,
        _ => 4,
    }
}

fn align_up(offset: u32, align: u32) -> u32 {
    (offset + align - 1) & !(align - 1)
}

/// Walk a struct's declared fields, assign natural-aligned offsets, and
/// return the total size rounded up to 8 bytes.
pub(crate) fn compute_struct_layout(s: &hir::Struct) -> StructLayout {
    let mut offset = 0u32;
    let mut fields = HashMap::new();
    for f in &s.fields {
        let size = field_size(&f.ty);
        offset = align_up(offset, size);
        fields.insert(f.name, (offset, f.ty.clone()));
        offset += size;
    }
    let size = align_up(offset.max(1), 8);
    StructLayout { size, fields }
}

pub(crate) fn compute_enum_layout(e: &hir::Enum) -> EnumLayout {
    let mut variants = Vec::with_capacity(e.variants.len());
    let mut max_payload = 0u32;

    for variant in &e.variants {
        let mut offset = 0u32;
        let mut fields = HashMap::with_capacity(variant.fields.len());
        for field in &variant.fields {
            let size = field_size(&field.ty);
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
    let align = match field_size(ty) {
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
