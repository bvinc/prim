//! HIR ↔ wasm type helpers and the wasm function-type registry.

use prim_compiler::hir;
use std::collections::HashMap;
use wasm_encoder::{TypeSection, ValType};

/// Map an HIR type to the wasm `ValType` that carries it.
pub(crate) fn hir_type_to_valtype(ty: &hir::Type) -> ValType {
    match ty {
        hir::Type::Bool
        | hir::Type::U8
        | hir::Type::I8
        | hir::Type::U16
        | hir::Type::I16
        | hir::Type::U32
        | hir::Type::I32
        | hir::Type::IntVar => ValType::I32,
        hir::Type::U64 | hir::Type::I64 => ValType::I64,
        hir::Type::Usize | hir::Type::Isize => ValType::I32,
        hir::Type::F32 => ValType::F32,
        hir::Type::F64 | hir::Type::FloatVar => ValType::F64,
        hir::Type::Pointer { .. } | hir::Type::Struct(_) => ValType::I32,
        _ => ValType::I32,
    }
}

/// Whether the HIR integer type is signed (signed div/rem/cmp variants).
pub(crate) fn is_signed_int(ty: &hir::Type) -> bool {
    matches!(
        ty,
        hir::Type::I8
            | hir::Type::I16
            | hir::Type::I32
            | hir::Type::I64
            | hir::Type::Isize
            | hir::Type::IntVar
    )
}

/// Whether an HIR expression of this type leaves a value on the wasm stack.
/// `Undetermined` expressions (failed lowering, error placeholders) don't.
pub(crate) fn produces_value(ty: &hir::Type) -> bool {
    !matches!(ty, hir::Type::Undetermined)
}

/// Deduplicating registry of wasm function types `(params, results)`.
/// Two functions with the same signature share a type-section entry.
pub(crate) struct TypeRegistry {
    entries: Vec<(Vec<ValType>, Vec<ValType>)>,
    map: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
}

impl TypeRegistry {
    pub(crate) fn new() -> Self {
        Self {
            entries: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Register a function signature, returning its (possibly cached) type index.
    pub(crate) fn register(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&idx) = self.map.get(&key) {
            idx
        } else {
            let idx = self.entries.len() as u32;
            self.map.insert(key.clone(), idx);
            self.entries.push(key);
            idx
        }
    }

    pub(crate) fn build_section(&self) -> TypeSection {
        let mut types = TypeSection::new();
        for (params, results) in &self.entries {
            types.ty().function(params.clone(), results.clone());
        }
        types
    }
}
