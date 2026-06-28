//! Shared analysis for Drop/RAII: which types need dropping, and which type
//! implements the `Drop` trait (and with what method).
//!
//! Used by the drop-elaboration pass (to decide which locals to drop) and by
//! codegen (to emit a type's drop glue). Operates on concrete, post-mono types
//! — a type needs dropping iff it implements `Drop` or (transitively) contains
//! a field that does.

use super::{FuncId, MethodOwner, Program, Type};
use std::cell::RefCell;
use std::collections::HashMap;

/// Resolves Drop information against a fixed `Program`. Caches `needs_drop`
/// results, since the type graph is queried repeatedly.
pub struct DropInfo<'a> {
    program: &'a Program,
    /// The `Drop` trait's id, found by name. `None` if no `Drop` trait exists
    /// (then nothing needs dropping).
    drop_trait: Option<super::TraitId>,
    cache: RefCell<HashMap<Type, bool>>,
}

impl<'a> DropInfo<'a> {
    pub fn new(program: &'a Program) -> Self {
        let drop_trait = program
            .traits
            .iter()
            .find(|t| {
                program
                    .symbols
                    .get(t.name.0 as usize)
                    .map(|s| program.interner.resolve(&s.name) == "Drop")
                    .unwrap_or(false)
            })
            .map(|t| t.id);
        DropInfo {
            program,
            drop_trait,
            cache: RefCell::new(HashMap::new()),
        }
    }

    /// The `FuncId` of `ty`'s `Drop::drop` method, if it implements `Drop`.
    pub fn drop_method(&self, ty: &Type) -> Option<FuncId> {
        let trait_id = self.drop_trait?;
        let owner = MethodOwner::of_type(ty)?;
        let fids = self.program.impls.get(&(trait_id, owner))?;
        let fid = *fids.first()?;
        // `FuncId(u32::MAX)` is the missing-impl sentinel.
        if fid.0 == u32::MAX { None } else { Some(fid) }
    }

    /// Whether a value of `ty` needs dropping: it implements `Drop`, or it is an
    /// aggregate that (transitively) contains a field that does.
    pub fn needs_drop(&self, ty: &Type) -> bool {
        if let Some(hit) = self.cache.borrow().get(ty) {
            return *hit;
        }
        // Insert `false` first so a (pointer-free) recursive type can't loop;
        // value-field recursion is acyclic, but this is belt-and-suspenders.
        self.cache.borrow_mut().insert(ty.clone(), false);
        let result = self.compute_needs_drop(ty);
        self.cache.borrow_mut().insert(ty.clone(), result);
        result
    }

    fn compute_needs_drop(&self, ty: &Type) -> bool {
        if self.drop_method(ty).is_some() {
            return true;
        }
        match ty {
            Type::Struct(sid, _) => self
                .program
                .structs
                .get(sid.0 as usize)
                .map(|s| s.fields.iter().any(|f| self.needs_drop(&f.ty)))
                .unwrap_or(false),
            Type::Enum(eid, _) => self
                .program
                .enums
                .get(eid.0 as usize)
                .map(|e| {
                    e.variants
                        .iter()
                        .any(|v| v.fields.iter().any(|f| self.needs_drop(&f.ty)))
                })
                .unwrap_or(false),
            Type::Tuple(elems) => elems.iter().any(|t| self.needs_drop(t)),
            Type::Array(elem, _) => self.needs_drop(elem),
            _ => false,
        }
    }
}
