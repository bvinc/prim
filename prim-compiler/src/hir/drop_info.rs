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
        let mut visiting: Vec<Type> = Vec::new();
        let result = self.compute_needs_drop(ty, &mut visiting);
        // Cache only the *top-level* result. A recursive edge is visited as
        // `false` (it does not independently require a drop — the enclosing
        // value's drop already covers it), so caching an intermediate result
        // computed under that cycle guard would poison later queries with a
        // wrong answer for mutually-recursive types (e.g. `Node` ↔
        // `Option[Node]`).
        self.cache.borrow_mut().insert(ty.clone(), result);
        result
    }

    fn compute_needs_drop(&self, ty: &Type, visiting: &mut Vec<Type>) -> bool {
        if let Some(hit) = self.cache.borrow().get(ty) {
            return *hit;
        }
        if self.drop_method(ty).is_some() {
            return true;
        }
        if visiting.contains(ty) {
            // Recursive edge back to a type already being computed: it does not
            // independently require a drop.
            return false;
        }
        visiting.push(ty.clone());
        let result = match ty {
            Type::Struct(sid, _) => self
                .program
                .structs
                .get(sid.0 as usize)
                .map(|s| {
                    s.fields
                        .iter()
                        .any(|f| self.compute_needs_drop(&f.ty, visiting))
                })
                .unwrap_or(false),
            Type::Enum(eid, _) => self
                .program
                .enums
                .get(eid.0 as usize)
                .map(|e| {
                    e.variants.iter().any(|v| {
                        v.fields
                            .iter()
                            .any(|f| self.compute_needs_drop(&f.ty, visiting))
                    })
                })
                .unwrap_or(false),
            Type::Tuple(elems) => elems.iter().any(|t| self.compute_needs_drop(t, visiting)),
            Type::Array(elem, _) => self.compute_needs_drop(elem, visiting),
            // A trait object owns its boxed value: it needs a destructor that
            // dispatches through the vtable to the concrete type's drop glue.
            Type::Trait(_) => true,
            _ => false,
        };
        visiting.pop();
        result
    }
}
