//! Common utilities for the Prim compiler.
//!
//! This crate provides foundational types used throughout the compilation
//! pipeline, including string interning for name deduplication.

use string_interner::StringInterner;
use string_interner::backend::BufferBackend;
use string_interner::symbol::SymbolU32;

/// Interned symbol handle for names. Cheap to copy and compare.
pub type InternSymbol = SymbolU32;

/// String interner for name deduplication.
pub type Interner = StringInterner<BufferBackend<InternSymbol>>;
