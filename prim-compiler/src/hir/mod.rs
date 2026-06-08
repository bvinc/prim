pub use prim_parse::{BinaryOp, InternSymbol, Interner};
pub use prim_tok::{FileId, ModuleId, Span};
use std::fmt;
use std::sync::Arc;

pub mod typecheck;
pub use typecheck::{TypeCheckError, TypeCheckKind, type_check};

pub mod mono;
pub use mono::monomorphize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StructId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TraitId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct EnumId(pub u32);

/// Position of a generic function's type parameter within its
/// `type_params` vec. `Type::Param(TypeParamId(i))` refers to the i-th
/// parameter of the enclosing function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeParamId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpanId(pub u32);

#[derive(Clone, Debug)]
pub struct Program {
    pub modules: Vec<Module>,
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub globals: Vec<Global>,
    pub traits: Vec<Trait>,
    /// `(receiver type, method name)` → impl method's FuncId. Populated
    /// by lowering each `impl Trait for Type { fn ... }` block, where the
    /// receiver type is a struct or an enum. Method calls in expressions
    /// look up here at typecheck time to resolve static dispatch.
    pub impl_methods: std::collections::HashMap<(MethodOwner, InternSymbol), FuncId>,
    /// `(trait, struct)` → vec of FuncIds in trait method declaration order.
    /// Used to generate vtables and to dispatch dynamic method calls.
    pub impls: std::collections::HashMap<(TraitId, StructId), Vec<FuncId>>,
    pub symbols: Vec<Symbol>,
    /// Shared with the loader and all parsed files in this compilation.
    /// `Arc` because `ThreadedRodeo` isn't `Clone` (it holds internal state
    /// that wouldn't make sense to duplicate).
    pub interner: Arc<Interner>,
    pub main: Option<SymbolId>,
    pub spans: Vec<(FileId, Span)>,
}

/// The concrete type an `impl` block attaches methods to. Both struct and
/// enum receivers resolve method calls through `Program::impl_methods`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MethodOwner {
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: ModuleId,
    pub name: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: FuncId,
    pub name: SymbolId,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
    pub body: Block,
    pub span: SpanId,
    pub runtime: Option<RuntimeAbi>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuntimeAbi {
    Write,
    Yield,
    PrintlnI64,
    PrintlnI32,
    PrintlnI16,
    PrintlnI8,
    PrintlnIsize,
    PrintlnU64,
    PrintlnU32,
    PrintlnU16,
    PrintlnU8,
    PrintlnUsize,
    PrintlnBool,
    PrintlnF64,
    PrintlnF32,
    NullMutU8,
    NullMutU32,
    NullMutUsize,
    PtrAddMutU8,
    PtrAddMutU32,
    PtrAddMutUsize,
    PtrSubMutU8,
    PtrSubMutU32,
    PtrSubMutUsize,
    PtrOffsetMutU8,
    PtrOffsetMutU32,
    PtrOffsetMutUsize,
    PtrByteAddMutU8,
    PtrByteAddMutU32,
    PtrByteAddMutUsize,
    PtrByteSubMutU8,
    PtrByteSubMutU32,
    PtrByteSubMutUsize,
    PtrByteOffsetMutU8,
    PtrByteOffsetMutU32,
    PtrByteOffsetMutUsize,
    PtrAddrMutU8,
    PtrAddrMutU32,
    PtrAddrMutUsize,
    MemoryGrow,
    MemoryCopy,
    MemoryFill,
    ClzU32,
    CtzU32,
    PopcntU32,
    ClzU64,
    CtzU64,
    PopcntU64,
    /// `size_of[T]()` — folded to a constant byte count in monomorphization;
    /// never reaches codegen.
    SizeOf,
    /// Generic pointer primitives over `*mut T`. Type-independent at the wasm
    /// level (a pointer is an `i32` address), so one intrinsic each serves
    /// every `T`; element scaling is done in Prim via `size_of[T]()`.
    Null,
    PtrByteAdd,
    PtrByteSub,
    PtrByteOffset,
    PtrAddr,
    /// Heap allocation backed by the bump allocator. `Free` does not reclaim
    /// yet (see std.mem); it exists so call sites can establish ownership
    /// discipline now.
    Alloc,
    Free,
}

impl RuntimeAbi {
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "prim_rt_write" => Some(Self::Write),
            "prim_rt_size_of" => Some(Self::SizeOf),
            "prim_rt_null" => Some(Self::Null),
            "prim_rt_ptr_byte_add" => Some(Self::PtrByteAdd),
            "prim_rt_ptr_byte_sub" => Some(Self::PtrByteSub),
            "prim_rt_ptr_byte_offset" => Some(Self::PtrByteOffset),
            "prim_rt_ptr_addr" => Some(Self::PtrAddr),
            "prim_rt_alloc" => Some(Self::Alloc),
            "prim_rt_free" => Some(Self::Free),
            "prim_rt_yield" => Some(Self::Yield),
            "prim_rt_println_i64" => Some(Self::PrintlnI64),
            "prim_rt_println_i32" => Some(Self::PrintlnI32),
            "prim_rt_println_i16" => Some(Self::PrintlnI16),
            "prim_rt_println_i8" => Some(Self::PrintlnI8),
            "prim_rt_println_isize" => Some(Self::PrintlnIsize),
            "prim_rt_println_u64" => Some(Self::PrintlnU64),
            "prim_rt_println_u32" => Some(Self::PrintlnU32),
            "prim_rt_println_u16" => Some(Self::PrintlnU16),
            "prim_rt_println_u8" => Some(Self::PrintlnU8),
            "prim_rt_println_usize" => Some(Self::PrintlnUsize),
            "prim_rt_println_bool" => Some(Self::PrintlnBool),
            "prim_rt_println_f64" => Some(Self::PrintlnF64),
            "prim_rt_println_f32" => Some(Self::PrintlnF32),
            "prim_rt_null_mut_u8" => Some(Self::NullMutU8),
            "prim_rt_null_mut_u32" => Some(Self::NullMutU32),
            "prim_rt_null_mut_usize" => Some(Self::NullMutUsize),
            "prim_rt_ptr_add_mut_u8" => Some(Self::PtrAddMutU8),
            "prim_rt_ptr_add_mut_u32" => Some(Self::PtrAddMutU32),
            "prim_rt_ptr_add_mut_usize" => Some(Self::PtrAddMutUsize),
            "prim_rt_ptr_sub_mut_u8" => Some(Self::PtrSubMutU8),
            "prim_rt_ptr_sub_mut_u32" => Some(Self::PtrSubMutU32),
            "prim_rt_ptr_sub_mut_usize" => Some(Self::PtrSubMutUsize),
            "prim_rt_ptr_offset_mut_u8" => Some(Self::PtrOffsetMutU8),
            "prim_rt_ptr_offset_mut_u32" => Some(Self::PtrOffsetMutU32),
            "prim_rt_ptr_offset_mut_usize" => Some(Self::PtrOffsetMutUsize),
            "prim_rt_ptr_byte_add_mut_u8" => Some(Self::PtrByteAddMutU8),
            "prim_rt_ptr_byte_add_mut_u32" => Some(Self::PtrByteAddMutU32),
            "prim_rt_ptr_byte_add_mut_usize" => Some(Self::PtrByteAddMutUsize),
            "prim_rt_ptr_byte_sub_mut_u8" => Some(Self::PtrByteSubMutU8),
            "prim_rt_ptr_byte_sub_mut_u32" => Some(Self::PtrByteSubMutU32),
            "prim_rt_ptr_byte_sub_mut_usize" => Some(Self::PtrByteSubMutUsize),
            "prim_rt_ptr_byte_offset_mut_u8" => Some(Self::PtrByteOffsetMutU8),
            "prim_rt_ptr_byte_offset_mut_u32" => Some(Self::PtrByteOffsetMutU32),
            "prim_rt_ptr_byte_offset_mut_usize" => Some(Self::PtrByteOffsetMutUsize),
            "prim_rt_ptr_addr_mut_u8" => Some(Self::PtrAddrMutU8),
            "prim_rt_ptr_addr_mut_u32" => Some(Self::PtrAddrMutU32),
            "prim_rt_ptr_addr_mut_usize" => Some(Self::PtrAddrMutUsize),
            "prim_rt_memory_grow" => Some(Self::MemoryGrow),
            "prim_rt_memory_copy" => Some(Self::MemoryCopy),
            "prim_rt_memory_fill" => Some(Self::MemoryFill),
            "prim_rt_clz_u32" => Some(Self::ClzU32),
            "prim_rt_ctz_u32" => Some(Self::CtzU32),
            "prim_rt_popcnt_u32" => Some(Self::PopcntU32),
            "prim_rt_clz_u64" => Some(Self::ClzU64),
            "prim_rt_ctz_u64" => Some(Self::CtzU64),
            "prim_rt_popcnt_u64" => Some(Self::PopcntU64),
            _ => None,
        }
    }
}

/// A type parameter in a generic function's signature. `bound`, if
/// present, restricts which concrete types may be substituted in and
/// permits calling that trait's methods on values of this type within
/// the body.
#[derive(Clone, Debug)]
pub struct TypeParam {
    pub name: SymbolId,
    pub bound: Option<TraitId>,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: StructId,
    pub name: SymbolId,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<Field>,
    pub span: SpanId,
}

/// A tagged-union enum. Values are heap pointers to a `{discriminant:
/// u32, payload: max(variant payload sizes)}` block. The discriminant
/// is the variant's index in `variants`.
#[derive(Clone, Debug)]
pub struct Enum {
    pub id: EnumId,
    pub name: SymbolId,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<Variant>,
    /// Variant name → position in `variants`. O(1) lookup at typecheck
    /// and pattern-match time.
    pub variant_idx: std::collections::HashMap<InternSymbol, u32>,
    pub span: SpanId,
}

/// One variant of an enum. Unit variants have `fields` empty.
#[derive(Clone, Debug)]
pub struct Variant {
    pub name: InternSymbol,
    pub fields: Vec<Field>,
    pub span: SpanId,
}

/// A trait: a set of method signatures. Trait values are fat pointers
/// `{vtable_addr: i32, data_addr: i32}` at the wasm level.
#[derive(Clone, Debug)]
pub struct Trait {
    pub id: TraitId,
    pub name: SymbolId,
    pub methods: Vec<TraitMethodSig>,
    /// Method name → position in `methods`. Lets typecheck resolve a
    /// `receiver.method()` call in O(1) instead of scanning.
    pub method_idx: std::collections::HashMap<InternSymbol, u32>,
    pub span: SpanId,
}

/// Trait method signature. The method's index in `Trait::methods` is its
/// vtable slot. `params` includes the receiver position (always the trait's
/// own type at trait-declaration time).
#[derive(Clone, Debug)]
pub struct TraitMethodSig {
    pub name: InternSymbol,
    pub params: Vec<Type>,
    pub ret: Option<Type>,
    pub span: SpanId,
}

/// A module-level mutable or immutable global. The initializer is a
/// constant value (numeric or bool literal) — wasm only permits constant
/// expressions in global initializers.
#[derive(Clone, Debug)]
pub struct Global {
    pub id: GlobalId,
    pub name: SymbolId,
    pub mutable: bool,
    pub ty: Type,
    pub init: GlobalInit,
    pub span: SpanId,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GlobalInit {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: SymbolId,
    pub ty: Type,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: InternSymbol,
    pub ty: Type,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    /// Trailing expression (without semicolon) - the block's value.
    pub expr: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let {
        name: SymbolId,
        mutable: bool,
        ty: Type,
        value: Expr,
        span: SpanId,
    },
    Assign {
        target: SymbolId,
        value: Expr,
        span: SpanId,
    },
    DerefAssign {
        ptr: Expr,
        value: Expr,
        span: SpanId,
    },
    Expr(Expr),
    Loop {
        body: Block,
        span: SpanId,
    },
    While {
        condition: Expr,
        body: Block,
        span: SpanId,
    },
    Break {
        span: SpanId,
    },
    Return {
        value: Option<Expr>,
        span: SpanId,
    },
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Ident(SymbolId),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        func: FuncId,
        /// Concrete types substituted for the callee's type parameters in
        /// declaration order. Empty for non-generic callees. Populated by
        /// typecheck via call-site inference; consumed by monomorphization
        /// to dispatch the call to the right specialization.
        type_args: Vec<Type>,
        args: Vec<Expr>,
    },
    StructLit {
        struct_id: StructId,
        /// Concrete types substituted for the struct's type parameters
        /// in declaration order. Empty for non-generic structs.
        /// Populated by typecheck inference, consumed by mono.
        type_args: Vec<Type>,
        fields: Vec<(InternSymbol, Expr)>,
    },
    /// `Enum.Variant` (unit) or `Enum.Variant { field = expr, ... }`
    /// (struct-like).
    VariantLit {
        enum_id: EnumId,
        variant_idx: u32,
        type_args: Vec<Type>,
        fields: Vec<(InternSymbol, Expr)>,
    },
    /// `match scrutinee { arms... }`. Arms are checked left-to-right at
    /// codegen via discriminant equality.
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Field {
        base: Box<Expr>,
        field: InternSymbol,
    },
    Deref(Box<Expr>),
    /// `receiver.method(args)` — kept in HIR until typecheck, which either
    /// rewrites to `Call` (concrete receiver) or to `DynCall` (trait
    /// receiver).
    MethodCall {
        receiver: Box<Expr>,
        method: InternSymbol,
        args: Vec<Expr>,
    },
    /// Dynamic method dispatch through a trait fat pointer. Emitted by
    /// typecheck when the receiver type is `Type::Trait(tid)`. `method_idx`
    /// is the position of the method in the trait's declaration order
    /// (i.e. the vtable slot).
    DynCall {
        receiver: Box<Expr>,
        trait_id: TraitId,
        method_idx: u32,
        args: Vec<Expr>,
    },
    /// A method call on a value whose type is `Type::Param(i)` with a
    /// declared bound. Resolved at monomorphization: after `T` is
    /// substituted to a concrete struct `S`, this is rewritten to a
    /// direct `Call` via `impl_methods[(S, method)]`.
    TraitBoundCall {
        receiver: Box<Expr>,
        type_param: TypeParamId,
        bound: TraitId,
        method: InternSymbol,
        args: Vec<Expr>,
    },
    /// Box a concrete-typed value into a trait fat pointer. Emitted by
    /// typecheck when a `Type::Struct(sid)` value flows into a
    /// `Type::Trait(tid)` slot. Codegen materializes the fat pointer struct
    /// `{vtable_addr, data_addr}` on the heap.
    Coerce {
        value: Box<Expr>,
        source_struct: StructId,
        target_trait: TraitId,
    },
    BitNot(Box<Expr>),
    ArrayLit(Vec<Expr>),
    Dbg {
        /// Pre-rendered `[path:line:col] expr_text = ` prefix string,
        /// computed at lowering time so codegen needs no source access.
        prefix: String,
        inner: Box<Expr>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Block(Block),
    /// Placeholder for expressions that failed during lowering.
    Error,
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard {
        span: SpanId,
    },
    Variant {
        enum_id: EnumId,
        variant_idx: u32,
        /// `(field name, local symbol introduced into the arm body, local type)`
        /// triples. Empty for unit variants.
        bindings: Vec<(InternSymbol, SymbolId, Type)>,
        span: SpanId,
    },
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub id: SymbolId,
    pub module: ModuleId,
    pub name: InternSymbol,
    pub kind: SymbolKind,
}

#[derive(Clone, Debug)]
pub enum SymbolKind {
    Module,
    Function(FuncId),
    Struct(StructId),
    Global(GlobalId),
    Param,
    Local,
    Trait,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Usize,
    Isize,
    F32,
    F64,
    Bool,
    Array(Box<Type>),
    /// A struct type with optional concrete type arguments. Empty
    /// `Vec<Type>` means a non-generic struct or an as-yet-uninstantiated
    /// generic; non-empty means a specific instantiation that mono will
    /// turn into a fresh concrete `StructId`.
    Struct(StructId, Vec<Type>),
    /// A trait type — at the wasm level a pointer to an 8-byte fat pointer
    /// struct `{vtable_addr, data_addr}`.
    Trait(TraitId),
    /// An enum type with optional type arguments. Like `Type::Struct`,
    /// non-empty args means a specific instantiation that mono will
    /// turn into a fresh concrete `EnumId`.
    Enum(EnumId, Vec<Type>),
    /// A type parameter `T` within a generic function's signature or body.
    /// Substituted to a concrete type by monomorphization before codegen.
    Param(TypeParamId),
    Pointer {
        mutable: bool,
        pointee: Box<Type>,
    },
    /// Undetermined integer type (will default to i32).
    IntVar,
    /// Undetermined float type (will default to f64).
    FloatVar,
    Undetermined,
}

impl Type {
    pub fn as_struct(&self) -> Option<StructId> {
        match self {
            Type::Struct(id, _) => Some(*id),
            _ => None,
        }
    }

    /// In-memory size of a value of this type, in bytes. Structs, enums,
    /// arrays, traits, and pointers are all 4-byte heap addresses; scalars
    /// use their natural width. Single source of truth shared by struct
    /// layout, `size_of`, and pointer arithmetic.
    pub fn size_bytes(&self) -> u32 {
        match self {
            Type::Bool | Type::I8 | Type::U8 => 1,
            Type::I16 | Type::U16 => 2,
            Type::I64 | Type::U64 | Type::F64 | Type::FloatVar => 8,
            _ => 4,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::U8 => write!(f, "u8"),
            Type::I8 => write!(f, "i8"),
            Type::U16 => write!(f, "u16"),
            Type::I16 => write!(f, "i16"),
            Type::U32 => write!(f, "u32"),
            Type::I32 => write!(f, "i32"),
            Type::U64 => write!(f, "u64"),
            Type::I64 => write!(f, "i64"),
            Type::Usize => write!(f, "usize"),
            Type::Isize => write!(f, "isize"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Array(elem) => write!(f, "[{elem}]"),
            Type::Struct(id, args) => {
                if args.is_empty() {
                    write!(f, "struct {:?}", id)
                } else {
                    write!(f, "struct {:?}<", id)?;
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    write!(f, ">")
                }
            }
            Type::Trait(id) => write!(f, "trait {:?}", id),
            Type::Enum(id, args) => {
                if args.is_empty() {
                    write!(f, "enum {:?}", id)
                } else {
                    write!(f, "enum {:?}<", id)?;
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    write!(f, ">")
                }
            }
            Type::Param(id) => write!(f, "T#{}", id.0),
            Type::Pointer { mutable, pointee } => {
                if *mutable {
                    write!(f, "*mut {pointee}")
                } else {
                    write!(f, "*const {pointee}")
                }
            }
            Type::IntVar => write!(f, "{{integer}}"),
            Type::FloatVar => write!(f, "{{float}}"),
            Type::Undetermined => write!(f, "unknown"),
        }
    }
}
