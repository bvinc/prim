pub use prim_parse::{BinaryOp, InternSymbol, Interner, PassMode};
pub use prim_tok::{FileId, ModuleId, Span};
use std::fmt;
use std::sync::Arc;

pub mod typecheck;
pub use typecheck::{TypeCheckError, TypeCheckKind, type_check};

pub mod mono;
pub use mono::monomorphize;

pub mod ownership;
pub use ownership::{MoveError, MoveErrorKind, check as check_ownership};

pub mod cfg;

pub mod drop_info;
pub use drop_info::DropInfo;

pub mod drop_elab;
pub use drop_elab::elaborate as elaborate_drops;

pub mod usefulness;

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
    pub impl_methods: std::collections::HashMap<(MethodOwner, InternSymbol), ImplFn>,
    /// `(trait, owner)` → vec of FuncIds in trait method declaration order.
    /// Owner is the implementing type (struct, enum, or primitive). Used to
    /// check trait-bound satisfaction, generate vtables (struct owners only),
    /// and dispatch dynamic method calls.
    pub impls: std::collections::HashMap<(TraitId, MethodOwner), Vec<FuncId>>,
    /// `(owner, method-name)` pairs provided by more than one trait. A concrete
    /// `value.method()` on such a pair is ambiguous and rejected (the bound
    /// disambiguates a generic call, but a concrete one has no bound).
    pub ambiguous_methods: std::collections::HashSet<(MethodOwner, InternSymbol)>,
    pub symbols: Vec<Symbol>,
    /// Shared with the loader and all parsed files in this compilation.
    /// `Arc` because `ThreadedRodeo` isn't `Clone` (it holds internal state
    /// that wouldn't make sense to duplicate).
    pub interner: Arc<Interner>,
    pub main: Option<SymbolId>,
    /// `@entry` function (the program's `_start`), if one is declared.
    pub entry: Option<FuncId>,
    pub spans: Vec<(FileId, Span)>,
}

/// The type an `impl` block attaches functions to: a struct, an enum, or a
/// primitive. Both methods and associated functions resolve through
/// `Program::impl_methods`, keyed by `(owner, name)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MethodOwner {
    Struct(StructId),
    Enum(EnumId),
    Prim(PrimKind),
    /// The `Array[T, N]` type family. A single owner for all array impls; the
    /// element type and length are the impl's own (const) generic parameters.
    Array,
}

/// Primitive types usable as `impl` targets (e.g. `impl u8 { ... }`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PrimKind {
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
    Bool,
    F32,
    F64,
}

impl MethodOwner {
    /// The owner a receiver/target type resolves to, if any.
    pub fn of_type(ty: &Type) -> Option<MethodOwner> {
        Some(match ty {
            Type::Struct(id, _) => MethodOwner::Struct(*id),
            Type::Enum(id, _) => MethodOwner::Enum(*id),
            Type::Array(_, _) => MethodOwner::Array,
            Type::U8 => MethodOwner::Prim(PrimKind::U8),
            Type::I8 => MethodOwner::Prim(PrimKind::I8),
            Type::U16 => MethodOwner::Prim(PrimKind::U16),
            Type::I16 => MethodOwner::Prim(PrimKind::I16),
            Type::U32 => MethodOwner::Prim(PrimKind::U32),
            Type::I32 => MethodOwner::Prim(PrimKind::I32),
            Type::U64 => MethodOwner::Prim(PrimKind::U64),
            Type::I64 => MethodOwner::Prim(PrimKind::I64),
            Type::Usize => MethodOwner::Prim(PrimKind::Usize),
            Type::Isize => MethodOwner::Prim(PrimKind::Isize),
            Type::Bool => MethodOwner::Prim(PrimKind::Bool),
            Type::F32 => MethodOwner::Prim(PrimKind::F32),
            Type::F64 => MethodOwner::Prim(PrimKind::F64),
            _ => return None,
        })
    }
}

/// An `impl` function: its `FuncId` plus whether it takes a `self` receiver
/// (a method, called `value.m(..)`) or not (an associated function, called
/// `Type.f(..)`).
#[derive(Clone, Copy, Debug)]
pub struct ImplFn {
    pub func: FuncId,
    pub is_method: bool,
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
    /// `now_nanos() -> u64` — current monotonic time in nanoseconds, via WASI
    /// `clock_time_get`. Backs `std.time` and the scheduler's wakers.
    ClockNow,
    /// `read_raw(fd: i32, ptr: *mut u8, cap: usize) -> usize` — read up to
    /// `cap` bytes from `fd` into `ptr` via WASI `fd_read`; returns the count
    /// read (0 at EOF).
    Read,
    /// `path_open_raw(dir_fd, dirflags, path, path_len, oflags, rights_base,
    /// rights_inheriting, fdflags, opened_out) -> errno` — direct WASI
    /// `path_open` passthrough backing `std.fs.File.open`.
    PathOpen,
    /// `close_fd(fd: i32) -> i32` — WASI `fd_close`; backs `std.fs` close/Drop.
    Close,
    /// `poll(subs: *mut u8, events: *mut u8, nsubs: usize) -> usize` — block in
    /// WASI `poll_oneoff` on `nsubs` subscriptions until at least one fires;
    /// returns the number of events written. The scheduler builds the
    /// subscription/event structs and waits on every parked task's waker.
    Poll,
    Yield,
    /// `resume(handle) -> bool` — run the task in slot `handle` until it yields
    /// or finishes; true if it yielded. Calls the runtime resume helper.
    Resume,
    /// `task_count() -> usize` — number of slots in the task table.
    TaskCount,
    /// `task_live(handle) -> bool` — whether the task slot still holds a
    /// (non-null) continuation.
    TaskLive,
    /// Marks the `spawn` builtin. Calls to it are recognized during lowering
    /// and rewritten to `ExprKind::Spawn`; this variant never reaches codegen.
    Spawn,
    /// `spawn_main()` — seed the program's `main` as a task. Lets the stdlib
    /// entry point spawn `main` without naming it across modules.
    SpawnMain,
    /// `trap()` — abort execution (wasm `unreachable`). Backs `panic`.
    Trap,
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
    /// `at[T](base, off) -> *mut T` — a typed pointer to the value of type `T`
    /// at `base + off` bytes. Runtime-identical to `PtrByteAdd` (an `i32`
    /// address add); `T` only types the result so a following deref loads the
    /// right width. This is how header fields are read out of a raw byte
    /// region without a reinterpret cast.
    At,
    /// `from_addr[T](a) -> *mut T` — the pointer with numeric address `a`. The
    /// dual of `PtrAddr` (`*mut T -> usize`) and a no-op at the wasm level (a
    /// pointer *is* its i32 address). Needed to recover a pointer from integer
    /// state that can't itself be pointer-typed — e.g. the allocator's root,
    /// which must live in a `usize` global because globals take a literal init
    /// and there is no pointer literal.
    FromAddr,
    /// `array_ptr[T, const N](a: Array[T, N]) -> *mut T` — a pointer to the
    /// array's first element. A no-op at the wasm level: a boxed array's value
    /// already is the address of its inline storage. This is the one compiler
    /// primitive array access needs; `get`/`len` are then ordinary Prim.
    ArrayPtr,
    /// Integer conversion primitives (std.convert). One wasm operation backs
    /// many named conversions; the source/destination types live in the std
    /// function signatures, not here.
    ConvNoop,
    ConvTruncU8,
    ConvTruncU16,
    ConvSext8,
    ConvSext16,
    ConvExtI32S,
    ConvExtI32U,
    ConvWrapI64,
    ConvWrapTruncU8,
    ConvWrapTruncU16,
    ConvWrapSext8,
    ConvWrapSext16,
    // Float <-> integer conversions used by float printing. The float-to-integer
    // direction truncates toward zero (drops the fraction) and saturates on
    // overflow/NaN, so the name carries `trunc`. Integer-to-float rounds to the
    // nearest representable value; f32->f64 is an exact widen.
    F64ToU64Trunc,
    U64ToF64,
    F32ToF64,
}

impl RuntimeAbi {
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "prim_rt_write" => Some(Self::Write),
            "prim_rt_now" => Some(Self::ClockNow),
            "prim_rt_read" => Some(Self::Read),
            "prim_rt_path_open" => Some(Self::PathOpen),
            "prim_rt_close" => Some(Self::Close),
            "prim_rt_poll" => Some(Self::Poll),
            "prim_rt_resume" => Some(Self::Resume),
            "prim_rt_task_count" => Some(Self::TaskCount),
            "prim_rt_task_live" => Some(Self::TaskLive),
            "prim_rt_spawn" => Some(Self::Spawn),
            "prim_rt_spawn_main" => Some(Self::SpawnMain),
            "prim_rt_trap" => Some(Self::Trap),
            "prim_rt_size_of" => Some(Self::SizeOf),
            "prim_rt_null" => Some(Self::Null),
            "prim_rt_ptr_byte_add" => Some(Self::PtrByteAdd),
            "prim_rt_ptr_byte_sub" => Some(Self::PtrByteSub),
            "prim_rt_ptr_byte_offset" => Some(Self::PtrByteOffset),
            "prim_rt_ptr_addr" => Some(Self::PtrAddr),
            "prim_rt_at" => Some(Self::At),
            "prim_rt_from_addr" => Some(Self::FromAddr),
            "prim_rt_array_ptr" => Some(Self::ArrayPtr),
            // prim_rt_alloc / prim_rt_free intentionally have no mapping: the
            // allocator is now Prim code (std.mem), called as a normal function.
            "prim_rt_f64_to_u64_trunc" => Some(Self::F64ToU64Trunc),
            "prim_rt_u64_to_f64" => Some(Self::U64ToF64),
            "prim_rt_f32_to_f64" => Some(Self::F32ToF64),
            "prim_rt_conv_noop" => Some(Self::ConvNoop),
            "prim_rt_conv_trunc_u8" => Some(Self::ConvTruncU8),
            "prim_rt_conv_trunc_u16" => Some(Self::ConvTruncU16),
            "prim_rt_conv_sext8" => Some(Self::ConvSext8),
            "prim_rt_conv_sext16" => Some(Self::ConvSext16),
            "prim_rt_conv_ext_i32_s" => Some(Self::ConvExtI32S),
            "prim_rt_conv_ext_i32_u" => Some(Self::ConvExtI32U),
            "prim_rt_conv_wrap_i64" => Some(Self::ConvWrapI64),
            "prim_rt_conv_wrap_trunc_u8" => Some(Self::ConvWrapTruncU8),
            "prim_rt_conv_wrap_trunc_u16" => Some(Self::ConvWrapTruncU16),
            "prim_rt_conv_wrap_sext8" => Some(Self::ConvWrapSext8),
            "prim_rt_conv_wrap_sext16" => Some(Self::ConvWrapSext16),
            "prim_rt_yield" => Some(Self::Yield),
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
    /// Whether this is an ordinary type parameter `T` or a const value
    /// parameter `const N: usize` (usable as a value in the body).
    pub kind: ParamKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ParamKind {
    Type,
    /// A const generic parameter; its value type is `usize` for now.
    Const,
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

/// One variant of an enum. Unit variants have `fields` empty. Tuple variants
/// (`is_tuple`) have positionally-named fields `0`, `1`, … and use positional
/// construction/pattern syntax.
#[derive(Clone, Debug)]
pub struct Variant {
    pub name: InternSymbol,
    pub fields: Vec<Field>,
    pub is_tuple: bool,
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
    /// Passing modes parallel to `params` (including the receiver at index 0),
    /// so receiver/arg modes are known for dynamically/bound-dispatched calls.
    pub param_modes: Vec<PassMode>,
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
    /// How the parameter is passed (`view`/`edit`/`take`). Checked by the
    /// ownership pass, then ignored by mono/codegen.
    pub mode: PassMode,
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
    /// `let <pattern> [: ty] = value`. The pattern is irrefutable (wildcard,
    /// binding, or tuples thereof); `ty` is the optional annotation
    /// (`Undetermined` if absent), applied to `value` before the pattern is
    /// typed against the result.
    Let {
        pattern: Pattern,
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
    /// `object.field = value` — store to a struct field.
    FieldAssign {
        object: Expr,
        field: InternSymbol,
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
    /// Drop the owned local `sym` (of concrete type `ty`): run its `Drop` glue,
    /// recursively drop its fields, then free its heap box. Inserted by the
    /// drop-elaboration pass after monomorphization; never written in source
    /// and never present before that pass. `id` is a unique tag assigned at
    /// insertion that ties this candidate to its CFG drop action and decision
    /// (so the analysis need not match candidates by traversal position).
    Drop {
        sym: SymbolId,
        ty: Type,
        span: SpanId,
        id: usize,
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
        /// Passing modes parallel to `args` (after the MethodCall→Call rewrite,
        /// index 0 is the receiver's mode). Consumed by the ownership pass.
        arg_modes: Vec<PassMode>,
    },
    /// `spawn(f)` — create a green-thread task from a `fn() -> ()` and return
    /// its handle (a `usize` slot in the task table). Lowers to
    /// `ref.func f; cont.new; table.grow`.
    Spawn {
        func: FuncId,
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
        /// Passing modes for the non-receiver `args` (parallel). The receiver's
        /// mode is resolved from the callee's `self` parameter at typecheck.
        arg_modes: Vec<PassMode>,
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
        /// Passing modes for the non-receiver `args` (parallel). The receiver's
        /// mode comes from the trait method's `param_modes[0]`.
        arg_modes: Vec<PassMode>,
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
        /// Passing modes for the non-receiver `args` (parallel). The receiver's
        /// mode comes from the bound trait method's `param_modes[0]`.
        arg_modes: Vec<PassMode>,
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
    /// Unary arithmetic negation, `-operand`.
    Neg(Box<Expr>),
    /// A tuple literal, `(a, b, ...)`.
    TupleLit(Vec<Expr>),
    /// Positional tuple access, `tuple.index`.
    TupleIndex {
        base: Box<Expr>,
        index: u32,
    },
    ArrayLit(Vec<Expr>),
    /// A reference to a const generic parameter used as a value (e.g. `N` in
    /// the body of `fn f[const N: usize]`). Typed `usize`; monomorphization
    /// replaces it with the concrete `Int` literal.
    ConstParam(TypeParamId),
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

/// A pattern, shared by `let` bindings and `match` arms. Recursive: tuple
/// patterns nest sub-patterns and variant fields nest sub-patterns. The `ty`
/// fields are filled in by typecheck.
#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard {
        ty: Type,
        span: SpanId,
    },
    /// A name binding — matches anything and binds it to `symbol`. `mode` is
    /// `Take` when the binding moves the value out of a `match` scrutinee
    /// (consuming it); `View` otherwise (`let` bindings and borrows).
    Binding {
        symbol: SymbolId,
        ty: Type,
        mode: PassMode,
        span: SpanId,
    },
    /// `(a, b, ...)` — matched element-wise against a tuple value. `ty` is the
    /// whole tuple type.
    Tuple {
        elems: Vec<Pattern>,
        ty: Type,
        span: SpanId,
    },
    /// An integer literal pattern. `ty` is the resolved scrutinee type.
    Int {
        value: i64,
        ty: Type,
        span: SpanId,
    },
    /// A boolean literal pattern.
    Bool {
        value: bool,
        span: SpanId,
    },
    Variant {
        enum_id: EnumId,
        variant_idx: u32,
        /// Field sub-patterns, ordered as written. Empty for unit variants.
        fields: Vec<FieldPattern>,
        span: SpanId,
    },
    /// A struct destructuring pattern, e.g. `Point { x, y }`. Irrefutable; may
    /// bind a subset of the struct's fields.
    Struct {
        struct_id: StructId,
        fields: Vec<FieldPattern>,
        span: SpanId,
    },
}

#[derive(Clone, Debug)]
pub struct FieldPattern {
    /// Field name on the variant.
    pub field: InternSymbol,
    /// The field's resolved type (filled in by typecheck).
    pub ty: Type,
    /// Sub-pattern matched against the field's value.
    pub pattern: Pattern,
}

impl Pattern {
    pub fn span(&self) -> SpanId {
        match self {
            Pattern::Wildcard { span, .. }
            | Pattern::Binding { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::Int { span, .. }
            | Pattern::Bool { span, .. }
            | Pattern::Variant { span, .. }
            | Pattern::Struct { span, .. } => *span,
        }
    }
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
    /// Fixed-size array `Array[T, N]`: element type and a length. The length
    /// is itself a `Type` so it rides the generic machinery — `ConstInt(n)`
    /// for a concrete length, `Param(id)` for a const generic parameter.
    Array(Box<Type>, Box<Type>),
    /// A const integer used as a generic argument (currently only an array
    /// length). Appears in substitution/inference/mono-key positions, never as
    /// the type of a runtime value.
    ConstInt(u64),
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
    /// An anonymous product type, `(A, B, ...)`. Structural: two tuples with
    /// the same element types are the same type. Boxed on the heap like a
    /// struct, with positional fields.
    Tuple(Vec<Type>),
    /// The unit type — an expression that yields no value (a statement, an
    /// empty block, a call to a function with no declared return). Distinct
    /// from `Undetermined`: unit is a fully-determined "no value", whereas
    /// `Undetermined` means the type checker could not determine a type (an
    /// error). `produces_value` is false only for `Unit`.
    Unit,
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
            Type::Array(elem, n) => write!(f, "Array[{elem}, {n}]"),
            Type::Tuple(elems) => {
                write!(f, "(")?;
                for (i, t) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
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
            Type::ConstInt(n) => write!(f, "{n}"),
            Type::Pointer { mutable, pointee } => {
                if *mutable {
                    write!(f, "*mut {pointee}")
                } else {
                    write!(f, "*const {pointee}")
                }
            }
            Type::Unit => write!(f, "()"),
            Type::IntVar => write!(f, "{{integer}}"),
            Type::FloatVar => write!(f, "{{float}}"),
            Type::Undetermined => write!(f, "unknown"),
        }
    }
}
