use prim_tok::Tokenizer;

mod error;
pub use error::{Diagnostic, ParseError, Severity};

pub use prim_tok::Span;

/// Interned symbol handle for names. Cheap to copy and compare.
pub type InternSymbol = lasso::Spur;

/// String interner for name deduplication.
///
/// Uses `lasso::ThreadedRodeo` so a single shared interner can be passed by
/// shared reference (`&Interner`) into parallel parses without locking on
/// reads. All `InternSymbol`s across a compilation come from this one
/// interner, which makes them universally comparable.
pub type Interner = lasso::ThreadedRodeo<InternSymbol>;

mod number;
mod parser;

/// An identifier with its interned symbol and source span.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ident {
    pub sym: InternSymbol,
    pub span: Span,
}
use parser::Parser;

#[derive(Debug, Clone, PartialEq)]
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
    /// Fixed-size array `Array[T, N]`: element type and a length that is either
    /// a literal or a const generic parameter name.
    Array(Box<Type>, ConstArg),
    /// Named type — either a struct or a trait. The optional type
    /// argument list lets generic instantiations like `Pair<i32>` appear
    /// in type positions (function params, return, let bindings). Empty
    /// vec means a plain name with no generic args.
    Struct(InternSymbol, Vec<Type>),
    Pointer {
        mutable: bool,
        pointee: Box<Type>,
    },
    /// An anonymous product type, `(A, B, ...)` with two or more elements.
    Tuple(Vec<Type>),
    /// `Self` (and a bare `self` parameter): the type an `impl`/`trait` is
    /// for. Resolved to the concrete target (in an impl) or the trait type
    /// (in a trait declaration) during HIR lowering.
    SelfType,
    Undetermined, // Type not yet determined during parsing
}

/// An expression with its span and type.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub span: Span,
    pub ty: Type,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ident(Ident),
    /// Dotted identifier path, e.g. `std.io.println_i32`, `Option.Some`,
    /// or `value.field`. Lowering resolves whether this names modules/enums
    /// or is value field access.
    Path(NamePath),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    FunctionCall {
        path: NamePath,
        args: Vec<Expr>,
        /// Per-argument passing modes, parallel to `args` (same length). A
        /// bare argument is `View`. Kept as a parallel vec so existing `args`
        /// iteration sites are untouched.
        arg_modes: Vec<PassMode>,
        /// Explicit type arguments from a turbofish call `f[T](args)`.
        /// Empty when omitted; the type checker infers them from value
        /// arguments in that case.
        type_args: Vec<Type>,
    },
    StructLiteral {
        name: Ident,
        fields: Vec<StructField>,
    },
    /// `Enum.Variant` (unit) or `Enum.Variant { field = expr, ... }`
    /// (struct-like). Resolved to a specific enum + variant index in
    /// HIR lowering.
    VariantLiteral {
        enum_path: NamePath,
        variant_name: Ident,
        fields: Vec<StructField>,
    },
    /// `match scrutinee { pattern => arm_expr, ... }`. `mode` is the
    /// scrutinee's access mode: `read` (the default), `mut` (exclusive borrow
    /// — requires `match mut e`), or `own` (consume).
    Match {
        mode: PassMode,
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: Ident,
    },
    /// `receiver.method(args)` — resolved to a concrete impl method at
    /// typecheck time based on the receiver's type.
    MethodCall {
        receiver: Box<Expr>,
        method: Ident,
        args: Vec<Expr>,
        /// Per-argument passing modes for the non-receiver `args` (parallel,
        /// same length). The receiver's mode comes from the resolved callee's
        /// `self` parameter at typecheck time.
        arg_modes: Vec<PassMode>,
    },
    Dereference(Box<Expr>),
    BitNot(Box<Expr>),
    /// Unary arithmetic negation, `-operand`.
    Neg(Box<Expr>),
    /// A tuple literal, `(a, b, ...)` with two or more elements.
    Tuple(Vec<Expr>),
    /// Positional tuple access, `tuple.0`.
    TupleIndex {
        object: Box<Expr>,
        index: u32,
    },
    Array(Vec<Expr>),
    Dbg(Box<Expr>),
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Block(Block),
    /// `unsafe { stmts }` — an unsafe block granting raw-pointer powers to
    /// its body (deref, pointer arithmetic, allocator, raw I/O).
    UnsafeBlock(Block),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Multiply => "*",
            BinaryOp::Divide => "/",
            BinaryOp::Modulo => "%",
            BinaryOp::Equals => "==",
            BinaryOp::NotEquals => "!=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEquals => ">=",
            BinaryOp::Less => "<",
            BinaryOp::LessEquals => "<=",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::ShiftLeft => "<<",
            BinaryOp::ShiftRight => ">>",
        };
        write!(f, "{symbol}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

/// A pattern, used by both `let` bindings and `match` arms. Patterns
/// nest recursively. `let` accepts only the irrefutable subset
/// (wildcard, binding, and tuples thereof); `match` accepts the full
/// set.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// `_` — matches anything, binds nothing.
    Wildcard { span: Span },
    /// A bare identifier — matches anything and binds it to a name. The
    /// `mutable` flag carries `let mut` through to the binding. `mode` carries a
    /// `take` prefix in a `match` arm (moving the value out of the scrutinee);
    /// `let` bindings are always owning and leave it `View`.
    Binding {
        name: Ident,
        mutable: bool,
        mode: PassMode,
        span: Span,
    },
    /// `(a, b, ...)` — matches a tuple element-wise.
    Tuple { elems: Vec<Pattern>, span: Span },
    /// An integer literal, e.g. `0` or `-1`. `ty` carries an explicit suffix
    /// (`Undetermined` otherwise — inferred from the scrutinee at typecheck).
    Int { value: i64, ty: Type, span: Span },
    /// A boolean literal, `true` or `false`.
    Bool { value: bool, span: Span },
    /// A struct-like enum variant, e.g. `Some { value: x }`. Field
    /// sub-patterns bind the payload.
    Variant {
        enum_path: NamePath,
        variant_name: Ident,
        /// `field name → sub-pattern` pairs. Empty for unit variants.
        fields: Vec<FieldPattern>,
        span: Span,
    },
    /// A struct destructuring pattern, e.g. `Point { x, y: y2 }`. Irrefutable;
    /// may bind a subset of the struct's fields.
    Struct {
        name: Ident,
        fields: Vec<FieldPattern>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    /// Field name on the variant.
    pub field: Ident,
    /// Sub-pattern matched against the field's value. When the source
    /// writes `Some { value }`, this is a `Binding` named `value`;
    /// `Some { value: x }` makes it a `Binding` named `x`.
    pub pattern: Pattern,
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard { span }
            | Pattern::Binding { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::Int { span, .. }
            | Pattern::Bool { span, .. }
            | Pattern::Variant { span, .. }
            | Pattern::Struct { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<StructFieldDefinition>,
    pub repr_c: bool,
    /// `true` for an `@builtin type Name[...]` stub: a fieldless nominal entry
    /// whose representation is intrinsic (e.g. `Array[T, N]`). It exists to give
    /// the built-in type a home for type parameters and impls.
    pub is_builtin: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDefinition {
    pub name: Ident,
    pub field_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<VariantDefinition>,
    pub span: Span,
}

/// A single variant of an `enum`. Empty `fields` means a unit variant
/// (e.g. `None`). A struct-like variant has named fields (e.g.
/// `Some { value: T }`). A tuple variant (e.g. `Some(T)`) is desugared at
/// parse time into named fields `0`, `1`, … and flagged with `is_tuple`,
/// which selects positional construction/pattern syntax and arity errors.
#[derive(Debug, Clone, PartialEq)]
pub struct VariantDefinition {
    pub name: Ident,
    pub fields: Vec<StructFieldDefinition>,
    pub is_tuple: bool,
    pub span: Span,
}

/// A module-level `let` (or `let mut`) declaration. The initializer is
/// restricted to a literal at lower time, since wasm globals can only be
/// initialized with a constant expression.
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalDecl {
    pub name: Ident,
    pub mutable: bool,
    pub type_annotation: Type,
    pub value: Expr,
    pub span: Span,
}

/// A block of statements with an optional trailing expression.
/// The trailing expression (without semicolon) is the block's value.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>, // trailing expression (no semicolon)
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        pattern: Pattern,
        type_annotation: Option<Type>,
        value: Expr,
    },
    Assign {
        target: Ident,
        value: Expr,
    },
    /// `object.field = value` — write to a struct field (possibly nested,
    /// e.g. `a.b.c = value`, where `object` is `a.b`).
    FieldAssign {
        object: Expr,
        field: Ident,
        value: Expr,
    },
    /// `*ptr_expr = value` — write through a pointer.
    DerefAssign {
        ptr: Expr,
        value: Expr,
    },
    Expr(Expr),
    Loop {
        body: Vec<Stmt>,
        span: Span,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
        span: Span,
    },
    /// `for var in start..end { body }`. Kept as a structured node and
    /// lowered to a `while` loop during HIR building, where hidden range
    /// temporaries and the loop variable get fresh symbols that user code
    /// cannot name or shadow.
    For {
        var: Ident,
        start: Expr,
        end: Expr,
        body: Vec<Stmt>,
        span: Span,
    },
    Break {
        span: Span,
    },
    /// `return` or `return expr` — early exit from the enclosing function.
    Return {
        value: Option<Expr>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub runtime_binding: Option<String>,
    /// `@entry`: this function is the program's wasm entry point (`_start`).
    pub is_entry: bool,
    /// `unsafe fn`: this function is part of the raw core; its body is an
    /// implicit `unsafe` block, and it may only be called from an `unsafe`
    /// context (an `unsafe { ... }` block or another `unsafe fn`).
    pub unsafe_fn: bool,
    pub span: Span,
}

/// A generic function's type parameter, e.g. `T` in `fn min<T: Ord>(...)`.
/// `bound` is the optional trait the parameter is constrained by; without
/// a bound, the body may only use values of this type opaquely.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Ident,
    pub bound: Option<Ident>,
    /// `true` for a `const N: usize` value parameter, `false` for a type `T`.
    pub is_const: bool,
}

/// An `Array[T, N]` length: either a literal count or a const generic
/// parameter referenced by name.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstArg {
    Int(u64),
    Name(Ident),
}

/// How a value crosses a call boundary. A *property of the parameter/binding*,
/// not a type modifier: the value's type is unchanged in all three cases. Modes
/// are checked then erased before monomorphization.
///
/// - `Read` — shared read borrow; caller keeps ownership, callee reads only.
/// - `Mut` — exclusive mutable borrow; caller keeps ownership, callee may
///   mutate in place (visible to the caller via aliasing).
/// - `Own` — ownership transfer (move); the caller loses access.
///
/// `Read` is the default in parameter position (a bare `x: T` borrows); an owned
/// parameter is written `x: own T`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PassMode {
    #[default]
    Read,
    Mut,
    Own,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Ident,
    pub type_annotation: Type,
    /// How this parameter is passed. The keyword precedes the parameter name
    /// (`mut v: Vec[T]`); a bare parameter defaults to `View`.
    pub mode: PassMode,
}

/// Parsed AST for a single source file.
///
/// The interner is NOT carried here — there's a single program-wide one held
/// by the loader and threaded into every `parse` call. Call
/// `interner.resolve(sym)` to look up identifier text.
#[derive(Debug, Clone)]
pub struct Program {
    pub module_name: Option<Ident>,
    pub imports: Vec<ImportDecl>,
    pub structs: Vec<StructDefinition>,
    pub enums: Vec<EnumDefinition>,
    pub functions: Vec<Function>,
    pub traits: Vec<TraitDefinition>,
    pub impls: Vec<ImplDefinition>,
    pub globals: Vec<GlobalDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub raw_path: NamePath,
    pub selector: ImportSelector,
    pub trailing_symbol: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelector {
    All,
    Named(Vec<Ident>),
}

impl ImportDecl {
    /// Get module path segments as strings (resolved from interner).
    pub fn module_segments(&self, interner: &Interner) -> Vec<String> {
        self.raw_path
            .segments
            .iter()
            .map(|ident| interner.resolve(&ident.sym).to_string())
            .collect()
    }

    /// Get selector names as strings (resolved from interner).
    pub fn selector_names(&self, interner: &Interner) -> Option<Vec<String>> {
        match &self.selector {
            ImportSelector::All => None,
            ImportSelector::Named(names) => Some(
                names
                    .iter()
                    .map(|ident| interner.resolve(&ident.sym).to_string())
                    .collect(),
            ),
        }
    }
}

/// Parse a Prim source file into an AST, interning identifiers into the
/// caller-provided shared interner.
///
/// Returns (Result, diagnostics) - diagnostics are returned on both success
/// and failure.
pub fn parse<'a>(
    input: &'a str,
    interner: &'a Interner,
) -> (Result<Program, ParseError>, Vec<Diagnostic>) {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => return (Err(e.into()), Vec::new()),
    };
    let mut parser = Parser::new(tokens, input, interner);
    parser.parse()
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub name: Ident,
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDefinition {
    /// `Some` for `impl Trait for Type`; `None` for an inherent `impl Type`.
    pub trait_name: Option<Ident>,
    /// The type being implemented (a struct/enum name or a primitive).
    pub target: Type,
    pub methods: Vec<ImplMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub name: Ident,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplMethod {
    pub name: Ident,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    /// `@runtime("...")` symbol for an intrinsic associated function with no
    /// body (e.g. the primitive conversions). `None` for an ordinary method.
    pub runtime: Option<String>,
}

/// A path of name segments (e.g., `module.submodule.function`).
#[derive(Debug, Clone, PartialEq)]
pub struct NamePath {
    pub segments: Vec<Ident>,
}

impl NamePath {
    pub fn from_single(ident: Ident) -> Self {
        Self {
            segments: vec![ident],
        }
    }
}
