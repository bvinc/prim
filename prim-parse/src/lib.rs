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
    Array(Box<Type>),
    /// Named type — either a struct or a trait. The optional type
    /// argument list lets generic instantiations like `Pair<i32>` appear
    /// in type positions (function params, return, let bindings). Empty
    /// vec means a plain name with no generic args.
    Struct(InternSymbol, Vec<Type>),
    Pointer {
        mutable: bool,
        pointee: Box<Type>,
    },
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
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    FunctionCall {
        path: NamePath,
        args: Vec<Expr>,
    },
    StructLiteral {
        name: Ident,
        fields: Vec<StructField>,
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
    },
    Dereference(Box<Expr>),
    BitNot(Box<Expr>),
    Array(Vec<Expr>),
    Dbg(Box<Expr>),
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Block(Block),
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
pub struct StructDefinition {
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<StructFieldDefinition>,
    pub repr_c: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDefinition {
    pub name: Ident,
    pub field_type: Type,
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
        name: Ident,
        mutable: bool,
        type_annotation: Option<Type>,
        value: Expr,
    },
    Assign {
        target: Ident,
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
    pub span: Span,
}

/// A generic function's type parameter, e.g. `T` in `fn min<T: Ord>(...)`.
/// `bound` is the optional trait the parameter is constrained by; without
/// a bound, the body may only use values of this type opaquely.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Ident,
    pub bound: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Ident,
    pub type_annotation: Type,
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
    pub trait_name: Ident,
    pub struct_name: Ident,
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
