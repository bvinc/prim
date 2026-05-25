pub use prim_parse::{BinaryOp, InternSymbol, Interner};
pub use prim_tok::{FileId, ModuleId, Span};
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

pub mod typecheck;
pub use typecheck::{TypeCheckError, TypeCheckKind, type_check};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StructId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpanId(pub u32);

#[derive(Clone, Debug)]
pub struct Program {
    pub modules: Vec<Module>,
    pub items: Items,
    pub symbols: SymbolTable,
    pub interner: Interner,
    pub main: Option<SymbolId>,
    pub files: Vec<FileInfo>,
    pub spans: Vec<(FileId, Span)>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: ModuleId,
    pub name: Vec<String>,
    pub files: Vec<FileId>,
}

#[derive(Clone, Debug, Default)]
pub struct Items {
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: FuncId,
    pub name: SymbolId,
    pub module: ModuleId,
    pub file: FileId,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
    pub body: Block,
    pub span: SpanId,
    pub runtime_binding: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: StructId,
    pub name: SymbolId,
    pub module: ModuleId,
    pub file: FileId,
    pub fields: Vec<Field>,
    pub span: SpanId,
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
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int {
        value: i64,
        ty: Type,
        span: SpanId,
    },
    Float {
        value: f64,
        ty: Type,
        span: SpanId,
    },
    Bool {
        value: bool,
        ty: Type,
        span: SpanId,
    },
    Str {
        value: String,
        ty: Type,
        span: SpanId,
    },
    Ident {
        symbol: SymbolId,
        ty: Type,
        span: SpanId,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Type,
        span: SpanId,
    },
    Call {
        func: FuncId,
        args: Vec<Expr>,
        ty: Type,
        span: SpanId,
    },
    StructLit {
        struct_id: StructId,
        fields: Vec<(InternSymbol, Expr)>,
        ty: Type,
        span: SpanId,
    },
    Field {
        base: Box<Expr>,
        field: InternSymbol,
        ty: Type,
        span: SpanId,
    },
    Deref {
        base: Box<Expr>,
        ty: Type,
        span: SpanId,
    },
    ArrayLit {
        elements: Vec<Expr>,
        ty: Type,
        span: SpanId,
    },
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
        ty: Type,
        span: SpanId,
    },
    Block {
        block: Block,
        ty: Type,
        span: SpanId,
    },
    /// Placeholder for expressions that failed during lowering.
    /// Errors are already recorded; this node prevents cascading failures.
    Error {
        span: SpanId,
    },
}

impl Expr {
    pub fn ty(&self) -> &Type {
        match self {
            Expr::Int { ty, .. }
            | Expr::Float { ty, .. }
            | Expr::Bool { ty, .. }
            | Expr::Str { ty, .. }
            | Expr::Ident { ty, .. }
            | Expr::Binary { ty, .. }
            | Expr::Call { ty, .. }
            | Expr::StructLit { ty, .. }
            | Expr::Field { ty, .. }
            | Expr::Deref { ty, .. }
            | Expr::ArrayLit { ty, .. }
            | Expr::If { ty, .. }
            | Expr::Block { ty, .. } => ty,
            Expr::Error { .. } => &Type::Undetermined,
        }
    }

    pub fn span(&self) -> SpanId {
        match self {
            Expr::Int { span, .. }
            | Expr::Float { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Str { span, .. }
            | Expr::Ident { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Call { span, .. }
            | Expr::StructLit { span, .. }
            | Expr::Field { span, .. }
            | Expr::Deref { span, .. }
            | Expr::ArrayLit { span, .. }
            | Expr::If { span, .. }
            | Expr::Block { span, .. }
            | Expr::Error { span } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FileInfo {
    pub id: FileId,
    pub path: PathBuf,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub entries: Vec<Symbol>,
    pub by_name: HashMap<(ModuleId, InternSymbol), SymbolId>,
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
    Param,
    Local,
    Field,
    Trait,
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
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
    Struct(StructId),
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
            Type::Struct(id) => Some(*id),
            _ => None,
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
            Type::Struct(id) => write!(f, "struct {:?}", id),
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
