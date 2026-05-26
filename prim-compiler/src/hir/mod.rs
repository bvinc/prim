pub use prim_parse::{BinaryOp, InternSymbol, Interner};
pub use prim_tok::{FileId, ModuleId, Span};
use std::fmt;

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
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
    pub symbols: Vec<Symbol>,
    pub interner: Interner,
    pub main: Option<SymbolId>,
    pub spans: Vec<(FileId, Span)>,
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
        args: Vec<Expr>,
    },
    StructLit {
        struct_id: StructId,
        fields: Vec<(InternSymbol, Expr)>,
    },
    Field {
        base: Box<Expr>,
        field: InternSymbol,
    },
    Deref(Box<Expr>),
    ArrayLit(Vec<Expr>),
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
