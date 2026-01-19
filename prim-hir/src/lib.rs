use prim_tok::Span;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

pub use prim_util::{InternSymbol, Interner};

pub mod typecheck;
pub use typecheck::{TypeCheckError, TypeCheckKind, type_check};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StructId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpanId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Clone, Debug)]
pub struct HirProgram {
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
    pub exports: Vec<SymbolId>,
    pub imports: Vec<ResolvedImport>,
}

#[derive(Clone, Debug)]
pub struct ResolvedImport {
    pub from: ModuleId,
    pub symbols: Vec<SymbolId>,
}

#[derive(Clone, Debug, Default)]
pub struct Items {
    pub functions: Vec<HirFunction>,
    pub structs: Vec<HirStruct>,
    pub globals: Vec<HirGlobal>,
}

#[derive(Clone, Debug)]
pub struct HirFunction {
    pub id: FuncId,
    pub name: SymbolId,
    pub module: ModuleId,
    pub file: FileId,
    pub params: Vec<HirParam>,
    pub ret: Option<Type>,
    pub body: HirBlock,
    pub span: SpanId,
    pub runtime_binding: Option<String>,
}

#[derive(Clone, Debug)]
pub struct HirStruct {
    pub id: StructId,
    pub name: SymbolId,
    pub module: ModuleId,
    pub file: FileId,
    pub fields: Vec<HirField>,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct HirGlobal {
    pub id: GlobalId,
    pub name: SymbolId,
    pub module: ModuleId,
    pub file: FileId,
    pub ty: Type,
    pub init: HirExpr,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct HirParam {
    pub name: SymbolId,
    pub ty: Type,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct HirField {
    pub name: InternSymbol,
    pub ty: Type,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct HirBlock {
    pub stmts: Vec<HirStmt>,
    /// Trailing expression (without semicolon) - the block's value.
    pub expr: Option<Box<HirExpr>>,
}

#[derive(Clone, Debug)]
pub enum HirStmt {
    Let {
        name: SymbolId,
        mutable: bool,
        ty: Type,
        value: HirExpr,
        span: SpanId,
    },
    Assign {
        target: SymbolId,
        value: HirExpr,
        span: SpanId,
    },
    Expr(HirExpr),
    Loop {
        body: HirBlock,
        span: SpanId,
    },
    While {
        condition: HirExpr,
        body: HirBlock,
        span: SpanId,
    },
    Break {
        span: SpanId,
    },
}

#[derive(Clone, Debug)]
pub enum HirExpr {
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
        left: Box<HirExpr>,
        right: Box<HirExpr>,
        ty: Type,
        span: SpanId,
    },
    Call {
        func: FuncId,
        args: Vec<HirExpr>,
        ty: Type,
        span: SpanId,
    },
    StructLit {
        struct_id: StructId,
        fields: Vec<(InternSymbol, HirExpr)>,
        ty: Type,
        span: SpanId,
    },
    Field {
        base: Box<HirExpr>,
        field: InternSymbol,
        ty: Type,
        span: SpanId,
    },
    Deref {
        base: Box<HirExpr>,
        ty: Type,
        span: SpanId,
    },
    ArrayLit {
        elements: Vec<HirExpr>,
        ty: Type,
        span: SpanId,
    },
    If {
        condition: Box<HirExpr>,
        then_branch: HirBlock,
        else_branch: Option<HirBlock>,
        ty: Type,
        span: SpanId,
    },
    Block {
        block: HirBlock,
        ty: Type,
        span: SpanId,
    },
}

impl HirExpr {
    pub fn ty(&self) -> &Type {
        match self {
            HirExpr::Int { ty, .. }
            | HirExpr::Float { ty, .. }
            | HirExpr::Bool { ty, .. }
            | HirExpr::Str { ty, .. }
            | HirExpr::Ident { ty, .. }
            | HirExpr::Binary { ty, .. }
            | HirExpr::Call { ty, .. }
            | HirExpr::StructLit { ty, .. }
            | HirExpr::Field { ty, .. }
            | HirExpr::Deref { ty, .. }
            | HirExpr::ArrayLit { ty, .. }
            | HirExpr::If { ty, .. }
            | HirExpr::Block { ty, .. } => ty,
        }
    }

    pub fn span(&self) -> SpanId {
        match self {
            HirExpr::Int { span, .. }
            | HirExpr::Float { span, .. }
            | HirExpr::Bool { span, .. }
            | HirExpr::Str { span, .. }
            | HirExpr::Ident { span, .. }
            | HirExpr::Binary { span, .. }
            | HirExpr::Call { span, .. }
            | HirExpr::StructLit { span, .. }
            | HirExpr::Field { span, .. }
            | HirExpr::Deref { span, .. }
            | HirExpr::ArrayLit { span, .. }
            | HirExpr::If { span, .. }
            | HirExpr::Block { span, .. } => *span,
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
    Global(GlobalId),
    Param,
    Local,
    Field,
    Trait,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        };
        write!(f, "{symbol}")
    }
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
