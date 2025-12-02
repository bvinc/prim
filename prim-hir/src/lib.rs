use std::collections::HashMap;
use std::path::PathBuf;

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
    pub files: Vec<FileInfo>,
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
    pub fields: Vec<HirField>,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct HirGlobal {
    pub id: GlobalId,
    pub name: SymbolId,
    pub module: ModuleId,
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
    pub name: SymbolId,
    pub ty: Type,
    pub span: SpanId,
}

#[derive(Clone, Debug)]
pub struct HirBlock {
    pub stmts: Vec<HirStmt>,
}

#[derive(Clone, Debug)]
pub enum HirStmt {
    Let {
        name: SymbolId,
        ty: Type,
        value: HirExpr,
        span: SpanId,
    },
    Expr(HirExpr),
    Loop {
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
        fields: Vec<(SymbolId, HirExpr)>,
        ty: Type,
        span: SpanId,
    },
    Field {
        base: Box<HirExpr>,
        field: SymbolId,
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
}

#[derive(Clone, Debug)]
pub struct FileInfo {
    pub id: FileId,
    pub path: PathBuf,
    pub source: String,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub entries: Vec<Symbol>,
    pub by_name: HashMap<(ModuleId, String), SymbolId>,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub id: SymbolId,
    pub module: ModuleId,
    pub name: String,
    pub kind: SymbolKind,
}

#[derive(Clone, Debug)]
pub enum SymbolKind {
    Function(FuncId),
    Struct(StructId),
    Global(GlobalId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
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
    StrSlice,
    Array(Box<Type>),
    Struct(StructId),
    Pointer { mutable: bool, pointee: Box<Type> },
}
