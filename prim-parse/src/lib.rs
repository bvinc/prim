use prim_tok::Tokenizer;
use string_interner::StringInterner;
use string_interner::backend::BufferBackend;
use string_interner::symbol::SymbolU32;

mod error;
pub use error::{Diagnostic, ParseError, Severity};

pub use prim_tok::Span;

mod number;
mod parser;
use parser::Parser;

type InternSymbol = SymbolU32;
type Interner = StringInterner<BufferBackend<InternSymbol>>;

#[derive(Debug, Clone, PartialEq)]
pub enum PointerMutability {
    Const,
    Mutable,
}

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
    Struct(Span), // struct name reference
    Pointer {
        mutability: PointerMutability,
        pointee: Box<Type>,
    },
    Undetermined, // Type not yet determined during parsing
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    IntLiteral {
        span: Span,
        value: i64,
        ty: Type,
    },
    FloatLiteral {
        span: Span,
        value: f64,
        ty: Type,
    },
    BoolLiteral {
        span: Span,
        value: bool,
        ty: Type,
    },
    StringLiteral {
        span: Span,
        value: String,
        ty: Type,
    },
    Identifier {
        span: Span,
        ty: Type,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
        span: Span,
        ty: Type,
    },
    FunctionCall {
        // Qualified name segments: e.g., module.function or just function
        path: NamePath,
        args: Vec<Expr>,
        ty: Type,
    },
    StructLiteral {
        name: Span,
        fields: Vec<StructField>,
        ty: Type,
    },
    FieldAccess {
        object: Box<Expr>,
        field: Span,
        ty: Type,
    },
    Dereference {
        operand: Box<Expr>,
        span: Span,
        ty: Type,
    },
    ArrayLiteral {
        elements: Vec<Expr>,
        span: Span,
        ty: Type,
    },
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
        span: Span,
        ty: Type,
    },
}

impl Expr {
    pub fn resolved_type(&self) -> &Type {
        match self {
            Expr::IntLiteral { ty, .. }
            | Expr::FloatLiteral { ty, .. }
            | Expr::BoolLiteral { ty, .. }
            | Expr::StringLiteral { ty, .. }
            | Expr::Identifier { ty, .. }
            | Expr::Binary { ty, .. }
            | Expr::FunctionCall { ty, .. }
            | Expr::StructLiteral { ty, .. }
            | Expr::FieldAccess { ty, .. }
            | Expr::Dereference { ty, .. }
            | Expr::ArrayLiteral { ty, .. }
            | Expr::If { ty, .. } => ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::IntLiteral { span, .. }
            | Expr::FloatLiteral { span, .. }
            | Expr::BoolLiteral { span, .. }
            | Expr::StringLiteral { span, .. }
            | Expr::Identifier { span, .. }
            | Expr::Binary { span, .. }
            | Expr::StructLiteral { name: span, .. }
            | Expr::FieldAccess { field: span, .. }
            | Expr::Dereference { span, .. }
            | Expr::ArrayLiteral { span, .. }
            | Expr::If { span, .. } => *span,
            Expr::FunctionCall { path, args, .. } => {
                let first = *path.segments.first().expect("path must have segments");
                let last = *path.segments.last().unwrap();
                let mut span = first.cover(last);
                if let Some(last_arg) = args.last() {
                    span = span.cover(last_arg.span());
                }
                span
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Span,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: Span,
    pub fields: Vec<StructFieldDefinition>,
    pub repr_c: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDefinition {
    pub name: Span,
    pub field_type: Type,
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
        name: Span,
        mutable: bool,
        type_annotation: Option<Type>,
        value: Expr,
    },
    Assign {
        target: Span,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: InternSymbol,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub runtime_binding: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Span,
    pub type_annotation: Type,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub module_name: Option<Span>,
    pub imports: Vec<ImportDecl>,
    pub structs: Vec<StructDefinition>,
    pub functions: Vec<Function>,
    pub traits: Vec<TraitDefinition>,
    pub impls: Vec<ImplDefinition>,
    pub interner: Interner,
}

impl Program {
    pub fn resolve(&self, symbol: InternSymbol) -> &str {
        self.interner
            .resolve(symbol)
            .expect("missing interned symbol")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub raw_path: NamePath,
    pub selector: ImportSelector,
    pub trailing_symbol: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelector {
    All,
    Named(Vec<Span>),
}

impl ImportDecl {
    pub fn module_segments<'a>(&'a self, source: &'a str) -> Vec<String> {
        self.raw_path
            .segments
            .iter()
            .map(|s| s.text(source).to_string())
            .collect()
    }

    pub fn selector_names<'a>(&'a self, source: &'a str) -> Option<Vec<String>> {
        match &self.selector {
            ImportSelector::All => None,
            ImportSelector::Named(names) => Some(
                names
                    .iter()
                    .map(|span| span.text(source).to_string())
                    .collect(),
            ),
        }
    }
}

/// Parse a Prim source file into an AST.
/// Returns (Result, diagnostics) - diagnostics are returned on both success and failure.
pub fn parse(input: &str) -> (Result<Program, ParseError>, Vec<Diagnostic>) {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => return (Err(e.into()), Vec::new()),
    };
    let mut parser = Parser::new(tokens, input);
    parser.parse()
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub name: Span,
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDefinition {
    pub trait_name: Span,
    pub struct_name: Span,
    pub methods: Vec<ImplMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub name: InternSymbol,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplMethod {
    pub name: InternSymbol,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamePath {
    pub segments: Vec<Span>,
}

impl NamePath {
    pub fn from_single(seg: Span) -> Self {
        Self {
            segments: vec![seg],
        }
    }
}
