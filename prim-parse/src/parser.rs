use crate::number::{parse_float_literal, parse_int_literal};
use crate::{
    BinaryOp, Block, ConstArg, Diagnostic, Expr, ExprKind, Function, GlobalDecl, Ident, ImportDecl,
    ImportSelector, Interner, NamePath, Parameter, ParseError, PassMode, Program, RefKind,
    Severity, Span, Stmt, StructDefinition, StructField, StructFieldDefinition, Type,
};
use prim_tok::{Token, TokenKind};

/// Precedence levels for operators (higher = tighter binding)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence(pub i32);

impl Precedence {
    pub const NONE: Precedence = Precedence(0);
    pub const LOGICAL_OR: Precedence = Precedence(4); // ||
    pub const LOGICAL_AND: Precedence = Precedence(6); // &&
    pub const EQUALITY: Precedence = Precedence(10); // == !=
    pub const COMPARISON: Precedence = Precedence(15); // > < >= <=
    // Bitwise operators bind TIGHTER than comparison/equality — Rust-style,
    // not C-style. `mask & 0xF == 0` parses as `(mask & 0xF) == 0`, NOT
    // `mask & (0xF == 0)` (the famous C footgun).
    pub const BIT_OR: Precedence = Precedence(16); // |
    pub const BIT_XOR: Precedence = Precedence(17); // ^
    pub const BIT_AND: Precedence = Precedence(18); // &
    pub const SHIFT: Precedence = Precedence(19); // << >>
    pub const ADDITION: Precedence = Precedence(20); // + -
    pub const MULTIPLICATION: Precedence = Precedence(30); // * /
    pub const UNARY: Precedence = Precedence(40); // -x !x
    pub const CALL: Precedence = Precedence(50); // func()
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    source: &'a str,
    module_name: Option<Ident>,
    interner: &'a Interner,
    diagnostics: Vec<Diagnostic>,
    /// Whether struct literals are allowed in the current expression context.
    /// Disabled when parsing if/while conditions to avoid ambiguity with block braces.
    allow_struct_literal: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str, interner: &'a Interner) -> Self {
        // Filter out comment tokens so the parser never sees them
        let tokens = tokens
            .into_iter()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();

        Self {
            tokens,
            current: 0,
            source,
            module_name: None,
            interner,
            diagnostics: Vec::new(),
            allow_struct_literal: true,
        }
    }

    pub fn parse(&mut self) -> (Result<Program, ParseError>, Vec<Diagnostic>) {
        let result = self.parse_internal();
        self.finalize(result)
    }

    fn finalize(
        &mut self,
        result: Result<Program, ParseError>,
    ) -> (Result<Program, ParseError>, Vec<Diagnostic>) {
        let diagnostics = std::mem::take(&mut self.diagnostics);
        let has_errors = diagnostics.iter().any(|d| d.severity == Severity::Error);

        if result.is_ok() && has_errors {
            return (Err(ParseError::HasErrors), diagnostics);
        }

        (result, diagnostics)
    }

    fn emit(&mut self, message: impl Into<String>, span: Span, severity: Severity) {
        self.diagnostics.push(Diagnostic {
            message: message.into(),
            span,
            severity,
        });
    }

    /// Intern a string from a span, returning just the symbol.
    fn intern(&self, span: Span) -> crate::InternSymbol {
        let text = span.text(self.source);
        self.interner.get_or_intern(text)
    }

    /// Create an identifier from a span.
    fn ident(&self, span: Span) -> Ident {
        Ident {
            sym: self.intern(span),
            span,
        }
    }

    fn parse_internal(&mut self) -> Result<Program, ParseError> {
        let mut structs = Vec::new();
        let mut enums = Vec::new();
        let mut functions = Vec::new();
        let mut traits = Vec::new();
        let mut impls = Vec::new();
        let mut imports: Vec<ImportDecl> = Vec::new();
        let mut globals: Vec<GlobalDecl> = Vec::new();

        // Optional module header: mod <identifier>
        if matches!(self.peek_kind(), Some(TokenKind::Mod)) {
            self.advance(); // consume 'mod'
            let span = self
                .consume(TokenKind::Identifier, "Expected module name after 'mod'")?
                .span;
            self.module_name = Some(self.ident(span));
            self.consume_optional_semicolon();
        }

        // Optional imports with optional selectors
        while matches!(self.peek_kind(), Some(TokenKind::Import)) {
            self.advance(); // consume 'import'
            let head_span = self
                .consume(TokenKind::Identifier, "Expected module name after 'import'")?
                .span;
            let head_ident = self.ident(head_span);
            let mut segments = vec![head_ident];
            let mut selector = ImportSelector::All;
            let mut trailing_symbol: Option<Ident> = None;

            loop {
                if !matches!(self.peek_kind(), Some(TokenKind::Dot)) {
                    break;
                }
                self.advance(); // consume '.'
                match self.peek_kind() {
                    Some(TokenKind::LeftBrace) => {
                        self.advance(); // consume '{'
                        let mut names = Vec::new();
                        loop {
                            let name_span = self
                                .consume(
                                    TokenKind::Identifier,
                                    "Expected identifier inside import braces",
                                )?
                                .span;
                            names.push(self.ident(name_span));
                            if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                                self.advance();
                                continue;
                            }
                            break;
                        }
                        self.consume(TokenKind::RightBrace, "Expected '}' to close import list")?;
                        selector = ImportSelector::Named(names);
                        trailing_symbol = None;
                        break;
                    }
                    Some(TokenKind::Identifier) => {
                        let seg_span = self.advance().span;
                        let seg_ident = self.ident(seg_span);
                        segments.push(seg_ident);
                        trailing_symbol = if segments.len() >= 2 {
                            Some(seg_ident)
                        } else {
                            None
                        };
                    }
                    Some(other) => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "identifier or '{' after '.' in import".to_string(),
                            found: other,
                            span: self.current_span(),
                        });
                    }
                    None => {
                        return Err(ParseError::UnexpectedEof {
                            span: self.current_span(),
                        });
                    }
                }
            }

            imports.push(ImportDecl {
                raw_path: NamePath { segments },
                selector,
                trailing_symbol,
            });
            self.consume_optional_semicolon();
        }

        while !self.is_at_end() {
            // Collect any leading attributes
            let attrs = self.parse_attributes()?;

            // Allow struct definitions and function definitions at the top level
            match self.peek_kind() {
                Some(TokenKind::Struct) => {
                    let struct_def = self.parse_struct_with_attrs(attrs)?;
                    structs.push(struct_def);
                }
                Some(TokenKind::Enum) => {
                    let enum_def = self.parse_enum_definition()?;
                    enums.push(enum_def);
                }
                Some(TokenKind::Fn) => {
                    let function = self.parse_function_with_attrs(attrs)?;
                    functions.push(function);
                }
                Some(TokenKind::Trait) => {
                    let tr = self.parse_trait_definition()?;
                    traits.push(tr);
                }
                Some(TokenKind::Type) => {
                    let ty = self.parse_builtin_type(attrs)?;
                    structs.push(ty);
                }
                Some(TokenKind::Impl) => {
                    let im = self.parse_impl_definition()?;
                    impls.push(im);
                }
                Some(TokenKind::Let) => {
                    let g = self.parse_global_decl()?;
                    globals.push(g);
                }
                _ => {
                    return Err(ParseError::StatementsOutsideFunction {
                        span: self.current_span(),
                    });
                }
            }
        }

        Ok(Program {
            module_name: self.module_name,
            imports,
            structs,
            enums,
            functions,
            traits,
            impls,
            globals,
        })
    }

    /// Temporarily disable struct literals and run the given closure.
    /// Used for parsing if/while conditions where `x { }` should not be a struct literal.
    fn without_struct_literals<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.allow_struct_literal;
        self.allow_struct_literal = false;
        let result = f(self);
        self.allow_struct_literal = prev;
        result
    }

    /// Parse an expression with minimum precedence
    pub fn parse_expression(&mut self, min_precedence: Precedence) -> Result<Expr, ParseError> {
        // Parse prefix expression
        let mut left = self.parse_prefix()?;

        // Parse infix expressions while precedence is sufficient
        while self.next_infix_precedence() > min_precedence {
            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    /// Parse a prefix expression (literals, identifiers, unary operators, grouping)
    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEof {
                span: self.current_span(),
            });
        }
        // A primitive type keyword is only valid in expression position as the
        // head of an associated call, e.g. `u8.from_i32(x)`. Lower it to an
        // identifier so the path/call machinery resolves it like `Type.f(..)`.
        if self.peek_is_primitive_type() && self.peek_kind_at(1) == Some(TokenKind::Dot) {
            let span = self.advance().span;
            return Ok(Expr {
                span,
                ty: Type::Undetermined,
                kind: ExprKind::Ident(self.ident(span)),
            });
        }
        // Borrow expressions: `read place` / `mut place`.
        if let Some(rk) = match self.peek_kind() {
            Some(TokenKind::Read) => Some(RefKind::Read),
            Some(TokenKind::Mut) => Some(RefKind::Mut),
            _ => None,
        } {
            let kw = self.advance().span;
            let place = self.parse_prefix()?;
            let span = kw.cover(place.span);
            return Ok(Expr {
                span,
                ty: Type::Undetermined,
                kind: ExprKind::Borrow {
                    kind: rk,
                    place: Box::new(place),
                },
            });
        }
        match self.peek_kind() {
            Some(TokenKind::IntLiteral) => {
                let span = self.advance().span;
                let literal_text = span.text(self.source).to_string();
                let (value, ty) = parse_int_literal(&literal_text, span)?;
                Ok(Expr {
                    span,
                    ty,
                    kind: ExprKind::Int(value),
                })
            }
            Some(TokenKind::StringLiteral) => {
                let span = self.advance().span;
                let value = Self::unescape_string_literal(span.text(self.source));
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::String(value),
                })
            }
            Some(TokenKind::FloatLiteral) => {
                let span = self.advance().span;
                let literal_text = span.text(self.source).to_string();
                let (value, ty) = parse_float_literal(&literal_text, span)?;
                Ok(Expr {
                    span,
                    ty,
                    kind: ExprKind::Float(value),
                })
            }
            Some(TokenKind::True) => {
                let span = self.advance().span;
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Bool(true),
                })
            }
            Some(TokenKind::False) => {
                let span = self.advance().span;
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Bool(false),
                })
            }
            Some(TokenKind::Identifier) => {
                let span = self.advance().span;
                let ident = self.ident(span);

                // Check if this is a function call. The `(` must be glued to
                // the name (no whitespace) — `f(x)` is a call, `f (x)` and a
                // `(` on the next line are not, so a line can't be silently
                // absorbed as a call of the previous one.
                if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) && self.glued_to_prev() {
                    self.advance(); // consume '('
                    let (args, arg_modes) = self.parse_argument_list()?;
                    let end_span = self.consume(TokenKind::RightParen, "Expected ')'")?;
                    let span = ident.span.cover(end_span.span);
                    Ok(Expr {
                        span,
                        ty: Type::Undetermined,
                        kind: ExprKind::FunctionCall {
                            path: NamePath::from_single(ident),
                            args,
                            arg_modes,
                            type_args: Vec::new(),
                        },
                    })
                } else if self.allow_struct_literal
                    && matches!(self.peek_kind(), Some(TokenKind::LeftBrace))
                {
                    // This is a struct literal
                    self.advance(); // consume '{'
                    let fields = self.parse_struct_literal_fields()?;
                    let end_span = self.consume(TokenKind::RightBrace, "Expected '}'")?;
                    let span = ident.span.cover(end_span.span);
                    Ok(Expr {
                        span,
                        ty: Type::Undetermined,
                        kind: ExprKind::StructLiteral {
                            name: ident,
                            fields,
                        },
                    })
                } else {
                    Ok(Expr {
                        span: ident.span,
                        ty: Type::Undetermined,
                        kind: ExprKind::Ident(ident),
                    })
                }
            }
            Some(TokenKind::LeftParen) => {
                let left_span = self.advance().span; // consume '('
                let first = self.parse_expression(Precedence::NONE)?;
                if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                    // Tuple literal: (a, b, ...). One or more commas follow.
                    let mut elements = vec![first];
                    while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                        self.advance(); // consume ','
                        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                            break; // trailing comma
                        }
                        elements.push(self.parse_expression(Precedence::NONE)?);
                    }
                    let right_span = self
                        .consume(TokenKind::RightParen, "Expected ')' to close tuple")?
                        .span;
                    Ok(Expr {
                        span: left_span.cover(right_span),
                        ty: Type::Undetermined,
                        kind: ExprKind::Tuple(elements),
                    })
                } else {
                    // Plain parenthesized grouping.
                    self.consume(TokenKind::RightParen, "Expected ')'")?;
                    Ok(first)
                }
            }
            Some(TokenKind::LeftBracket) => {
                // Array literal: [expr, expr, ...]
                let left_span = self.advance().span; // consume '['
                let mut elements = Vec::new();
                // Allow empty literal []
                if !matches!(self.peek_kind(), Some(TokenKind::RightBracket)) {
                    elements.push(self.parse_expression(Precedence::NONE)?);
                    while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                        self.advance(); // consume ','
                        elements.push(self.parse_expression(Precedence::NONE)?);
                    }
                }
                let right_span = self
                    .consume(
                        TokenKind::RightBracket,
                        "Expected ']' to close array literal",
                    )?
                    .span;
                let span = left_span.cover(right_span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Array(elements),
                })
            }
            // Both UnaryMinus (tokenized when whitespace makes it unambiguous)
            // and Minus (tokenized as infix subtract) act as negation in prefix
            // position — the parser knows it's expecting an expression.
            // Both UnaryMinus (tokenized when whitespace makes it unambiguous)
            // and Minus (tokenized as infix subtract) act as negation in prefix
            // position — the parser knows it's expecting an expression.
            Some(TokenKind::UnaryMinus) | Some(TokenKind::Minus) => {
                let minus_span = self.advance().span; // consume '-'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = minus_span.cover(operand.span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Neg(Box::new(operand)),
                })
            }
            // Unary plus is the identity on a numeric value: just parse and
            // return the operand.
            Some(TokenKind::UnaryPlus) | Some(TokenKind::Plus) => {
                self.advance(); // consume '+'
                self.parse_expression(Precedence::UNARY)
            }
            Some(TokenKind::Bang) => {
                let bang_span = self.advance().span; // consume '!'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = bang_span.cover(operand.span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::BitNot(Box::new(operand)),
                })
            }
            // Both UnaryStar (tokenized when whitespace makes it unambiguous)
            // and Star (tokenized as infix multiply) act as dereference in
            // prefix position — the parser knows it's expecting an expression.
            Some(TokenKind::UnaryStar) | Some(TokenKind::Star) => {
                let star_span = self.advance().span; // consume '*'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = star_span.cover(operand.span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Dereference(Box::new(operand)),
                })
            }
            Some(TokenKind::If) => self.parse_if_expression(),
            Some(TokenKind::Match) => self.parse_match_expression(),
            Some(TokenKind::LeftBrace) => {
                // Block expression: { stmts; expr }
                let block = self.parse_block()?;
                let span = block.span;
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Block(block),
                })
            }
            Some(TokenKind::At) => self.parse_expr_attribute(),
            Some(kind) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: kind,
                span: self.current_span(),
            }),
            None => Err(ParseError::UnexpectedEof {
                span: self.current_span(),
            }),
        }
    }

    /// Parse an infix expression (binary operators, function calls, field access)
    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let Some(kind) = self.peek_kind() else {
            return Ok(left);
        };

        // Short-circuit `&&` / `||` desugar to `if`, so the right operand is
        // only evaluated when needed: `a && b` → `if a { b } else { false }`,
        // `a || b` → `if a { true } else { b }`.
        if matches!(kind, TokenKind::AmpAmp | TokenKind::PipePipe) {
            let precedence = get_precedence_for_token(kind);
            self.advance();
            let right = self.parse_expression(precedence)?;
            let span = left.span.cover(right.span);
            let block = |e: Expr| Block {
                span: e.span,
                stmts: Vec::new(),
                expr: Some(Box::new(e)),
            };
            let bool_lit = |b: bool| Expr {
                span,
                ty: Type::Undetermined,
                kind: ExprKind::Bool(b),
            };
            let (then_branch, else_branch) = if kind == TokenKind::AmpAmp {
                (block(right), block(bool_lit(false)))
            } else {
                (block(bool_lit(true)), block(right))
            };
            return Ok(Expr {
                span,
                ty: Type::Undetermined,
                kind: ExprKind::If {
                    condition: Box::new(left),
                    then_branch,
                    else_branch: Some(else_branch),
                },
            });
        }

        // Binary operators
        if let Some(binary_op) = token_to_binary_op(kind) {
            let precedence = get_precedence_for_token(kind);
            self.advance();
            let right = self.parse_expression(precedence)?;
            let span = left.span.cover(right.span);
            return Ok(Expr {
                span,
                ty: Type::Undetermined,
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: binary_op,
                    right: Box::new(right),
                },
            });
        }

        match kind {
            TokenKind::LeftParen => {
                // Function call: identifier(args) or qualified path(args).
                // Lowering decides whether a two-segment local path like
                // `value.method(args)` is a method call.
                let path = match Self::expr_to_path(&left) {
                    Some(path) => path,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "function name".to_string(),
                            found: kind,
                            span: self.current_span(),
                        });
                    }
                };
                self.advance();
                let (args, arg_modes) = self.parse_argument_list()?;
                let end_span = self.consume(TokenKind::RightParen, "Expected ')'")?;
                let span = left.span.cover(end_span.span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::FunctionCall {
                        path,
                        args,
                        arg_modes,
                        type_args: Vec::new(),
                    },
                })
            }
            // Turbofish call: `path[T1, T2](args)`. Only a path can carry
            // type arguments; the bracket list must be followed by a call.
            TokenKind::LeftBracket => {
                let Some(path) = Self::expr_to_path(&left) else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "a function name before '['".to_string(),
                        found: kind,
                        span: self.current_span(),
                    });
                };
                self.advance(); // consume '['
                let type_args = self.parse_type_arg_list()?;
                // The call args must be glued to the `]`: `f[T](x)`, never
                // `f[T] (x)` or a `(` on the next line.
                if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) && !self.glued_to_prev() {
                    return Err(ParseError::UnexpectedToken {
                        expected: "'(' immediately after type arguments (no space)".to_string(),
                        found: TokenKind::LeftParen,
                        span: self.current_span(),
                    });
                }
                self.consume(TokenKind::LeftParen, "Expected '(' after type arguments")?;
                let (args, arg_modes) = self.parse_argument_list()?;
                let end_span = self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr {
                    span: left.span.cover(end_span.span),
                    ty: Type::Undetermined,
                    kind: ExprKind::FunctionCall {
                        path,
                        args,
                        arg_modes,
                        type_args,
                    },
                })
            }
            TokenKind::Dot => {
                self.advance();
                // `tuple.0` — positional tuple access.
                if matches!(self.peek_kind(), Some(TokenKind::IntLiteral)) {
                    let idx_span = self.advance().span;
                    let (value, _ty) = parse_int_literal(idx_span.text(self.source), idx_span)?;
                    return Ok(Expr {
                        span: left.span.cover(idx_span),
                        ty: Type::Undetermined,
                        kind: ExprKind::TupleIndex {
                            object: Box::new(left),
                            index: value as u32,
                        },
                    });
                }
                let name_span = self
                    .consume(
                        TokenKind::Identifier,
                        "Expected field or method name after '.'",
                    )?
                    .span;
                let name = self.ident(name_span);
                // `(` after the name → method call; otherwise field access.
                if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
                    self.advance(); // consume '('
                    let (args, arg_modes) = self.parse_argument_list()?;
                    let close =
                        self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
                    let span = left.span.cover(close.span);
                    if let Some(mut path) = Self::expr_to_path(&left) {
                        path.segments.push(name);
                        return Ok(Expr {
                            span,
                            ty: Type::Undetermined,
                            kind: ExprKind::FunctionCall {
                                path,
                                args,
                                arg_modes,
                                type_args: Vec::new(),
                            },
                        });
                    }
                    Ok(Expr {
                        span,
                        ty: Type::Undetermined,
                        kind: ExprKind::MethodCall {
                            receiver: Box::new(left),
                            method: name,
                            args,
                            arg_modes,
                        },
                    })
                } else {
                    let span = left.span.cover(name.span);
                    if let Some(mut path) = Self::expr_to_path(&left) {
                        path.segments.push(name);
                        if self.allow_struct_literal
                            && path.segments.len() >= 2
                            && matches!(self.peek_kind(), Some(TokenKind::LeftBrace))
                        {
                            self.advance(); // consume '{'
                            let fields = self.parse_struct_literal_fields()?;
                            let end_span = self.consume(TokenKind::RightBrace, "Expected '}'")?;
                            let variant_name = path.segments.pop().expect("variant segment");
                            return Ok(Expr {
                                span: left.span.cover(end_span.span),
                                ty: Type::Undetermined,
                                kind: ExprKind::VariantLiteral {
                                    enum_path: path,
                                    variant_name,
                                    fields,
                                },
                            });
                        }
                        return Ok(Expr {
                            span,
                            ty: Type::Undetermined,
                            kind: ExprKind::Path(path),
                        });
                    }
                    Ok(Expr {
                        span,
                        ty: Type::Undetermined,
                        kind: ExprKind::FieldAccess {
                            object: Box::new(left),
                            field: name,
                        },
                    })
                }
            }
            _ => Ok(left),
        }
    }

    fn expr_to_path(expr: &Expr) -> Option<NamePath> {
        match &expr.kind {
            ExprKind::Ident(ident) => Some(NamePath::from_single(*ident)),
            ExprKind::Path(path) => Some(path.clone()),
            _ => None,
        }
    }

    /// Parse `T1 (, T2)* ]` — concrete type arguments used at a
    /// generic instantiation site like `Pair[i32, u8]`. The leading `[`
    /// has already been consumed.
    fn parse_type_arg_list(&mut self) -> Result<Vec<Type>, ParseError> {
        let mut args = Vec::new();
        loop {
            args.push(self.parse_type()?);
            match self.peek_kind() {
                Some(TokenKind::Comma) => {
                    self.advance();
                    continue;
                }
                Some(TokenKind::RightBracket) => {
                    self.advance();
                    break;
                }
                Some(other) => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "',' or ']' in type argument list".to_string(),
                        found: other,
                        span: self.current_span(),
                    });
                }
                None => {
                    return Err(ParseError::UnexpectedEof {
                        span: self.current_span(),
                    });
                }
            }
        }
        Ok(args)
    }

    /// Parse `T [: Bound] (, T [: Bound])* ]` — the leading `[` has already
    /// been consumed.
    fn parse_type_param_list(&mut self) -> Result<Vec<crate::TypeParam>, ParseError> {
        let mut params = Vec::new();
        loop {
            // `const N: usize` declares a const value parameter; a bare name is
            // an ordinary type parameter.
            let is_const = if matches!(self.peek_kind(), Some(TokenKind::Const)) {
                self.advance(); // consume 'const'
                true
            } else {
                false
            };
            let name_span = self
                .consume(TokenKind::Identifier, "Expected type parameter name")?
                .span;
            let name = self.ident(name_span);
            let bound = if matches!(self.peek_kind(), Some(TokenKind::Colon)) {
                self.advance(); // consume ':'
                if is_const {
                    // A const param's annotation is its value type (only `usize`
                    // for now) — a primitive-type keyword, not an identifier.
                    // Accept and drop it.
                    self.advance();
                    None
                } else {
                    let bound_span = self
                        .consume(TokenKind::Identifier, "Expected trait name after ':'")?
                        .span;
                    Some(self.ident(bound_span))
                }
            } else {
                None
            };
            params.push(crate::TypeParam {
                name,
                bound,
                is_const,
            });
            match self.peek_kind() {
                Some(TokenKind::Comma) => {
                    self.advance();
                    continue;
                }
                Some(TokenKind::RightBracket) => {
                    self.advance();
                    break;
                }
                Some(other) => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "',' or ']' in type parameter list".to_string(),
                        found: other,
                        span: self.current_span(),
                    });
                }
                None => {
                    return Err(ParseError::UnexpectedEof {
                        span: self.current_span(),
                    });
                }
            }
        }
        Ok(params)
    }

    /// Parse a comma-separated argument list, each argument optionally prefixed
    /// by a passing mode (`mut v`, `take x`); a bare argument is `View`. The
    /// returned mode vec is parallel to (same length as) the args vec.
    fn parse_argument_list(&mut self) -> Result<(Vec<Expr>, Vec<PassMode>), ParseError> {
        let mut args = Vec::new();
        let mut modes = Vec::new();

        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
            return Ok((args, modes)); // Empty argument list
        }

        // Parse first argument
        modes.push(self.parse_pass_mode());
        args.push(self.parse_expression(Precedence::NONE)?);

        // Parse remaining arguments
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.advance(); // consume ','
            modes.push(self.parse_pass_mode());
            args.push(self.parse_expression(Precedence::NONE)?);
        }

        Ok((args, modes))
    }

    /// Consume an optional leading `read`/`mut`/`take` mode keyword (on a call
    /// argument or a parameter name), defaulting to `View`.
    fn parse_pass_mode(&mut self) -> PassMode {
        match self.peek_kind() {
            Some(TokenKind::Read) => {
                self.advance();
                PassMode::Read
            }
            Some(TokenKind::Mut) => {
                self.advance();
                PassMode::Mut
            }
            Some(TokenKind::Take) => {
                self.advance();
                PassMode::Take
            }
            _ => PassMode::Read,
        }
    }

    // Helper methods
    fn parse_function_with_attrs(
        &mut self,
        mut attrs: PendingAttrs,
    ) -> Result<Function, ParseError> {
        let runtime = attrs.runtime.take();
        let repr_c = attrs.repr_c;
        let is_entry = attrs.entry;

        // Consume 'fn' keyword
        let fn_start = self.consume(TokenKind::Fn, "Expected 'fn'")?.span.start();

        // Parse function name
        let name_span = self
            .consume(TokenKind::Identifier, "Expected function name")?
            .span;
        let name = self.ident(name_span);

        // Optional type parameter list: [T, U: Trait, ...]
        let type_params = if matches!(self.peek_kind(), Some(TokenKind::LeftBracket)) {
            self.advance(); // consume '['
            self.parse_type_param_list()?
        } else {
            Vec::new()
        };

        // Parse parameter list
        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;

        // Parse optional return type
        let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
            self.advance(); // consume '->'
            Some(self.parse_type()?)
        } else {
            None
        };

        // Optional provenance clause: `-> T from param` names the parameter a
        // returned borrow is derived from.
        let provenance = if self.consume_optional(TokenKind::From) {
            let span = self
                .consume(
                    TokenKind::Identifier,
                    "Expected parameter name after `from`",
                )?
                .span;
            Some(self.ident(span))
        } else {
            None
        };

        // Validate attributes on function
        if repr_c {
            return Err(ParseError::InvalidAttributeUsage {
                message: "@repr is only valid on structs".to_string(),
                span: name.span,
            });
        }

        // Parse either a declaration (with ';') or a definition with a body
        let (body, span_end) = if matches!(self.peek_kind(), Some(TokenKind::Semicolon)) {
            let semicolon = self.advance(); // consume ';'
            if runtime.is_none() {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "function declarations without body require @runtime attribute"
                        .to_string(),
                    span: name.span,
                });
            }
            let empty_block = Block {
                stmts: Vec::new(),
                expr: None,
                span: semicolon.span,
            };
            (empty_block, semicolon.span.end())
        } else {
            if runtime.is_some() {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "@runtime functions must not have a body".to_string(),
                    span: name.span,
                });
            }
            let body = self.parse_block()?;
            let span_end = body.span.end();
            (body, span_end)
        };

        let full_span = attrs.finalize_span(fn_start, span_end);

        Ok(Function {
            name,
            type_params,
            parameters,
            return_type,
            provenance,
            body,
            runtime_binding: runtime,
            is_entry,
            span: full_span,
        })
    }

    fn parse_struct_with_attrs(
        &mut self,
        attrs: PendingAttrs,
    ) -> Result<StructDefinition, ParseError> {
        let repr_c = attrs.repr_c;

        // Consume 'struct' keyword
        let struct_start = self
            .consume(TokenKind::Struct, "Expected 'struct'")?
            .span
            .start();

        // Optional view-kind modifier: `struct view Name { ... }`, sitting
        // between the keyword and the name like `mut` in `let mut x`.
        let is_view = self.consume_optional(TokenKind::View);

        // Parse struct name
        let name_span = self
            .consume(TokenKind::Identifier, "Expected struct name")?
            .span;
        let name = self.ident(name_span);

        // Optional type parameter list: struct Pair[T, U: Trait] { ... }
        let type_params = if matches!(self.peek_kind(), Some(TokenKind::LeftBracket)) {
            self.advance(); // consume '['
            self.parse_type_param_list()?
        } else {
            Vec::new()
        };

        // Parse struct body
        self.consume(TokenKind::LeftBrace, "Expected '{' to start struct body")?;
        let fields = self.parse_struct_field_list()?;
        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end struct body")?;
        let struct_end = right_brace.span.end();

        let full_span = attrs.finalize_span(struct_start, struct_end);

        Ok(StructDefinition {
            name,
            type_params,
            fields,
            repr_c,
            is_builtin: false,
            is_view,
            span: full_span,
        })
    }

    /// Parse an `@builtin type Name[params]` stub: a fieldless nominal type
    /// whose representation is intrinsic. Modeled as a `StructDefinition` so it
    /// flows through the normal nominal pipeline (ids, type params, impls).
    fn parse_builtin_type(&mut self, attrs: PendingAttrs) -> Result<StructDefinition, ParseError> {
        let start = self
            .consume(TokenKind::Type, "Expected 'type'")?
            .span
            .start();
        if !attrs.builtin {
            return Err(ParseError::InvalidAttributeUsage {
                message: "a `type` declaration must be `@builtin` (type aliases are not yet \
                          supported)"
                    .to_string(),
                span: self.current_span(),
            });
        }
        let name_span = self
            .consume(TokenKind::Identifier, "Expected type name")?
            .span;
        let name = self.ident(name_span);
        let type_params = if matches!(self.peek_kind(), Some(TokenKind::LeftBracket)) {
            self.advance(); // consume '['
            self.parse_type_param_list()?
        } else {
            Vec::new()
        };
        let end = self.previous().span.end();
        let full_span = attrs.finalize_span(start, end);
        Ok(StructDefinition {
            name,
            type_params,
            fields: Vec::new(),
            repr_c: false,
            is_builtin: true,
            is_view: false,
            span: full_span,
        })
    }

    fn parse_enum_definition(&mut self) -> Result<crate::EnumDefinition, ParseError> {
        let span_start = self
            .consume(TokenKind::Enum, "Expected 'enum'")?
            .span
            .start();
        let is_view = self.consume_optional(TokenKind::View);
        let name_span = self
            .consume(TokenKind::Identifier, "Expected enum name")?
            .span;
        let name = self.ident(name_span);

        // Optional type-param list mirrors the struct grammar.
        let type_params = if matches!(self.peek_kind(), Some(TokenKind::LeftBracket)) {
            self.advance();
            self.parse_type_param_list()?
        } else {
            Vec::new()
        };

        self.consume(TokenKind::LeftBrace, "Expected '{' to start enum body")?;
        let mut variants = Vec::new();
        if !matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            variants.push(self.parse_variant_definition()?);
            while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                self.advance();
                if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                    break;
                }
                variants.push(self.parse_variant_definition()?);
            }
        }
        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end enum body")?;
        let span = crate::Span::new(span_start, right_brace.span.end());

        Ok(crate::EnumDefinition {
            name,
            type_params,
            variants,
            is_view,
            span,
        })
    }

    fn parse_variant_definition(&mut self) -> Result<crate::VariantDefinition, ParseError> {
        let name_span = self
            .consume(TokenKind::Identifier, "Expected variant name")?
            .span;
        let name = self.ident(name_span);
        let span_start = name_span.start();
        // Unit variant: `None`. Struct-like: `Some { value: T, ... }`.
        // Tuple variant: `Some(T, ...)` — desugared to fields named `0`, `1`, …
        let (fields, is_tuple, span_end) = if matches!(self.peek_kind(), Some(TokenKind::LeftBrace))
        {
            self.advance();
            let fields = self.parse_struct_field_list()?;
            let right_brace =
                self.consume(TokenKind::RightBrace, "Expected '}' to end variant body")?;
            (fields, false, right_brace.span.end())
        } else if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
            self.advance();
            let mut fields = Vec::new();
            if !matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                loop {
                    let pos_span = self.current_span();
                    let field_type = self.parse_type()?;
                    let sym = self.interner.get_or_intern(fields.len().to_string());
                    fields.push(crate::StructFieldDefinition {
                        name: crate::Ident {
                            sym,
                            span: pos_span,
                        },
                        field_type,
                    });
                    if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                        self.advance();
                        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            let close = self.consume(TokenKind::RightParen, "Expected ')' to end tuple variant")?;
            (fields, true, close.span.end())
        } else {
            (Vec::new(), false, name_span.end())
        };
        Ok(crate::VariantDefinition {
            name,
            fields,
            is_tuple,
            span: crate::Span::new(span_start, span_end),
        })
    }

    fn parse_trait_definition(&mut self) -> Result<crate::TraitDefinition, ParseError> {
        let span_start = self
            .consume(TokenKind::Trait, "Expected 'trait'")?
            .span
            .start();
        let name_span = self
            .consume(TokenKind::Identifier, "Expected trait name")?
            .span;
        let name = self.ident(name_span);
        self.consume(TokenKind::LeftBrace, "Expected '{' to start trait body")?;

        // Parse zero or more method signatures: fn name(params) [-> type] ;
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn)) {
            self.advance();
            let mname_span = self
                .consume(TokenKind::Identifier, "Expected method name")?
                .span;
            let mname = self.ident(mname_span);
            self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
            let parameters = self.parse_method_params()?;
            self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
            let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };
            self.consume(
                TokenKind::Semicolon,
                "Expected ';' after trait method signature",
            )?;
            methods.push(crate::TraitMethod {
                name: mname,
                parameters,
                return_type,
            });
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end trait body")?;
        Ok(crate::TraitDefinition {
            name,
            methods,
            span: crate::Span::new(span_start, right_brace.span.end()),
        })
    }

    fn parse_impl_definition(&mut self) -> Result<crate::ImplDefinition, ParseError> {
        let span_start = self
            .consume(TokenKind::Impl, "Expected 'impl'")?
            .span
            .start();
        // `impl Trait for Type { ... }` or inherent `impl Type { ... }`. Parse
        // the first type, then if `for` follows it was the trait name.
        let first = self.parse_type()?;
        let (trait_name, target) = if matches!(self.peek_kind(), Some(TokenKind::For)) {
            self.advance(); // consume 'for'
            let trait_ident = match first {
                Type::Struct(sym, ref args) if args.is_empty() => Ident {
                    sym,
                    span: self.previous().span,
                },
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "a trait name before 'for'".to_string(),
                        found: TokenKind::For,
                        span: self.current_span(),
                    });
                }
            };
            (Some(trait_ident), self.parse_type()?)
        } else {
            (None, first)
        };
        self.consume(TokenKind::LeftBrace, "Expected '{' to start impl body")?;

        // Parse zero or more methods. Each is either an ordinary method with a
        // body, or a `@runtime("...")` associated intrinsic declared with `;`
        // and no body (the primitive conversions). A leading `@` starts an
        // attribute; otherwise the method begins at `fn`.
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn | TokenKind::At)) {
            let mut attrs = self.parse_attributes()?;
            let runtime = attrs.runtime.take();
            if attrs.repr_c || attrs.entry {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "only @runtime is valid on impl methods".to_string(),
                    span: self.current_span(),
                });
            }
            self.consume(TokenKind::Fn, "Expected 'fn'")?;
            let mname_span = self
                .consume(TokenKind::Identifier, "Expected method name")?
                .span;
            let mname = self.ident(mname_span);
            self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
            let parameters = self.parse_method_params()?;
            self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
            let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };
            let provenance = if self.consume_optional(TokenKind::From) {
                let span = self
                    .consume(
                        TokenKind::Identifier,
                        "Expected parameter name after `from`",
                    )?
                    .span;
                Some(self.ident(span))
            } else {
                None
            };
            // `@runtime` methods are bodyless declarations terminated by `;`;
            // everything else uses parse_block so trailing expressions are
            // preserved (same as regular function bodies).
            let body = if matches!(self.peek_kind(), Some(TokenKind::Semicolon)) {
                let semicolon = self.advance();
                if runtime.is_none() {
                    return Err(ParseError::InvalidAttributeUsage {
                        message: "impl method declarations without a body require @runtime"
                            .to_string(),
                        span: mname.span,
                    });
                }
                Block {
                    stmts: Vec::new(),
                    expr: None,
                    span: semicolon.span,
                }
            } else {
                if runtime.is_some() {
                    return Err(ParseError::InvalidAttributeUsage {
                        message: "@runtime impl methods must not have a body".to_string(),
                        span: mname.span,
                    });
                }
                self.parse_block()?
            };
            methods.push(crate::ImplMethod {
                name: mname,
                parameters,
                return_type,
                provenance,
                body,
                runtime,
            });
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end impl body")?;
        Ok(crate::ImplDefinition {
            trait_name,
            target,
            methods,
            span: crate::Span::new(span_start, right_brace.span.end()),
        })
    }

    fn parse_struct_field_list(&mut self) -> Result<Vec<StructFieldDefinition>, ParseError> {
        let mut fields = Vec::new();

        // Handle empty field list
        if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            return Ok(fields);
        }

        // Parse first field
        fields.push(self.parse_struct_field_definition()?);

        // Parse remaining fields
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.advance(); // consume ','

            // Allow trailing comma
            if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                break;
            }

            fields.push(self.parse_struct_field_definition()?);
        }

        Ok(fields)
    }

    fn parse_struct_field_definition(&mut self) -> Result<StructFieldDefinition, ParseError> {
        let name_span = self
            .consume(TokenKind::Identifier, "Expected field name")?
            .span;
        let name = self.ident(name_span);

        self.consume(TokenKind::Colon, "Expected ':' after field name")?;
        let field_type = self.parse_type()?;

        Ok(StructFieldDefinition { name, field_type })
    }

    fn parse_struct_literal_fields(&mut self) -> Result<Vec<StructField>, ParseError> {
        let mut fields = Vec::new();

        // Handle empty field list
        if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            return Ok(fields);
        }

        // Parse first field
        fields.push(self.parse_struct_literal_field()?);

        // Parse remaining fields
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.advance(); // consume ','

            // Allow trailing comma
            if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                break;
            }

            fields.push(self.parse_struct_literal_field()?);
        }

        Ok(fields)
    }

    fn parse_struct_literal_field(&mut self) -> Result<StructField, ParseError> {
        let name_span = self
            .consume(TokenKind::Identifier, "Expected field name")?
            .span;
        let name = self.ident(name_span);

        self.consume(TokenKind::Equals, "Expected '=' after field name")?;
        let value = self.parse_expression(Precedence::NONE)?;

        Ok(StructField { name, value })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();

        // Handle empty parameter list
        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
            return Ok(parameters);
        }

        // Parse first parameter
        parameters.push(self.parse_parameter()?);

        // Parse remaining parameters
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.advance(); // consume ','
            parameters.push(self.parse_parameter()?);
        }

        Ok(parameters)
    }

    /// Like `parse_parameter_list`, but a leading bare `self` is recognized as
    /// the method receiver — a parameter typed `Self`. Its presence is what
    /// makes a function in an `impl`/`trait` a method rather than an
    /// associated function.
    fn parse_method_params(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();
        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
            return Ok(parameters);
        }

        // A receiver is `self` optionally prefixed by a mode (`mut self`).
        // Detect by looking past an optional leading mode keyword.
        let self_offset = match self.peek_kind() {
            Some(TokenKind::Read | TokenKind::Mut | TokenKind::Take) => 1,
            _ => 0,
        };
        let leading_self = matches!(self.peek_kind_at(self_offset), Some(TokenKind::Identifier))
            && self
                .tokens
                .get(self.current + self_offset)
                .map(|t| t.span.text(self.source))
                == Some("self");
        if leading_self {
            let mode = self.parse_pass_mode(); // consumes the mode keyword if present
            let span = self.advance().span; // consume `self`
            parameters.push(Parameter {
                name: self.ident(span),
                type_annotation: Type::SelfType,
                mode,
            });
            // `(self)` — no further parameters.
            if !matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                return Ok(parameters);
            }
            self.advance(); // consume ',' after self
        }

        // First non-self parameter, then the comma-separated rest.
        parameters.push(self.parse_parameter()?);
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.advance(); // consume ','
            parameters.push(self.parse_parameter()?);
        }

        Ok(parameters)
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let name_span = self
            .consume(TokenKind::Identifier, "Expected parameter name")?
            .span;
        let name = self.ident(name_span);

        self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
        let ty = self.parse_type()?;

        // The passing mode is part of the type: `v: read Vec[T]` borrows,
        // `v: mut Vec[T]` mutably borrows, a bare `v: Vec[T]` is owned (moved
        // in). The borrow is unwrapped here so the rest of the pipeline keeps
        // seeing `{ mode, inner type }`.
        let (mode, type_annotation) = match ty {
            Type::Ref {
                kind: RefKind::Read,
                inner,
            } => (PassMode::Read, *inner),
            Type::Ref {
                kind: RefKind::Mut,
                inner,
            } => (PassMode::Mut, *inner),
            other => (PassMode::Take, other),
        };

        Ok(Parameter {
            name,
            type_annotation,
            mode,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let kind = self.peek_kind().ok_or(ParseError::UnexpectedEof {
            span: self.current_span(),
        })?;

        // Borrow types: `read T` (shared) / `mut T` (exclusive). `take` is not
        // a type — it's the move operator at call sites.
        if let Some(rk) = match kind {
            TokenKind::Read => Some(RefKind::Read),
            TokenKind::Mut => Some(RefKind::Mut),
            _ => None,
        } {
            self.advance();
            let inner = self.parse_type()?;
            return Ok(Type::Ref {
                kind: rk,
                inner: Box::new(inner),
            });
        }

        // Handle primitive types with a simple lookup
        if let Some(ty) = token_to_primitive_type(kind) {
            self.advance();
            return Ok(ty);
        }

        match kind {
            TokenKind::LeftParen => {
                // Tuple type `(A, B, ...)`; a single `(T)` is just grouping.
                self.advance(); // consume '('
                let first = self.parse_type()?;
                if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                    let mut elems = vec![first];
                    while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                        self.advance(); // consume ','
                        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                            break;
                        }
                        elems.push(self.parse_type()?);
                    }
                    self.consume(TokenKind::RightParen, "Expected ')' to close tuple type")?;
                    Ok(Type::Tuple(elems))
                } else {
                    self.consume(TokenKind::RightParen, "Expected ')'")?;
                    Ok(first)
                }
            }
            TokenKind::Identifier => {
                let span = self.advance().span;
                let text = span.text(self.source);
                if text == "Self" {
                    return Ok(Type::SelfType);
                }
                if text == "Array" {
                    // Fixed-size array `Array[T, N]`: element type, then a
                    // length literal (a const, not a type).
                    self.consume(TokenKind::LeftBracket, "Expected '[' after Array")?;
                    let elem_ty = self.parse_type()?;
                    self.consume(
                        TokenKind::Comma,
                        "Expected ',' between Array element type and length",
                    )?;
                    // Length is either a literal count or a const-param name.
                    let len = match self.peek_kind() {
                        Some(TokenKind::Identifier) => {
                            let s = self.advance().span;
                            ConstArg::Name(self.ident(s))
                        }
                        _ => {
                            let s = self.advance().span;
                            let (n, _) = parse_int_literal(s.text(self.source), s)?;
                            ConstArg::Int(n as u64)
                        }
                    };
                    self.consume(TokenKind::RightBracket, "Expected ']' to close Array[T, N]")?;
                    return Ok(Type::Array(Box::new(elem_ty), len));
                }
                let name = self.intern(span);
                // Optional generic instantiation: `Pair[i32]` or `Map[K, V]`.
                let type_args = if matches!(self.peek_kind(), Some(TokenKind::LeftBracket)) {
                    self.advance(); // consume '['
                    self.parse_type_arg_list()?
                } else {
                    Vec::new()
                };
                Ok(Type::Struct(name, type_args))
            }
            // A `*` in type position is always a pointer. The tokenizer emits
            // `UnaryStar` when spacing disambiguates, but `Star` (infix
            // multiply) inside e.g. a turbofish `at[*mut u8]`; both mean the
            // same thing here.
            TokenKind::UnaryStar | TokenKind::Star => {
                self.advance(); // consume '*'
                let mutable = match self.peek_kind() {
                    Some(TokenKind::Const) => {
                        self.advance();
                        false
                    }
                    Some(TokenKind::Mut) => {
                        self.advance();
                        true
                    }
                    Some(kind) => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "'const' or 'mut' after '*'".to_string(),
                            found: kind,
                            span: self.current_span(),
                        });
                    }
                    None => {
                        return Err(ParseError::UnexpectedEof {
                            span: self.current_span(),
                        });
                    }
                };
                let pointee = Box::new(self.parse_type()?);
                Ok(Type::Pointer { mutable, pointee })
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "type".to_string(),
                found: kind,
                span: self.current_span(),
            }),
        }
    }

    fn parse_statement_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while let Some(kind) = self.peek_kind() {
            if kind == TokenKind::RightBrace {
                break;
            }
            let statement = self.parse_statement()?;
            let statement_end = self.previous().span.end();
            let has_semicolon = matches!(self.peek_kind(), Some(TokenKind::Semicolon));

            if has_semicolon {
                self.advance();
            } else if let Some(next) = self.peek() {
                let is_right_brace = next.kind == TokenKind::RightBrace;
                if !is_right_brace && self.is_same_line(statement_end, next.span.start()) {
                    self.emit(
                        "statements on the same line should be separated by a semicolon",
                        next.span,
                        Severity::Error,
                    );
                    // Continue parsing to collect more errors
                }
            }

            statements.push(statement);
        }

        Ok(statements)
    }

    /// Parse a block with statements and optional trailing expression.
    /// A trailing expression (without semicolon) becomes the block's value.
    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let left_brace = self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        let block_start = left_brace.span.start();

        let mut stmts = Vec::new();

        while let Some(kind) = self.peek_kind() {
            if kind == TokenKind::RightBrace {
                break;
            }

            // Blocks are statement lists — there is no trailing-expression
            // value. A function produces its result with `return`.
            let stmt = self.parse_statement()?;
            let stmt_end = self.previous().span.end();
            let has_semicolon = matches!(self.peek_kind(), Some(TokenKind::Semicolon));

            if has_semicolon {
                self.advance();
            } else if !matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                // No semicolon and not at the closing brace: two statements
                // share a line.
                if let Some(next) = self.peek() {
                    if self.is_same_line(stmt_end, next.span.start()) {
                        self.emit(
                            "statements on the same line should be separated by a semicolon",
                            next.span,
                            Severity::Error,
                        );
                    }
                }
            }
            stmts.push(stmt);
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}'")?;
        let block_end = right_brace.span.end();

        Ok(Block {
            stmts,
            expr: None,
            span: Span::new(block_start, block_end),
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Let) => self.parse_let_statement(),
            Some(TokenKind::Loop) => self.parse_loop_statement(),
            Some(TokenKind::While) => self.parse_while_statement(),
            Some(TokenKind::For) => self.parse_for_statement(),
            Some(TokenKind::Break) => self.parse_break_statement(),
            Some(TokenKind::Return) => self.parse_return_statement(),
            _ => {
                // Parse an expression, then decide: a trailing `=` makes it an
                // assignment whose left side must be an lvalue (a variable, a
                // struct field, or a dereference); otherwise it's an
                // expression statement.
                let expr = self.parse_expression(Precedence::NONE)?;
                if !matches!(self.peek_kind(), Some(TokenKind::Equals)) {
                    return Ok(Stmt::Expr(expr));
                }
                let lhs_span = expr.span;
                self.advance(); // consume '='
                let value = self.parse_expression(Precedence::NONE)?;
                match expr.kind {
                    ExprKind::Ident(target) => Ok(Stmt::Assign { target, value }),
                    ExprKind::Dereference(inner) => Ok(Stmt::DerefAssign { ptr: *inner, value }),
                    ExprKind::FieldAccess { object, field } => Ok(Stmt::FieldAssign {
                        object: *object,
                        field,
                        value,
                    }),
                    ExprKind::Path(mut path) if path.segments.len() >= 2 => {
                        let field = path.segments.pop().expect("field segment");
                        let object = if path.segments.len() == 1 {
                            Expr {
                                span: path.segments[0].span,
                                ty: Type::Undetermined,
                                kind: ExprKind::Ident(path.segments[0]),
                            }
                        } else {
                            let span = path.segments[0]
                                .span
                                .cover(path.segments.last().expect("path").span);
                            Expr {
                                span,
                                ty: Type::Undetermined,
                                kind: ExprKind::Path(path),
                            }
                        };
                        Ok(Stmt::FieldAssign {
                            object,
                            field,
                            value,
                        })
                    }
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "a variable, field, or `*ptr` on the left of `=`".to_string(),
                        found: TokenKind::Equals,
                        span: lhs_span,
                    }),
                }
            }
        }
    }

    fn unescape_string_literal(raw: &str) -> String {
        let inner = raw
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(raw);

        let mut out = String::with_capacity(inner.len());
        let mut chars = inner.chars();
        while let Some(ch) = chars.next() {
            if ch != '\\' {
                out.push(ch);
                continue;
            }
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                Some('t') => out.push('\t'),
                Some('"') => out.push('"'),
                Some('\\') => out.push('\\'),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        }
        out
    }

    /// Parse a module-level `let [mut] NAME: TYPE = EXPR;` declaration.
    /// Type annotation is required (no global type inference). Initializer
    /// is validated to be a literal at lowering time.
    fn parse_global_decl(&mut self) -> Result<GlobalDecl, ParseError> {
        let let_span = self.consume(TokenKind::Let, "Expected 'let'")?.span;
        let mutable = matches!(self.peek_kind(), Some(TokenKind::Mut));
        if mutable {
            self.advance();
        }
        let name_span = self
            .consume(TokenKind::Identifier, "Expected identifier after 'let'")?
            .span;
        let name = self.ident(name_span);
        self.consume(
            TokenKind::Colon,
            "Module-level `let` requires a type annotation",
        )?;
        let type_annotation = self.parse_type()?;
        self.consume(TokenKind::Equals, "Expected '=' in module-level let")?;
        let value = self.parse_expression(Precedence::NONE)?;
        let end = value.span.end();
        self.consume(TokenKind::Semicolon, "Expected ';' after module-level let")?;
        Ok(GlobalDecl {
            name,
            mutable,
            type_annotation,
            value,
            span: Span::new(let_span.start(), end),
        })
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::Let, "Expected 'let'")?;

        // The binding form is a pattern; `let` accepts only the irrefutable
        // subset (wildcard, binding, tuples thereof), enforced at lower time.
        let pattern = self.parse_pattern()?;

        // Optional type annotation
        let type_annotation = if matches!(self.peek_kind(), Some(TokenKind::Colon)) {
            self.advance(); // consume ':'
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenKind::Equals, "Expected '=' in let statement")?;
        let value = self.parse_expression(Precedence::NONE)?;
        // Terminator will be handled by parse_statement_list

        Ok(Stmt::Let {
            pattern,
            type_annotation,
            value,
        })
    }

    fn parse_loop_statement(&mut self) -> Result<Stmt, ParseError> {
        let loop_start = {
            let token = self.consume(TokenKind::Loop, "Expected 'loop'")?;
            token.span.start()
        };
        self.consume(TokenKind::LeftBrace, "Expected '{' after 'loop'")?;
        let body = self.parse_statement_list()?;
        let end = self.consume(TokenKind::RightBrace, "Expected '}' to end loop body")?;

        Ok(Stmt::Loop {
            body,
            span: Span::new(loop_start, end.span.end()),
        })
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, ParseError> {
        let while_start = {
            let token = self.consume(TokenKind::While, "Expected 'while'")?;
            token.span.start()
        };
        // Disallow struct literals in condition to avoid ambiguity with `while x { }`
        let condition = self.without_struct_literals(|p| p.parse_expression(Precedence::NONE))?;
        self.consume(TokenKind::LeftBrace, "Expected '{' after while condition")?;
        let body = self.parse_statement_list()?;
        let end = self.consume(TokenKind::RightBrace, "Expected '}' to end while body")?;

        Ok(Stmt::While {
            condition,
            body,
            span: Span::new(while_start, end.span.end()),
        })
    }

    /// `for var in start..end { body }`. Parsed into a structured `Stmt::For`
    /// node; the hygienic lowering to a `while` loop happens in the HIR
    /// builder, which can mint fresh symbols for the loop variable and the
    /// range bound so neither the bounds nor the body can capture them.
    fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        let for_start = self.consume(TokenKind::For, "Expected 'for'")?.span.start();
        let var_span = self
            .consume(TokenKind::Identifier, "Expected loop variable after 'for'")?
            .span;
        let var = self.ident(var_span);
        self.consume(TokenKind::In, "Expected 'in' after the loop variable")?;
        // Disallow struct literals so the body's `{` isn't read as one.
        let (start, end) =
            self.without_struct_literals(|p| -> Result<(Expr, Expr), ParseError> {
                let start = p.parse_expression(Precedence::NONE)?;
                p.consume(TokenKind::DotDot, "Expected '..' in for-loop range")?;
                let end = p.parse_expression(Precedence::NONE)?;
                Ok((start, end))
            })?;
        self.consume(TokenKind::LeftBrace, "Expected '{' to start for-loop body")?;
        let body = self.parse_statement_list()?;
        let end_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end for-loop body")?;
        let span = Span::new(for_start, end_brace.span.end());

        Ok(Stmt::For {
            var,
            start,
            end,
            body,
            span,
        })
    }

    fn parse_break_statement(&mut self) -> Result<Stmt, ParseError> {
        let token = self.consume(TokenKind::Break, "Expected 'break'")?;
        Ok(Stmt::Break { span: token.span })
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        let span = self.consume(TokenKind::Return, "Expected 'return'")?.span;
        // Go-style: `return` is statement-ending, so its value must begin on the
        // same line — a newline after `return` is a bare return (as if a
        // semicolon were inserted), never a grab of the next line.
        let value = match self.peek() {
            None => None,
            Some(next) if matches!(next.kind, TokenKind::Semicolon | TokenKind::RightBrace) => None,
            Some(next) if !self.is_same_line(span.end(), next.span.start()) => None,
            _ => Some(self.parse_expression(Precedence::NONE)?),
        };
        Ok(Stmt::Return { value, span })
    }

    fn parse_if_expression(&mut self) -> Result<Expr, ParseError> {
        let if_start = {
            let token = self.consume(TokenKind::If, "Expected 'if'")?;
            token.span.start()
        };

        // Disallow struct literals in condition to avoid ambiguity with `if x { }`
        let condition = self.without_struct_literals(|p| p.parse_expression(Precedence::NONE))?;

        let then_branch = self.parse_block()?;
        let mut end = then_branch.span.end();

        let else_branch = if matches!(self.peek_kind(), Some(TokenKind::Else)) {
            self.advance(); // consume 'else'
            // Check for `else if` - treat as `else { if ... }`
            if matches!(self.peek_kind(), Some(TokenKind::If)) {
                let nested_if = self.parse_if_expression()?;
                let nested_span = nested_if.span;
                end = nested_span.end();
                Some(Block {
                    stmts: Vec::new(),
                    expr: Some(Box::new(nested_if)),
                    span: nested_span,
                })
            } else {
                let else_block = self.parse_block()?;
                end = else_block.span.end();
                Some(else_block)
            }
        } else {
            None
        };

        Ok(Expr {
            span: Span::new(if_start, end),
            ty: Type::Undetermined,
            kind: ExprKind::If {
                condition: Box::new(condition),
                then_branch,
                else_branch,
            },
        })
    }

    fn parse_match_expression(&mut self) -> Result<Expr, ParseError> {
        let match_start = self
            .consume(TokenKind::Match, "Expected 'match'")?
            .span
            .start();
        // The scrutinee is parsed without struct-literal context for the
        // same reason `if` and `while` do — `{` introduces the arm list.
        let scrutinee = self.without_struct_literals(|p| p.parse_expression(Precedence::NONE))?;
        self.consume(TokenKind::LeftBrace, "Expected '{' to start match arms")?;
        let mut arms = Vec::new();
        if !matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            arms.push(self.parse_match_arm()?);
            while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                self.advance();
                if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                    break;
                }
                arms.push(self.parse_match_arm()?);
            }
        }
        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end match")?;
        Ok(Expr {
            span: Span::new(match_start, right_brace.span.end()),
            ty: Type::Undetermined,
            kind: ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
        })
    }

    fn parse_match_arm(&mut self) -> Result<crate::MatchArm, ParseError> {
        let pattern = self.parse_pattern()?;
        self.consume(TokenKind::FatArrow, "Expected '=>' after match pattern")?;
        let body = self.parse_expression(Precedence::NONE)?;
        let span = pattern.span().cover(body.span);
        Ok(crate::MatchArm {
            pattern,
            body,
            span,
        })
    }

    /// Parse a pattern. Recursive: tuple patterns nest sub-patterns, and
    /// variant fields nest sub-patterns. Disambiguation:
    /// - `mut x` / bare `x` → a binding
    /// - `_` → wildcard
    /// - `(...)` → tuple
    /// - `A.B { ... }` (a dotted path) → enum variant
    fn parse_pattern(&mut self) -> Result<crate::Pattern, ParseError> {
        // `take [mut] x` — a binding that moves the value out of the scrutinee.
        if matches!(self.peek_kind(), Some(TokenKind::Take)) {
            let take_span = self.advance().span;
            let mutable = matches!(self.peek_kind(), Some(TokenKind::Mut));
            if mutable {
                self.advance();
            }
            let name_span = self
                .consume(TokenKind::Identifier, "Expected binding name after 'take'")?
                .span;
            let name = self.ident(name_span);
            return Ok(crate::Pattern::Binding {
                name,
                mutable,
                mode: crate::PassMode::Take,
                span: take_span.cover(name_span),
            });
        }

        // `mut x` — a mutable binding.
        if matches!(self.peek_kind(), Some(TokenKind::Mut)) {
            let mut_span = self.advance().span;
            let name_span = self
                .consume(TokenKind::Identifier, "Expected binding name after 'mut'")?
                .span;
            let name = self.ident(name_span);
            return Ok(crate::Pattern::Binding {
                name,
                mutable: true,
                mode: crate::PassMode::Read,
                span: mut_span.cover(name_span),
            });
        }

        // `(a, b, ...)` — a tuple pattern.
        if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
            let open = self.advance().span;
            let mut elems = Vec::new();
            if !matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                elems.push(self.parse_pattern()?);
                while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                    self.advance();
                    if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                        break;
                    }
                    elems.push(self.parse_pattern()?);
                }
            }
            let close =
                self.consume(TokenKind::RightParen, "Expected ')' to close tuple pattern")?;
            return Ok(crate::Pattern::Tuple {
                elems,
                span: open.cover(close.span),
            });
        }

        // Literal patterns: integers (with optional leading `-`) and booleans.
        if matches!(self.peek_kind(), Some(TokenKind::IntLiteral)) {
            let span = self.advance().span;
            let (value, ty) = parse_int_literal(span.text(self.source), span)?;
            return Ok(crate::Pattern::Int { value, ty, span });
        }
        if matches!(
            self.peek_kind(),
            Some(TokenKind::Minus | TokenKind::UnaryMinus)
        ) {
            let minus_span = self.advance().span;
            let lit_span = self
                .consume(TokenKind::IntLiteral, "Expected an integer after '-'")?
                .span;
            let (value, ty) = parse_int_literal(lit_span.text(self.source), lit_span)?;
            return Ok(crate::Pattern::Int {
                value: -value,
                ty,
                span: minus_span.cover(lit_span),
            });
        }
        if matches!(self.peek_kind(), Some(TokenKind::True)) {
            let span = self.advance().span;
            return Ok(crate::Pattern::Bool { value: true, span });
        }
        if matches!(self.peek_kind(), Some(TokenKind::False)) {
            let span = self.advance().span;
            return Ok(crate::Pattern::Bool { value: false, span });
        }

        let first_span = self
            .consume(TokenKind::Identifier, "Expected pattern")?
            .span;
        if first_span.text(self.source) == "_" {
            return Ok(crate::Pattern::Wildcard { span: first_span });
        }

        // A bare name followed by `{` is a struct destructuring pattern.
        if matches!(self.peek_kind(), Some(TokenKind::LeftBrace)) {
            let name = self.ident(first_span);
            let (fields, close) = self.parse_pattern_fields()?;
            return Ok(crate::Pattern::Struct {
                name,
                fields,
                span: first_span.cover(close),
            });
        }

        // A bare identifier (no `.`) is a binding; a dotted path is an
        // enum-variant pattern.
        if !matches!(self.peek_kind(), Some(TokenKind::Dot)) {
            let name = self.ident(first_span);
            return Ok(crate::Pattern::Binding {
                name,
                mutable: false,
                mode: crate::PassMode::Read,
                span: first_span,
            });
        }

        let mut segments = vec![self.ident(first_span)];
        while matches!(self.peek_kind(), Some(TokenKind::Dot)) {
            self.advance();
            let segment_span = self
                .consume(TokenKind::Identifier, "Expected name segment in pattern")?
                .span;
            segments.push(self.ident(segment_span));
        }
        let variant_name = segments.pop().expect("variant segment");
        let enum_path = NamePath { segments };
        let (fields, end_span) = if matches!(self.peek_kind(), Some(TokenKind::LeftBrace)) {
            self.parse_pattern_fields()?
        } else if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
            // Tuple variant: `Some(p0, p1, ...)`, desugared to fields `0`, `1`, …
            self.advance(); // consume '('
            let mut fields = Vec::new();
            if !matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                loop {
                    let elem = self.parse_pattern()?;
                    let sym = self.interner.get_or_intern(fields.len().to_string());
                    fields.push(crate::FieldPattern {
                        field: crate::Ident {
                            sym,
                            span: elem.span(),
                        },
                        pattern: elem,
                    });
                    if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                        self.advance();
                        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            let close = self.consume(TokenKind::RightParen, "Expected ')' in tuple pattern")?;
            (fields, close.span)
        } else {
            (Vec::new(), variant_name.span)
        };
        let span = first_span.cover(end_span);
        Ok(crate::Pattern::Variant {
            enum_path,
            variant_name,
            fields,
            span,
        })
    }

    /// Parse a `{ field, field: subpat, ... }` field list (the leading `{` is
    /// the current token). Returns the fields and the closing brace's span.
    fn parse_pattern_fields(
        &mut self,
    ) -> Result<(Vec<crate::FieldPattern>, prim_tok::Span), ParseError> {
        self.advance(); // consume '{'
        let mut fields = Vec::new();
        if !matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            fields.push(self.parse_field_pattern()?);
            while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                self.advance();
                if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                    break;
                }
                fields.push(self.parse_field_pattern()?);
            }
        }
        let close = self.consume(TokenKind::RightBrace, "Expected '}' in pattern")?;
        Ok((fields, close.span))
    }

    fn parse_field_pattern(&mut self) -> Result<crate::FieldPattern, ParseError> {
        // `take [mut] name` / `mut name` shorthands bind the field to its own
        // name; `take` additionally moves it out of the scrutinee.
        let lead = self.peek().map(|t| t.span);
        let take = matches!(self.peek_kind(), Some(TokenKind::Take));
        if take {
            self.advance();
        }
        let mutable = matches!(self.peek_kind(), Some(TokenKind::Mut));
        if mutable {
            self.advance();
        }
        if take || mutable {
            let field_span = self
                .consume(TokenKind::Identifier, "Expected field name in pattern")?
                .span;
            let field = self.ident(field_span);
            let span = lead.unwrap_or(field_span).cover(field_span);
            return Ok(crate::FieldPattern {
                field,
                pattern: crate::Pattern::Binding {
                    name: field,
                    mutable,
                    mode: if take {
                        crate::PassMode::Take
                    } else {
                        crate::PassMode::Read
                    },
                    span,
                },
            });
        }
        let field_span = self
            .consume(TokenKind::Identifier, "Expected field name in pattern")?
            .span;
        let field = self.ident(field_span);
        // `name: <pattern>` matches the field against a sub-pattern; bare
        // `name` is shorthand for `name: name` — a binding of the field's
        // own name.
        let pattern = if matches!(self.peek_kind(), Some(TokenKind::Colon)) {
            self.advance();
            self.parse_pattern()?
        } else {
            crate::Pattern::Binding {
                name: field,
                mutable: false,
                mode: crate::PassMode::Read,
                span: field_span,
            }
        };
        Ok(crate::FieldPattern { field, pattern })
    }

    fn consume(&mut self, expected: TokenKind, message: &str) -> Result<&Token, ParseError> {
        match self.tokens.get(self.current) {
            Some(tok) if tok.kind == expected => Ok(self.advance()),
            Some(tok) => Err(ParseError::UnexpectedToken {
                expected: message.to_string(),
                found: tok.kind,
                span: tok.span,
            }),
            None => Err(ParseError::UnexpectedEof {
                span: self.current_span(),
            }),
        }
    }

    /// Consume the next token iff it is `kind`, reporting whether it was there.
    fn consume_optional(&mut self, kind: TokenKind) -> bool {
        if self.peek_kind() == Some(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.current).map(|t| t.kind)
    }

    fn peek_kind_at(&self, offset: usize) -> Option<TokenKind> {
        self.tokens.get(self.current + offset).map(|t| t.kind)
    }

    /// True if the current token is a primitive type keyword (`u8`, `i32`, …).
    fn peek_is_primitive_type(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(
                TokenKind::U8
                    | TokenKind::I8
                    | TokenKind::U16
                    | TokenKind::I16
                    | TokenKind::U32
                    | TokenKind::I32
                    | TokenKind::U64
                    | TokenKind::I64
                    | TokenKind::Usize
                    | TokenKind::Isize
                    | TokenKind::F32
                    | TokenKind::F64
                    | TokenKind::Bool
            )
        )
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn current_span(&self) -> Span {
        self.peek()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::new(self.source.len(), self.source.len()))
    }

    /// True when the current token is glued to the previous one — no
    /// whitespace (or comment) between them. This is what distinguishes a call
    /// `(` / turbofish `[` from a parenthesized group / array on the next line:
    /// `f(x)` is a call, but `f (x)` and `f\n(x)` are not. Mirrors the lexer's
    /// existing spacing rules for `*` (deref vs multiply) and `-`.
    fn glued_to_prev(&self) -> bool {
        if self.current == 0 {
            return false;
        }
        match (self.peek(), self.tokens.get(self.current - 1)) {
            (Some(next), Some(prev)) => next.span.start() == prev.span.end(),
            _ => false,
        }
    }

    /// Precedence of the next infix/postfix token. A `(` or `[` that is not
    /// glued to the preceding token is not a call/turbofish — it ends the
    /// current expression (and begins a new grouped expression), so it reports
    /// no precedence.
    ///
    /// Line continuation follows Go's rule: a newline ends the current
    /// expression whenever the token before it is "statement-ending" (an
    /// identifier, literal, `return`/`break`, or a closing `)`/`]`/`}`), as if
    /// a semicolon had been inserted there. So `a\n+ b` is two statements but
    /// `a +\nb` continues, and `foo()\n.bar()` breaks the chain while
    /// `foo().\nbar()` keeps it. Continuation is decided purely by the last
    /// token of the line, never by how the next line starts.
    fn next_infix_precedence(&self) -> Precedence {
        if self.current > 0 {
            if let (Some(prev), Some(next)) = (self.tokens.get(self.current - 1), self.peek()) {
                if !self.is_same_line(prev.span.end(), next.span.start())
                    && is_statement_ending(prev.kind)
                {
                    return Precedence::NONE;
                }
            }
        }
        match self.peek_kind() {
            Some(kind @ (TokenKind::LeftParen | TokenKind::LeftBracket)) => {
                if self.glued_to_prev() {
                    get_precedence_for_token(kind)
                } else {
                    Precedence::NONE
                }
            }
            Some(kind) => get_precedence_for_token(kind),
            None => Precedence::NONE,
        }
    }

    fn is_same_line(&self, left_end: usize, right_start: usize) -> bool {
        let start = left_end.min(self.source.len());
        let end = right_start.max(start).min(self.source.len());
        !self.source.as_bytes()[start..end].contains(&b'\n')
    }

    /// Consume an optional semicolon between statements.
    fn consume_optional_semicolon(&mut self) {
        if matches!(self.peek_kind(), Some(TokenKind::Semicolon)) {
            self.advance();
        }
    }
}

#[derive(Default, Clone)]
struct PendingAttrs {
    runtime: Option<String>,
    repr_c: bool,
    entry: bool,
    builtin: bool,
    span_start: Option<usize>,
    span_end: Option<usize>,
}

impl PendingAttrs {
    fn extend_span(&mut self, start: usize, end: usize) {
        self.span_start = Some(self.span_start.map_or(start, |s| s.min(start)));
        self.span_end = Some(self.span_end.map_or(end, |e| e.max(end)));
    }

    fn include_span(&mut self, span: Span) {
        self.extend_span(span.start(), span.end());
    }

    fn finalize_span(&self, fallback_start: usize, fallback_end: usize) -> Span {
        let start = self
            .span_start
            .map_or(fallback_start, |s| s.min(fallback_start));
        let end = self.span_end.map_or(fallback_end, |e| e.max(fallback_end));
        Span::new(start, end)
    }
}

impl<'a> Parser<'a> {
    /// Parse an expression-position attribute like `@dbg(expr)`.
    /// Currently only `@dbg` is supported here.
    fn parse_expr_attribute(&mut self) -> Result<Expr, ParseError> {
        let at_span = self.advance().span; // consume '@'
        let name_tok = self.consume(TokenKind::Identifier, "attribute name")?;
        let name_span = name_tok.span;
        let name = name_span.text(self.source).to_string();
        if name != "dbg" {
            return Err(ParseError::InvalidAttributeUsage {
                message: format!("unknown expression attribute @{name}"),
                span: name_span,
            });
        }
        self.consume(TokenKind::LeftParen, "Expected '(' after @dbg")?;
        let inner = self.parse_expression(Precedence::NONE)?;
        let close = self.consume(TokenKind::RightParen, "Expected ')' after @dbg argument")?;
        let span = at_span.cover(close.span);
        Ok(Expr {
            span,
            ty: Type::Undetermined,
            kind: ExprKind::Dbg(Box::new(inner)),
        })
    }

    fn parse_attributes(&mut self) -> Result<PendingAttrs, ParseError> {
        let mut attrs = PendingAttrs::default();
        loop {
            if !matches!(self.peek_kind(), Some(TokenKind::At)) {
                break;
            }
            let at_token = self.advance(); // consume '@'
            attrs.include_span(at_token.span);
            // Attribute name
            let name_tok = self.consume(TokenKind::Identifier, "attribute name")?;
            let name_span = name_tok.span;
            let name = name_span.text(self.source).to_string();
            attrs.include_span(name_span);
            match name.as_str() {
                "runtime" => {
                    self.consume(TokenKind::LeftParen, "Expected '(' after attribute name")?;
                    let sym_tok =
                        self.consume(TokenKind::StringLiteral, "Expected runtime symbol string")?;
                    let sym_span = sym_tok.span;
                    let sym = sym_span.text(self.source);
                    let sym_clean = sym.trim_matches('"').to_string();
                    self.consume(TokenKind::RightParen, "Expected ')' after attribute")?;
                    if attrs.runtime.is_some() {
                        return Err(ParseError::InvalidAttributeUsage {
                            message: "duplicate @runtime attribute".to_string(),
                            span: name_span,
                        });
                    }
                    attrs.runtime = Some(sym_clean);
                }
                "entry" => {
                    // `@entry` marks the program's wasm entry point; no value.
                    if attrs.entry {
                        return Err(ParseError::InvalidAttributeUsage {
                            message: "duplicate @entry attribute".to_string(),
                            span: name_span,
                        });
                    }
                    attrs.entry = true;
                }
                "builtin" => {
                    // `@builtin type Name[...]` marks an intrinsic opaque type.
                    if attrs.builtin {
                        return Err(ParseError::InvalidAttributeUsage {
                            message: "duplicate @builtin attribute".to_string(),
                            span: name_span,
                        });
                    }
                    attrs.builtin = true;
                }
                "repr" => {
                    self.consume(TokenKind::LeftParen, "Expected '(' after attribute name")?;
                    let arg_tok = self.consume(
                        TokenKind::StringLiteral,
                        "Expected repr string literal (\"C\")",
                    )?;
                    let arg_span = arg_tok.span;
                    let arg_text = arg_span.text(self.source);
                    let val = arg_text.trim_matches('"');
                    if val != "C" {
                        return Err(ParseError::InvalidAttributeUsage {
                            message: "@repr only supports \"C\"".to_string(),
                            span: name_span,
                        });
                    }
                    self.consume(TokenKind::RightParen, "Expected ')' after attribute")?;
                    if attrs.repr_c {
                        return Err(ParseError::InvalidAttributeUsage {
                            message: "duplicate @repr attribute".to_string(),
                            span: name_span,
                        });
                    }
                    attrs.repr_c = true;
                }
                _ => {
                    return Err(ParseError::InvalidAttributeUsage {
                        message: format!("unknown attribute @{}", name),
                        span: name_span,
                    });
                }
            }
            attrs.include_span(self.previous().span);
        }
        Ok(attrs)
    }
}

/// Convert a token to a binary operator, or None if it's not a binary operator
fn token_to_binary_op(token_kind: TokenKind) -> Option<BinaryOp> {
    match token_kind {
        TokenKind::Plus => Some(BinaryOp::Add),
        TokenKind::Minus => Some(BinaryOp::Subtract),
        TokenKind::Star => Some(BinaryOp::Multiply),
        TokenKind::Slash => Some(BinaryOp::Divide),
        TokenKind::Percent => Some(BinaryOp::Modulo),
        TokenKind::DoubleEquals => Some(BinaryOp::Equals),
        TokenKind::NotEquals => Some(BinaryOp::NotEquals),
        TokenKind::Greater => Some(BinaryOp::Greater),
        TokenKind::GreaterEquals => Some(BinaryOp::GreaterEquals),
        TokenKind::Less => Some(BinaryOp::Less),
        TokenKind::LessEquals => Some(BinaryOp::LessEquals),
        TokenKind::Ampersand => Some(BinaryOp::BitAnd),
        TokenKind::Pipe => Some(BinaryOp::BitOr),
        TokenKind::Caret => Some(BinaryOp::BitXor),
        TokenKind::LeftShift => Some(BinaryOp::ShiftLeft),
        TokenKind::RightShift => Some(BinaryOp::ShiftRight),
        _ => None,
    }
}

/// Convert a token to a primitive type, or None if it's not a primitive type keyword
fn token_to_primitive_type(token_kind: TokenKind) -> Option<Type> {
    match token_kind {
        TokenKind::U8 => Some(Type::U8),
        TokenKind::I8 => Some(Type::I8),
        TokenKind::U16 => Some(Type::U16),
        TokenKind::I16 => Some(Type::I16),
        TokenKind::U32 => Some(Type::U32),
        TokenKind::I32 => Some(Type::I32),
        TokenKind::U64 => Some(Type::U64),
        TokenKind::I64 => Some(Type::I64),
        TokenKind::Usize => Some(Type::Usize),
        TokenKind::Isize => Some(Type::Isize),
        TokenKind::F32 => Some(Type::F32),
        TokenKind::F64 => Some(Type::F64),
        TokenKind::Bool => Some(Type::Bool),
        _ => None,
    }
}

/// Get precedence for a specific token kind - standalone function
fn get_precedence_for_token(token_kind: TokenKind) -> Precedence {
    match token_kind {
        TokenKind::PipePipe => Precedence::LOGICAL_OR,
        TokenKind::AmpAmp => Precedence::LOGICAL_AND,
        TokenKind::DoubleEquals | TokenKind::NotEquals => Precedence::EQUALITY,
        TokenKind::Greater | TokenKind::GreaterEquals | TokenKind::Less | TokenKind::LessEquals => {
            Precedence::COMPARISON
        }
        TokenKind::Pipe => Precedence::BIT_OR,
        TokenKind::Caret => Precedence::BIT_XOR,
        TokenKind::Ampersand => Precedence::BIT_AND,
        TokenKind::LeftShift | TokenKind::RightShift => Precedence::SHIFT,
        TokenKind::Plus | TokenKind::Minus => Precedence::ADDITION,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::MULTIPLICATION,
        TokenKind::LeftParen => Precedence::CALL,
        TokenKind::LeftBracket => Precedence::CALL, // turbofish call f[T](...)
        TokenKind::Dot => Precedence::CALL, // Field access has same precedence as function calls
        _ => Precedence::NONE,
    }
}

/// Token kinds that can end a statement, mirroring Go's line-continuation rule:
/// identifiers, literals, the `return`/`break` keywords, and the closing
/// delimiters `)`/`]`/`}`. A newline after any of these terminates the
/// statement; a newline after anything else (a binary operator, `(`, `.`,
/// `,`, `=`, ...) is a continuation. (Primitive type keywords count too, since
/// they appear as identifiers in expression position, e.g. `u8.from_i32`.)
fn is_statement_ending(token_kind: TokenKind) -> bool {
    matches!(
        token_kind,
        TokenKind::IntLiteral
            | TokenKind::FloatLiteral
            | TokenKind::StringLiteral
            | TokenKind::CharLiteral
            | TokenKind::MultilineStringSegment
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Identifier
            | TokenKind::U8
            | TokenKind::I8
            | TokenKind::U16
            | TokenKind::I16
            | TokenKind::U32
            | TokenKind::I32
            | TokenKind::U64
            | TokenKind::I64
            | TokenKind::Usize
            | TokenKind::Isize
            | TokenKind::F32
            | TokenKind::F64
            | TokenKind::Bool
            | TokenKind::Return
            | TokenKind::Break
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_tok::Tokenizer;

    fn parse_expr(input: &str) -> Expr {
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize().unwrap();
        let interner = Interner::new();
        let mut parser = Parser::new(tokens, input, &interner);
        parser.parse_expression(Precedence::NONE).unwrap()
    }

    #[test]
    fn test_precedence_mul_binds_tighter() {
        // 2 + 3 * 4 should parse as 2 + (3 * 4)
        let expr = parse_expr("2 + 3 * 4");
        let ExprKind::Binary { op, right, .. } = &expr.kind else {
            panic!("expected binary");
        };
        assert!(matches!(op, BinaryOp::Add));
        assert!(matches!(
            right.kind,
            ExprKind::Binary {
                op: BinaryOp::Multiply,
                ..
            }
        ));
    }

    #[test]
    fn test_left_associativity() {
        // 1 + 2 + 3 should parse as (1 + 2) + 3
        let expr = parse_expr("1 + 2 + 3");
        let ExprKind::Binary { left, op, .. } = &expr.kind else {
            panic!("expected binary");
        };
        assert!(matches!(op, BinaryOp::Add));
        assert!(matches!(
            left.kind,
            ExprKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        ));
    }
}
