use crate::number::{parse_float_literal, parse_int_literal};
use crate::{
    BinaryOp, Block, Diagnostic, Expr, ExprKind, Function, Ident, ImportDecl, ImportSelector,
    Interner, NamePath, Parameter, ParseError, PointerMutability, Program, Severity, Span, Stmt,
    StructDefinition, StructField, StructFieldDefinition, Type,
};
use prim_tok::{Token, TokenKind};

/// Precedence levels for operators (higher = tighter binding)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence(pub i32);

impl Precedence {
    pub const NONE: Precedence = Precedence(0);
    pub const EQUALITY: Precedence = Precedence(10); // == !=
    pub const COMPARISON: Precedence = Precedence(15); // > < >= <=
    pub const ADDITION: Precedence = Precedence(20); // + -
    pub const MULTIPLICATION: Precedence = Precedence(30); // * /
    pub const UNARY: Precedence = Precedence(40); // -x
    pub const CALL: Precedence = Precedence(50); // func()
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    source: &'a str,
    module_name: Option<Ident>,
    interner: Interner,
    diagnostics: Vec<Diagnostic>,
    /// Whether struct literals are allowed in the current expression context.
    /// Disabled when parsing if/while conditions to avoid ambiguity with block braces.
    allow_struct_literal: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
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
            interner: Interner::new(),
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
    fn intern(&mut self, span: Span) -> crate::InternSymbol {
        let text = span.text(self.source);
        self.interner.get_or_intern(text)
    }

    /// Create an identifier from a span.
    fn ident(&mut self, span: Span) -> Ident {
        Ident {
            sym: self.intern(span),
            span,
        }
    }

    fn parse_internal(&mut self) -> Result<Program, ParseError> {
        let mut structs = Vec::new();
        let mut functions = Vec::new();
        let mut traits = Vec::new();
        let mut impls = Vec::new();
        let mut imports: Vec<ImportDecl> = Vec::new();

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
                Some(TokenKind::Fn) => {
                    let function = self.parse_function_with_attrs(attrs)?;
                    functions.push(function);
                }
                Some(TokenKind::Trait) => {
                    let tr = self.parse_trait_definition()?;
                    traits.push(tr);
                }
                Some(TokenKind::Impl) => {
                    let im = self.parse_impl_definition()?;
                    impls.push(im);
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
            functions,
            traits,
            impls,
            interner: std::mem::replace(&mut self.interner, Interner::new()),
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
        while self
            .peek_kind()
            .map(get_precedence_for_token)
            .unwrap_or(Precedence::NONE)
            > min_precedence
        {
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

                // Check if this is a function call
                if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
                    self.advance(); // consume '('
                    let args = self.parse_argument_list()?;
                    let end_span = self.consume(TokenKind::RightParen, "Expected ')'")?;
                    let span = ident.span.cover(end_span.span);
                    Ok(Expr {
                        span,
                        ty: Type::Undetermined,
                        kind: ExprKind::FunctionCall {
                            path: NamePath::from_single(ident),
                            args,
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
                self.advance(); // consume '('
                let expr = self.parse_expression(Precedence::NONE)?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(expr)
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
            Some(TokenKind::UnaryMinus) => {
                let minus_span = self.advance().span; // consume '-'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = minus_span.cover(operand.span);
                // Represent unary minus as 0 - operand
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Binary {
                        left: Box::new(Expr {
                            span: minus_span,
                            ty: Type::Undetermined,
                            kind: ExprKind::Int(0),
                        }),
                        op: BinaryOp::Subtract,
                        right: Box::new(operand),
                    },
                })
            }
            Some(TokenKind::UnaryPlus) => {
                let plus_span = self.advance().span; // consume '+'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = plus_span.cover(operand.span);
                // Represent unary plus as 0 + operand
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::Binary {
                        left: Box::new(Expr {
                            span: plus_span,
                            ty: Type::Undetermined,
                            kind: ExprKind::Int(0),
                        }),
                        op: BinaryOp::Add,
                        right: Box::new(operand),
                    },
                })
            }
            Some(TokenKind::UnaryStar) => {
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
            Some(TokenKind::At) => {
                // Treat stray '@' as a tokenizer-level unexpected character to preserve error behavior
                Err(ParseError::TokenError(
                    prim_tok::TokenError::UnexpectedCharacter {
                        ch: '@',
                        span: self.current_span(),
                    },
                ))
            }
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
                // Function call: identifier(args) or qualified: module.ident(args)
                let (path, start_span) = match &left.kind {
                    ExprKind::Ident(ident) => (vec![*ident], left.span),
                    ExprKind::FieldAccess { object, field } => {
                        if let ExprKind::Ident(obj_ident) = &object.kind {
                            (vec![*obj_ident, *field], object.span)
                        } else {
                            return Err(ParseError::UnexpectedToken {
                                expected: "module name before '.'".to_string(),
                                found: kind,
                                span: self.current_span(),
                            });
                        }
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "function name".to_string(),
                            found: kind,
                            span: self.current_span(),
                        });
                    }
                };
                self.advance();
                let args = self.parse_argument_list()?;
                let end_span = self.consume(TokenKind::RightParen, "Expected ')'")?;
                let span = start_span.cover(end_span.span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::FunctionCall {
                        path: NamePath { segments: path },
                        args,
                    },
                })
            }
            TokenKind::Dot => {
                self.advance();
                let field_span = self
                    .consume(TokenKind::Identifier, "Expected field name after '.'")?
                    .span;
                let field = self.ident(field_span);
                let span = left.span.cover(field.span);
                Ok(Expr {
                    span,
                    ty: Type::Undetermined,
                    kind: ExprKind::FieldAccess {
                        object: Box::new(left),
                        field,
                    },
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
            return Ok(args); // Empty argument list
        }

        // Parse first argument
        args.push(self.parse_expression(Precedence::NONE)?);

        // Parse remaining arguments
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.advance(); // consume ','
            args.push(self.parse_expression(Precedence::NONE)?);
        }

        Ok(args)
    }

    // Helper methods
    fn parse_function_with_attrs(
        &mut self,
        mut attrs: PendingAttrs,
    ) -> Result<Function, ParseError> {
        let runtime = attrs.runtime.take();
        let repr_c = attrs.repr_c;

        // Consume 'fn' keyword
        let fn_start = self.consume(TokenKind::Fn, "Expected 'fn'")?.span.start();

        // Parse function name
        let name_span = self
            .consume(TokenKind::Identifier, "Expected function name")?
            .span;
        let name = self.ident(name_span);

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
            parameters,
            return_type,
            body,
            runtime_binding: runtime,
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

        // Parse struct name
        let name_span = self
            .consume(TokenKind::Identifier, "Expected struct name")?
            .span;
        let name = self.ident(name_span);

        // Parse struct body
        self.consume(TokenKind::LeftBrace, "Expected '{' to start struct body")?;
        let fields = self.parse_struct_field_list()?;
        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end struct body")?;
        let struct_end = right_brace.span.end();

        let full_span = attrs.finalize_span(struct_start, struct_end);

        Ok(StructDefinition {
            name,
            fields,
            repr_c,
            span: full_span,
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
            let parameters = self.parse_parameter_list()?;
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
        let trait_name_span = self
            .consume(TokenKind::Identifier, "Expected trait name after 'impl'")?
            .span;
        let trait_name = self.ident(trait_name_span);
        self.consume(TokenKind::For, "Expected 'for' in impl")?;
        let struct_name_span = self
            .consume(TokenKind::Identifier, "Expected type name after 'for'")?
            .span;
        let struct_name = self.ident(struct_name_span);
        self.consume(TokenKind::LeftBrace, "Expected '{' to start impl body")?;

        // Parse zero or more method bodies: fn name(params) [-> type] { statements }
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn)) {
            self.advance();
            let mname_span = self
                .consume(TokenKind::Identifier, "Expected method name")?
                .span;
            let mname = self.ident(mname_span);
            self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
            let parameters = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
            let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };
            self.consume(TokenKind::LeftBrace, "Expected '{' to start method body")?;
            let body = self.parse_statement_list()?;
            self.consume(TokenKind::RightBrace, "Expected '}' to end method body")?;
            methods.push(crate::ImplMethod {
                name: mname,
                parameters,
                return_type,
                body,
            });
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end impl body")?;
        Ok(crate::ImplDefinition {
            trait_name,
            struct_name,
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

    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let name_span = self
            .consume(TokenKind::Identifier, "Expected parameter name")?
            .span;
        let name = self.ident(name_span);

        self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
        let type_annotation = self.parse_type()?;

        Ok(Parameter {
            name,
            type_annotation,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let kind = self.peek_kind().ok_or(ParseError::UnexpectedEof {
            span: self.current_span(),
        })?;

        // Handle primitive types with a simple lookup
        if let Some(ty) = token_to_primitive_type(kind) {
            self.advance();
            return Ok(ty);
        }

        match kind {
            TokenKind::LeftBracket => {
                self.advance(); // consume '['
                let elem_ty = self.parse_type()?;
                self.consume(
                    TokenKind::RightBracket,
                    "Expected ']' after array element type",
                )?;
                Ok(Type::Array(Box::new(elem_ty)))
            }
            TokenKind::Identifier => {
                let span = self.advance().span;
                let name = self.intern(span);
                Ok(Type::Struct(name))
            }
            TokenKind::UnaryStar => {
                self.advance(); // consume '*'
                let mutability = match self.peek_kind() {
                    Some(TokenKind::Const) => {
                        self.advance();
                        PointerMutability::Const
                    }
                    Some(TokenKind::Mut) => {
                        self.advance();
                        PointerMutability::Mutable
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
                Ok(Type::Pointer {
                    mutability,
                    pointee,
                })
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
        let mut trailing_expr: Option<Box<Expr>> = None;

        while let Some(kind) = self.peek_kind() {
            if kind == TokenKind::RightBrace {
                break;
            }

            // Try to parse a statement
            let stmt = self.parse_statement()?;
            let stmt_end = self.previous().span.end();
            let has_semicolon = matches!(self.peek_kind(), Some(TokenKind::Semicolon));

            if has_semicolon {
                self.advance();
                stmts.push(stmt);
            } else if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                // This is the last item in the block without a semicolon
                // If it's an expression statement, it becomes the trailing expression
                match stmt {
                    Stmt::Expr(expr) => {
                        trailing_expr = Some(Box::new(expr));
                    }
                    other => {
                        // Non-expression statements at the end still need to be stored
                        stmts.push(other);
                    }
                }
            } else {
                // No semicolon and not at closing brace - emit error
                if let Some(next) = self.peek() {
                    if self.is_same_line(stmt_end, next.span.start()) {
                        self.emit(
                            "statements on the same line should be separated by a semicolon",
                            next.span,
                            Severity::Error,
                        );
                    }
                }
                stmts.push(stmt);
            }
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}'")?;
        let block_end = right_brace.span.end();

        Ok(Block {
            stmts,
            expr: trailing_expr,
            span: Span::new(block_start, block_end),
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Let) => self.parse_let_statement(),
            Some(TokenKind::Loop) => self.parse_loop_statement(),
            Some(TokenKind::While) => self.parse_while_statement(),
            Some(TokenKind::Break) => self.parse_break_statement(),
            Some(TokenKind::Identifier) => {
                // Check if this is an assignment (identifier followed by '=')
                // We need to look ahead to distinguish `x = value` from `x + y`
                let saved_pos = self.current;
                let ident_token = self.advance();
                let ident_span = ident_token.span;
                if matches!(self.peek_kind(), Some(TokenKind::Equals)) {
                    let target = self.ident(ident_span);
                    self.advance(); // consume '='
                    let value = self.parse_expression(Precedence::NONE)?;
                    Ok(Stmt::Assign { target, value })
                } else {
                    // Not an assignment, backtrack and parse as expression
                    self.current = saved_pos;
                    let expr = self.parse_expression(Precedence::NONE)?;
                    Ok(Stmt::Expr(expr))
                }
            }
            _ => {
                // Expression statement (no semicolon required)
                let expr = self.parse_expression(Precedence::NONE)?;
                Ok(Stmt::Expr(expr))
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

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::Let, "Expected 'let'")?;

        let mutable = matches!(self.peek_kind(), Some(TokenKind::Mut));
        if mutable {
            self.advance();
        }

        let name_span = self.consume(TokenKind::Identifier, "identifier")?.span;
        let name = self.ident(name_span);

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
            name,
            mutable,
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

    fn parse_break_statement(&mut self) -> Result<Stmt, ParseError> {
        let token = self.consume(TokenKind::Break, "Expected 'break'")?;
        Ok(Stmt::Break { span: token.span })
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

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn current_span(&self) -> Span {
        self.peek()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::new(self.source.len(), self.source.len()))
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
            self.consume(TokenKind::LeftParen, "Expected '(' after attribute name")?;
            match name.as_str() {
                "runtime" => {
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
                "repr" => {
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
        TokenKind::DoubleEquals | TokenKind::NotEquals => Precedence::EQUALITY,
        TokenKind::Greater | TokenKind::GreaterEquals | TokenKind::Less | TokenKind::LessEquals => {
            Precedence::COMPARISON
        }
        TokenKind::Plus | TokenKind::Minus => Precedence::ADDITION,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::MULTIPLICATION,
        TokenKind::LeftParen => Precedence::CALL,
        TokenKind::Dot => Precedence::CALL, // Field access has same precedence as function calls
        _ => Precedence::NONE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_tok::Tokenizer;

    fn parse_expr(input: &str) -> Expr {
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, input);
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
