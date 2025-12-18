use crate::{
    BinaryOp, Expr, Function, ImportDecl, ImportSelector, NamePath, Parameter, ParseError,
    PointerMutability, Program, Span, Stmt, StructDefinition, StructField, StructFieldDefinition,
    Type,
};
use prim_tok::{Token, TokenKind};

/// Precedence levels for operators (higher = tighter binding)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence(pub i32);

impl Precedence {
    pub const NONE: Precedence = Precedence(0);
    pub const EQUALITY: Precedence = Precedence(10); // ==
    pub const ADDITION: Precedence = Precedence(20); // + -
    pub const MULTIPLICATION: Precedence = Precedence(30); // * /
    pub const UNARY: Precedence = Precedence(40); // -x
    pub const CALL: Precedence = Precedence(50); // func()
    pub const PRIMARY: Precedence = Precedence(60); // literals, identifiers
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    source: &'a str,
    module_name: Option<Span>,
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
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        self.parse_internal(true)
    }

    pub fn parse_without_main(&mut self) -> Result<Program, ParseError> {
        self.parse_internal(false)
    }

    fn parse_internal(&mut self, require_main: bool) -> Result<Program, ParseError> {
        let mut structs = Vec::new();
        let mut functions = Vec::new();
        let mut traits = Vec::new();
        let mut impls = Vec::new();
        let mut imports: Vec<ImportDecl> = Vec::new();

        // Skip leading newlines
        self.skip_newlines();

        // Optional module header: mod <identifier>
        if matches!(self.peek_kind(), Some(TokenKind::Mod)) {
            self.advance(); // consume 'mod'
            let name_token =
                self.consume(TokenKind::Identifier, "Expected module name after 'mod'")?;
            self.module_name = Some(name_token.span);
            // terminate with newline/semicolon or end of block
            let _ = self.consume_statement_terminator();
            self.skip_newlines();
        }

        // Optional imports with optional selectors
        while matches!(self.peek_kind(), Some(TokenKind::Import)) {
            self.advance(); // consume 'import'
            self.skip_newlines();
            let head =
                self.consume(TokenKind::Identifier, "Expected module name after 'import'")?;
            let mut segments = vec![head.span];
            let mut selector = ImportSelector::All;
            let mut trailing_symbol: Option<Span> = None;

            loop {
                self.skip_newlines();
                if !matches!(self.peek_kind(), Some(TokenKind::Dot)) {
                    break;
                }
                self.advance(); // consume '.'
                self.skip_newlines();
                match self.peek_kind() {
                    Some(TokenKind::LeftBrace) => {
                        self.advance(); // consume '{'
                        self.skip_newlines();
                        let mut names = Vec::new();
                        loop {
                            let name_tok = self.consume(
                                TokenKind::Identifier,
                                "Expected identifier inside import braces",
                            )?;
                            let name_span = name_tok.span;
                            names.push(name_span);
                            self.skip_newlines();
                            if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                                self.advance();
                                self.skip_newlines();
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
                        let seg_tok = self.advance();
                        let seg_span = seg_tok.span;
                        segments.push(seg_span);
                        trailing_symbol = if segments.len() >= 2 {
                            Some(seg_span)
                        } else {
                            None
                        };
                    }
                    Some(other) => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "identifier or '{' after '.' in import".to_string(),
                            found: other,
                            position: self.position(),
                        });
                    }
                    None => return Err(ParseError::UnexpectedEof),
                }
            }

            imports.push(ImportDecl {
                raw_path: NamePath { segments },
                selector,
                trailing_symbol,
            });
            let _ = self.consume_statement_terminator();
            self.skip_newlines();
        }

        while !self.is_at_end() {
            // Collect any leading attributes
            let attrs = self.parse_attributes()?;

            // Allow struct definitions and function definitions at the top level
            match self.peek_kind() {
                Some(TokenKind::Struct) => {
                    let struct_def = self.parse_struct_with_attrs(attrs)?;
                    structs.push(struct_def);
                    // Skip newlines between definitions
                    self.skip_newlines();
                }
                Some(TokenKind::Fn) => {
                    let function = self.parse_function_with_attrs(attrs)?;
                    functions.push(function);
                    // Skip newlines between functions
                    self.skip_newlines();
                }
                Some(TokenKind::Trait) => {
                    let tr = self.parse_trait_definition()?;
                    traits.push(tr);
                    self.skip_newlines();
                }
                Some(TokenKind::Impl) => {
                    let im = self.parse_impl_definition()?;
                    impls.push(im);
                    self.skip_newlines();
                }
                _ => {
                    // If there is a stray '@' anywhere, surface it as a tokenizer error for compatibility
                    if let Some(tok) = self
                        .tokens
                        .iter()
                        .skip(self.current)
                        .find(|t| t.kind == TokenKind::At)
                    {
                        return Err(ParseError::TokenError(
                            prim_tok::TokenError::UnexpectedCharacter {
                                ch: '@',
                                position: tok.span.start(),
                            },
                        ));
                    }
                    return Err(ParseError::StatementsOutsideFunction);
                }
            }
        }

        // Validate that a main function exists
        if require_main && !functions.iter().any(|f| self.span_text(&f.name) == "main") {
            return Err(ParseError::MissingMainFunction);
        }

        Ok(Program {
            module_name: self.module_name,
            imports,
            structs,
            functions,
            traits,
            impls,
        })
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

    /// Parse a prefix expression - much simpler direct approach
    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEof);
        }
        match self.peek_kind() {
            Some(TokenKind::IntLiteral) => {
                let token_span = {
                    let token = self.advance();
                    token.span
                };
                let owned_source = self.source.to_string();
                let literal_text = token_span.text(&owned_source).to_string();
                let num_part = literal_text
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect::<String>();
                let value: i64 = match num_part.parse() {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(ParseError::InvalidIntegerLiteral {
                            literal: literal_text.clone(),
                            position: token_span.start(),
                        });
                    }
                };
                Ok(Expr::IntLiteral {
                    span: token_span,
                    value,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::StringLiteral) => {
                let token_span = {
                    let token = self.advance();
                    token.span
                };
                Ok(Expr::StringLiteral {
                    span: token_span,
                    value: Self::unescape_string_literal(token_span.text(self.source)),
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::FloatLiteral) => {
                let token = self.advance();
                Ok(Expr::FloatLiteral {
                    span: token.span,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::True) => {
                self.advance();
                Ok(Expr::BoolLiteral {
                    value: true,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::False) => {
                self.advance();
                Ok(Expr::BoolLiteral {
                    value: false,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::Identifier) => {
                let token = self.advance();
                let name = token.span;

                // Check if this is a function call
                if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
                    self.advance(); // consume '('
                    let args = self.parse_argument_list()?;
                    self.consume(TokenKind::RightParen, "Expected ')'")?;
                    Ok(Expr::FunctionCall {
                        path: crate::NamePath::from_single(name),
                        args,
                        ty: Type::Undetermined,
                    })
                } else if matches!(self.peek_kind(), Some(TokenKind::LeftBrace)) {
                    // This is a struct literal
                    self.advance(); // consume '{'
                    let fields = self.parse_struct_literal_fields()?;
                    self.consume(TokenKind::RightBrace, "Expected '}'")?;
                    Ok(Expr::StructLiteral {
                        name,
                        fields,
                        ty: Type::Undetermined,
                    })
                } else {
                    Ok(Expr::Identifier {
                        span: name,
                        ty: Type::Undetermined,
                    })
                }
            }
            Some(TokenKind::Println) => {
                let token = self.advance();
                let name_span = token.span;
                self.consume(TokenKind::LeftParen, "Expected '(' after println")?;
                let args = self.parse_argument_list()?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr::FunctionCall {
                    path: crate::NamePath::from_single(name_span),
                    args,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::LeftParen) => {
                self.advance(); // consume '('
                let expr = self.parse_expression(Precedence::NONE)?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(expr)
            }
            Some(TokenKind::LeftBracket) => {
                // Array literal: [expr, expr, ...]
                self.advance(); // consume '['
                let mut elements = Vec::new();
                // Allow empty literal []
                if !matches!(self.peek_kind(), Some(TokenKind::RightBracket)) {
                    elements.push(self.parse_expression(Precedence::NONE)?);
                    while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                        self.advance(); // consume ','
                        elements.push(self.parse_expression(Precedence::NONE)?);
                    }
                }
                self.consume(
                    TokenKind::RightBracket,
                    "Expected ']' to close array literal",
                )?;
                Ok(Expr::ArrayLiteral {
                    elements,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::Minus) => {
                self.advance(); // consume '-'
                let operand = self.parse_expression(Precedence::UNARY)?;
                // Represent unary minus as 0 - operand
                Ok(Expr::Binary {
                    left: Box::new(Expr::IntLiteral {
                        span: Span::new(0, 1),
                        value: 0,
                        ty: Type::Undetermined,
                    }), // placeholder "0"
                    op: BinaryOp::Subtract,
                    right: Box::new(operand),
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::Star) => {
                self.advance(); // consume '*'
                let operand = self.parse_expression(Precedence::UNARY)?;
                Ok(Expr::Dereference {
                    operand: Box::new(operand),
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::At) => {
                // Treat stray '@' as a tokenizer-level unexpected character to preserve error behavior
                Err(ParseError::TokenError(
                    prim_tok::TokenError::UnexpectedCharacter {
                        ch: '@',
                        position: self.position(),
                    },
                ))
            }
            Some(_) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: self.peek_kind().unwrap_or(TokenKind::Newline),
                position: self.position(),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Parse an infix expression - much simpler direct approach
    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        if let Some(kind) = self.peek_kind() {
            if let Some(binary_op) = token_to_binary_op(kind) {
                let precedence = get_precedence_for_token(kind);
                self.advance(); // consume operator
                let right = self.parse_expression(precedence)?;

                return Ok(Expr::Binary {
                    left: Box::new(left),
                    op: binary_op,
                    right: Box::new(right),
                    ty: Type::Undetermined,
                });
            }
        }
        if matches!(self.peek_kind(), Some(TokenKind::LeftParen)) {
            // Function call: identifier(args) or qualified: module.ident(args)
            let path: Vec<Span> = match left {
                Expr::Identifier { span: name, .. } => vec![name],
                Expr::FieldAccess { object, field, .. } => {
                    if let Expr::Identifier { span: module, .. } = *object {
                        vec![module, field]
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "module name before '.'".to_string(),
                            found: self.peek_kind().unwrap_or(TokenKind::Newline),
                            position: self.position(),
                        });
                    }
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "function name".to_string(),
                        found: self.peek_kind().unwrap_or(TokenKind::Newline),
                        position: self.position(),
                    });
                }
            };
            self.advance(); // consume '('
            let args = self.parse_argument_list()?;
            self.consume(TokenKind::RightParen, "Expected ')'")?;
            Ok(Expr::FunctionCall {
                path: crate::NamePath { segments: path },
                args,
                ty: Type::Undetermined,
            })
        } else if matches!(self.peek_kind(), Some(TokenKind::Dot)) {
            // Field access: expr.field
            self.advance(); // consume '.'
            let field_token =
                self.consume(TokenKind::Identifier, "Expected field name after '.'")?;
            let field = field_token.span;
            Ok(Expr::FieldAccess {
                object: Box::new(left),
                field,
                ty: Type::Undetermined,
            })
        } else {
            Ok(left) // No infix operator, return left as-is
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
        let fn_token = self.consume(TokenKind::Fn, "Expected 'fn'")?;
        let fn_start = fn_token.span.start();
        self.skip_newlines();

        // Parse function name
        let name_token = self.consume(TokenKind::Identifier, "Expected function name")?;
        let name = name_token.span;
        self.skip_newlines();

        // Parse parameter list
        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
        self.skip_newlines();

        // Parse optional return type
        let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
            self.advance(); // consume '->'
            self.skip_newlines();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.skip_newlines();

        // Validate attributes on function
        if repr_c {
            return Err(ParseError::InvalidAttributeUsage {
                message: "@repr is only valid on structs".to_string(),
                position: name.start(),
            });
        }

        // Parse either a declaration (with ';') or a definition with a body
        let (body, span_end) = if matches!(self.peek_kind(), Some(TokenKind::Semicolon)) {
            let semicolon = self.advance(); // consume ';'
            if runtime.is_none() {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "function declarations without body require @runtime attribute"
                        .to_string(),
                    position: name.start(),
                });
            }
            (Vec::new(), semicolon.span.end())
        } else {
            if runtime.is_some() {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "@runtime functions must not have a body".to_string(),
                    position: name.start(),
                });
            }
            self.consume(TokenKind::LeftBrace, "Expected '{' to start function body")?;
            let body = self.parse_statement_list()?;
            let right_brace =
                self.consume(TokenKind::RightBrace, "Expected '}' to end function body")?;
            (body, right_brace.span.end())
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
        let struct_token = self.consume(TokenKind::Struct, "Expected 'struct'")?;
        let struct_start = struct_token.span.start();
        self.skip_newlines();

        // Parse struct name
        let name_token = self.consume(TokenKind::Identifier, "Expected struct name")?;
        let name = name_token.span;
        self.skip_newlines();

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
        let trait_token = self.consume(TokenKind::Trait, "Expected 'trait'")?;
        let span_start = trait_token.span.start();
        self.skip_newlines();
        let name_token = self.consume(TokenKind::Identifier, "Expected trait name")?;
        let name = name_token.span;
        self.skip_newlines();
        self.consume(TokenKind::LeftBrace, "Expected '{' to start trait body")?;
        self.skip_newlines();

        // Parse zero or more method signatures: fn name(params) [-> type] ;
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn)) {
            self.advance();
            self.skip_newlines();
            let name_tok = self.consume(TokenKind::Identifier, "Expected method name")?;
            let mname = name_tok.span;
            self.skip_newlines();
            self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
            let parameters = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
            self.skip_newlines();
            let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
                self.advance();
                self.skip_newlines();
                Some(self.parse_type()?)
            } else {
                None
            };
            self.skip_newlines();
            self.consume(
                TokenKind::Semicolon,
                "Expected ';' after trait method signature",
            )?;
            methods.push(crate::TraitMethod {
                name: mname,
                parameters,
                return_type,
            });
            self.skip_newlines();
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expected '}' to end trait body")?;
        Ok(crate::TraitDefinition {
            name,
            methods,
            span: crate::Span::new(span_start, right_brace.span.end()),
        })
    }

    fn parse_impl_definition(&mut self) -> Result<crate::ImplDefinition, ParseError> {
        let impl_token = self.consume(TokenKind::Impl, "Expected 'impl'")?;
        let span_start = impl_token.span.start();
        self.skip_newlines();
        let trait_tok = self.consume(TokenKind::Identifier, "Expected trait name after 'impl'")?;
        let trait_name = trait_tok.span;
        self.skip_newlines();
        self.consume(TokenKind::For, "Expected 'for' in impl")?;
        self.skip_newlines();
        let type_tok = self.consume(TokenKind::Identifier, "Expected type name after 'for'")?;
        let struct_name = type_tok.span;
        self.skip_newlines();
        self.consume(TokenKind::LeftBrace, "Expected '{' to start impl body")?;
        self.skip_newlines();

        // Parse zero or more method bodies: fn name(params) [-> type] { statements }
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn)) {
            self.advance();
            self.skip_newlines();
            let name_tok = self.consume(TokenKind::Identifier, "Expected method name")?;
            let mname = name_tok.span;
            self.skip_newlines();
            self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
            let parameters = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
            self.skip_newlines();
            let return_type = if matches!(self.peek_kind(), Some(TokenKind::Arrow)) {
                self.advance();
                self.skip_newlines();
                Some(self.parse_type()?)
            } else {
                None
            };
            self.skip_newlines();
            self.consume(TokenKind::LeftBrace, "Expected '{' to start method body")?;
            let body = self.parse_statement_list()?;
            self.consume(TokenKind::RightBrace, "Expected '}' to end method body")?;
            methods.push(crate::ImplMethod {
                name: mname,
                parameters,
                return_type,
                body,
            });
            self.skip_newlines();
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

        self.skip_newlines();

        // Handle empty field list
        if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            return Ok(fields);
        }

        // Parse first field
        fields.push(self.parse_struct_field_definition()?);

        // Parse remaining fields
        while {
            self.skip_newlines();
            matches!(self.peek_kind(), Some(TokenKind::Comma))
        } {
            self.advance(); // consume ','
            self.skip_newlines();

            // Allow trailing comma
            if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                break;
            }

            fields.push(self.parse_struct_field_definition()?);
        }

        self.skip_newlines();
        Ok(fields)
    }

    fn parse_struct_field_definition(&mut self) -> Result<StructFieldDefinition, ParseError> {
        let name_token = self.consume(TokenKind::Identifier, "Expected field name")?;
        let name = name_token.span;
        self.skip_newlines();

        self.consume(TokenKind::Colon, "Expected ':' after field name")?;
        self.skip_newlines();
        let field_type = self.parse_type()?;

        Ok(StructFieldDefinition { name, field_type })
    }

    fn parse_struct_literal_fields(&mut self) -> Result<Vec<StructField>, ParseError> {
        let mut fields = Vec::new();

        self.skip_newlines();

        // Handle empty field list
        if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
            return Ok(fields);
        }

        // Parse first field
        fields.push(self.parse_struct_literal_field()?);

        // Parse remaining fields
        while {
            self.skip_newlines();
            matches!(self.peek_kind(), Some(TokenKind::Comma))
        } {
            self.advance(); // consume ','
            self.skip_newlines();

            // Allow trailing comma
            if matches!(self.peek_kind(), Some(TokenKind::RightBrace)) {
                break;
            }

            fields.push(self.parse_struct_literal_field()?);
        }

        self.skip_newlines();
        Ok(fields)
    }

    fn parse_struct_literal_field(&mut self) -> Result<StructField, ParseError> {
        let name_token = self.consume(TokenKind::Identifier, "Expected field name")?;
        let name = name_token.span;
        self.skip_newlines();

        self.consume(TokenKind::Equals, "Expected '=' after field name")?;
        self.skip_newlines();
        let value = self.parse_expression(Precedence::NONE)?;

        Ok(StructField { name, value })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();

        self.skip_newlines();

        // Handle empty parameter list
        if matches!(self.peek_kind(), Some(TokenKind::RightParen)) {
            return Ok(parameters);
        }

        // Parse first parameter
        parameters.push(self.parse_parameter()?);

        // Parse remaining parameters
        while {
            self.skip_newlines();
            matches!(self.peek_kind(), Some(TokenKind::Comma))
        } {
            self.advance(); // consume ','
            self.skip_newlines();
            parameters.push(self.parse_parameter()?);
        }

        self.skip_newlines();
        Ok(parameters)
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let name_token = self.consume(TokenKind::Identifier, "Expected parameter name")?;
        let name = name_token.span;
        self.skip_newlines();

        self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
        self.skip_newlines();
        let type_annotation = self.parse_type()?;

        Ok(Parameter {
            name,
            type_annotation,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.peek_kind() {
            None => Err(ParseError::UnexpectedEof),
            Some(TokenKind::U8) => {
                self.advance();
                Ok(Type::U8)
            }
            Some(TokenKind::I8) => {
                self.advance();
                Ok(Type::I8)
            }
            Some(TokenKind::U16) => {
                self.advance();
                Ok(Type::U16)
            }
            Some(TokenKind::I16) => {
                self.advance();
                Ok(Type::I16)
            }
            Some(TokenKind::U32) => {
                self.advance();
                Ok(Type::U32)
            }
            Some(TokenKind::I32) => {
                self.advance();
                Ok(Type::I32)
            }
            Some(TokenKind::U64) => {
                self.advance();
                Ok(Type::U64)
            }
            Some(TokenKind::I64) => {
                self.advance();
                Ok(Type::I64)
            }
            Some(TokenKind::Usize) => {
                self.advance();
                Ok(Type::Usize)
            }
            Some(TokenKind::Isize) => {
                self.advance();
                Ok(Type::Isize)
            }
            Some(TokenKind::F32) => {
                self.advance();
                Ok(Type::F32)
            }
            Some(TokenKind::F64) => {
                self.advance();
                Ok(Type::F64)
            }
            Some(TokenKind::Bool) => {
                self.advance();
                Ok(Type::Bool)
            }
            Some(TokenKind::LeftBracket) => {
                // Dynamic array type: [T]
                self.advance(); // consume '['
                let elem_ty = self.parse_type()?;
                self.consume(
                    TokenKind::RightBracket,
                    "Expected ']' after array element type",
                )?;
                Ok(Type::Array(Box::new(elem_ty)))
            }
            Some(TokenKind::Identifier) => {
                // This could be a struct type reference
                let token = self.advance();
                Ok(Type::Struct(token.span))
            }
            Some(TokenKind::Star) => {
                // Parse pointer type: *const T or *mut T
                self.advance(); // consume '*'

                let mutability = match self.peek_kind() {
                    Some(TokenKind::Const) => {
                        self.advance(); // consume 'const'
                        PointerMutability::Const
                    }
                    Some(TokenKind::Mut) => {
                        self.advance(); // consume 'mut'
                        PointerMutability::Mutable
                    }
                    Some(_) => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "'const' or 'mut' after '*'".to_string(),
                            found: self.peek_kind().unwrap_or(TokenKind::Newline),
                            position: self.position(),
                        });
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };

                let pointee = Box::new(self.parse_type()?);
                Ok(Type::Pointer {
                    mutability,
                    pointee,
                })
            }
            Some(_) => Err(ParseError::UnexpectedToken {
                expected: "type".to_string(),
                found: self.peek_kind().unwrap_or(TokenKind::Newline),
                position: self.position(),
            }),
        }
    }

    fn parse_statement_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        // Skip leading newlines
        self.skip_newlines();

        while let Some(kind) = self.peek_kind() {
            if kind == TokenKind::RightBrace {
                break;
            }
            statements.push(self.parse_statement()?);

            // After each statement, require a terminator or end of block
            self.consume_statement_terminator()?;

            // Skip any additional newlines
            self.skip_newlines();
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Let) => self.parse_let_statement(),
            Some(TokenKind::Loop) => self.parse_loop_statement(),
            Some(TokenKind::Break) => self.parse_break_statement(),
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
        self.skip_newlines();

        let name_token = self.consume(TokenKind::Identifier, "identifier")?;
        let name = name_token.span;
        self.skip_newlines();

        // Optional type annotation
        let type_annotation = if matches!(self.peek_kind(), Some(TokenKind::Colon)) {
            self.advance(); // consume ':'
            self.skip_newlines();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.skip_newlines();

        self.consume(TokenKind::Equals, "Expected '=' in let statement")?;
        self.skip_newlines();
        let value = self.parse_expression(Precedence::NONE)?;
        // Terminator will be handled by parse_statement_list

        Ok(Stmt::Let {
            name,
            type_annotation,
            value,
        })
    }

    fn parse_loop_statement(&mut self) -> Result<Stmt, ParseError> {
        let loop_start = {
            let token = self.consume(TokenKind::Loop, "Expected 'loop'")?;
            token.span.start()
        };
        self.skip_newlines();
        self.consume(TokenKind::LeftBrace, "Expected '{' after 'loop'")?;
        let body = self.parse_statement_list()?;
        let end = self.consume(TokenKind::RightBrace, "Expected '}' to end loop body")?;

        Ok(Stmt::Loop {
            body,
            span: Span::new(loop_start, end.span.end()),
        })
    }

    fn parse_break_statement(&mut self) -> Result<Stmt, ParseError> {
        let token = self.consume(TokenKind::Break, "Expected 'break'")?;
        Ok(Stmt::Break { span: token.span })
    }

    fn consume(&mut self, expected: TokenKind, message: &str) -> Result<&Token, ParseError> {
        match self.tokens.get(self.current) {
            Some(tok) if tok.kind == expected => Ok(self.advance()),
            Some(tok) => Err(ParseError::UnexpectedToken {
                expected: message.to_string(),
                found: tok.kind,
                position: tok.span.start(),
            }),
            None => Err(ParseError::UnexpectedEof),
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

    #[inline]
    fn peek_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.current).map(|t| t.kind)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    #[inline]
    fn position(&self) -> usize {
        self.peek()
            .map(|t| t.span.start())
            .unwrap_or_else(|| self.source.len())
    }

    fn span_text(&self, span: &Span) -> &str {
        span.text(self.source)
    }

    /// Skip newline tokens (used when newlines are not significant)
    fn skip_newlines(&mut self) {
        while matches!(self.peek_kind(), Some(TokenKind::Newline)) {
            self.advance();
        }
    }

    /// Consume a statement terminator (semicolon, newline, or end of block)
    fn consume_statement_terminator(&mut self) -> Result<(), ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Semicolon) => {
                self.advance();
                Ok(())
            }
            Some(TokenKind::Newline) => {
                self.advance();
                Ok(())
            }
            Some(TokenKind::RightBrace) => {
                // End of block terminates statement
                Ok(())
            }
            None => Ok(()),
            _ => Err(ParseError::UnexpectedToken {
                expected: "';', newline, or '}' after statement".to_string(),
                found: self.peek_kind().unwrap_or(TokenKind::Newline),
                position: self.position(),
            }),
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
    fn record_start(&mut self, pos: usize) {
        match self.span_start {
            Some(current) if pos < current => self.span_start = Some(pos),
            None => self.span_start = Some(pos),
            _ => {}
        }

        if let Some(current_end) = self.span_end {
            if pos > current_end {
                self.span_end = Some(pos);
            }
        }
    }

    fn record_end(&mut self, pos: usize) {
        match self.span_end {
            Some(current) if pos > current => self.span_end = Some(pos),
            None => self.span_end = Some(pos),
            _ => {}
        }

        if let Some(current_start) = self.span_start {
            if pos < current_start {
                self.span_start = Some(pos);
            }
        }
    }

    fn include_span(&mut self, span: Span) {
        self.record_start(span.start());
        self.record_end(span.end());
    }

    fn finalize_span(&self, fallback_start: usize, fallback_end: usize) -> Span {
        let start = self
            .span_start
            .map(|value| value.min(fallback_start))
            .unwrap_or(fallback_start);
        let end = self
            .span_end
            .map(|value| value.max(fallback_end))
            .unwrap_or(fallback_end);
        Span::new(start, end)
    }
}

impl<'a> Parser<'a> {
    fn parse_attributes(&mut self) -> Result<PendingAttrs, ParseError> {
        let mut attrs = PendingAttrs::default();
        loop {
            self.skip_newlines();
            if !matches!(self.peek_kind(), Some(TokenKind::At)) {
                break;
            }
            let at_token = self.advance(); // consume '@'
            attrs.record_start(at_token.span.start());
            attrs.record_end(at_token.span.end());
            // Attribute name
            let name_tok = self.consume(TokenKind::Identifier, "attribute name")?;
            let name_span = name_tok.span;
            let name = self.span_text(&name_span).to_string();
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
                            position: name_span.start(),
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
                            position: name_span.start(),
                        });
                    }
                    self.consume(TokenKind::RightParen, "Expected ')' after attribute")?;
                    if attrs.repr_c {
                        return Err(ParseError::InvalidAttributeUsage {
                            message: "duplicate @repr attribute".to_string(),
                            position: name_span.start(),
                        });
                    }
                    attrs.repr_c = true;
                }
                _ => {
                    return Err(ParseError::InvalidAttributeUsage {
                        message: format!("unknown attribute @{}", name),
                        position: name_span.start(),
                    });
                }
            }
            let end_pos = self.previous().span.end();
            attrs.record_end(end_pos);
            self.skip_newlines();
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
        TokenKind::DoubleEquals => Some(BinaryOp::Equals),
        _ => None,
    }
}

/// Get precedence for a specific token kind - standalone function
fn get_precedence_for_token(token_kind: TokenKind) -> Precedence {
    match token_kind {
        TokenKind::DoubleEquals => Precedence::EQUALITY,
        TokenKind::Plus | TokenKind::Minus => Precedence::ADDITION,
        TokenKind::Star | TokenKind::Slash => Precedence::MULTIPLICATION,
        TokenKind::LeftParen => Precedence::CALL,
        TokenKind::Dot => Precedence::CALL, // Field access has same precedence as function calls
        _ => Precedence::NONE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prim_tok::Tokenizer;

    #[test]
    fn test_pratt_parser_direct() {
        let test_cases = vec![
            ("2 + 3", "Simple addition"),
            ("2 * 3", "Simple multiplication"),
            ("2 + 3 * 4", "Addition and multiplication with precedence"),
            ("2 * 3 + 4", "Multiplication and addition with precedence"),
            ("1 + 2 + 3", "Left associative addition"),
            ("(2 + 3) * 4", "Parentheses"),
        ];

        for (input, description) in test_cases {
            println!("Testing: {} - {}", input, description);

            let mut tokenizer = Tokenizer::new(input);
            let tokens = tokenizer.tokenize().expect("Failed to tokenize");
            let mut parser = Parser::new(tokens, input);

            match parser.parse_expression(Precedence::NONE) {
                Ok(expr) => {
                    println!("  ✓ Parsed: {:?}", expr);

                    // Specific test for precedence
                    if input == "2 + 3 * 4" {
                        if let Expr::Binary {
                            left: _,
                            op: BinaryOp::Add,
                            right,
                            ..
                        } = &expr
                        {
                            if let Expr::Binary {
                                op: BinaryOp::Multiply,
                                ..
                            } = &**right
                            {
                                println!("    ✓ Correct precedence: 2 + (3 * 4)");
                            } else {
                                panic!("Right should be multiplication");
                            }
                        } else {
                            panic!("Should be addition at top level");
                        }
                    }
                }
                Err(e) => {
                    panic!("Failed to parse '{}': {:?}", input, e);
                }
            }
        }
    }
}
