use crate::number::{parse_float_literal, parse_int_literal};
use crate::{
    BinaryOp, Diagnostic, Expr, Function, ImportDecl, ImportSelector, Interner, NamePath,
    Parameter, ParseError, PointerMutability, Program, Severity, Span, Stmt, StructDefinition,
    StructField, StructFieldDefinition, Type,
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
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    source: &'a str,
    module_name: Option<Span>,
    interner: Interner,
    diagnostics: Vec<Diagnostic>,
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

    fn parse_internal(&mut self) -> Result<Program, ParseError> {
        let mut structs = Vec::new();
        let mut functions = Vec::new();
        let mut traits = Vec::new();
        let mut impls = Vec::new();
        let mut imports: Vec<ImportDecl> = Vec::new();

        // Optional module header: mod <identifier>
        if matches!(self.peek_kind(), Some(TokenKind::Mod)) {
            self.advance(); // consume 'mod'
            let name_token =
                self.consume(TokenKind::Identifier, "Expected module name after 'mod'")?;
            self.module_name = Some(name_token.span);
            self.consume_optional_semicolon();
        }

        // Optional imports with optional selectors
        while matches!(self.peek_kind(), Some(TokenKind::Import)) {
            self.advance(); // consume 'import'
            let head =
                self.consume(TokenKind::Identifier, "Expected module name after 'import'")?;
            let mut segments = vec![head.span];
            let mut selector = ImportSelector::All;
            let mut trailing_symbol: Option<Span> = None;

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
                            let name_tok = self.consume(
                                TokenKind::Identifier,
                                "Expected identifier inside import braces",
                            )?;
                            let name_span = name_tok.span;
                            names.push(name_span);
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
                            span: self.current_span(),
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
                    return Err(ParseError::StatementsOutsideFunction);
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
            return Err(ParseError::UnexpectedEof);
        }
        match self.peek_kind() {
            Some(TokenKind::IntLiteral) => {
                let token_span = self.advance().span;
                let literal_text = token_span.text(self.source).to_string();
                let (value, ty) = parse_int_literal(&literal_text, token_span)?;
                Ok(Expr::IntLiteral {
                    span: token_span,
                    value,
                    ty,
                })
            }
            Some(TokenKind::StringLiteral) => {
                let token_span = self.advance().span;
                Ok(Expr::StringLiteral {
                    span: token_span,
                    value: Self::unescape_string_literal(token_span.text(self.source)),
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::FloatLiteral) => {
                let token_span = self.advance().span;
                let literal_text = token_span.text(self.source).to_string();
                let (value, ty) = parse_float_literal(&literal_text, token_span)?;
                Ok(Expr::FloatLiteral {
                    span: token_span,
                    value,
                    ty,
                })
            }
            Some(TokenKind::True) => {
                let token = self.advance();
                Ok(Expr::BoolLiteral {
                    span: token.span,
                    value: true,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::False) => {
                let token = self.advance();
                Ok(Expr::BoolLiteral {
                    span: token.span,
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
                Ok(Expr::ArrayLiteral {
                    elements,
                    span,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::UnaryMinus) => {
                let minus_span = self.advance().span; // consume '-'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = minus_span.cover(operand.span());
                // Represent unary minus as 0 - operand
                Ok(Expr::Binary {
                    left: Box::new(Expr::IntLiteral {
                        span: minus_span,
                        value: 0,
                        ty: Type::Undetermined,
                    }), // placeholder "0"
                    op: BinaryOp::Subtract,
                    right: Box::new(operand),
                    span,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::UnaryPlus) => {
                let plus_span = self.advance().span; // consume '+'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = plus_span.cover(operand.span());
                // Represent unary plus as 0 + operand
                Ok(Expr::Binary {
                    left: Box::new(Expr::IntLiteral {
                        span: plus_span,
                        value: 0,
                        ty: Type::Undetermined,
                    }), // placeholder "0"
                    op: BinaryOp::Add,
                    right: Box::new(operand),
                    span,
                    ty: Type::Undetermined,
                })
            }
            Some(TokenKind::UnaryStar) => {
                let star_span = self.advance().span; // consume '*'
                let operand = self.parse_expression(Precedence::UNARY)?;
                let span = star_span.cover(operand.span());
                Ok(Expr::Dereference {
                    operand: Box::new(operand),
                    span,
                    ty: Type::Undetermined,
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
            Some(_) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: self.peek_kind().unwrap(),
                span: self.current_span(),
            }),
            None => Err(ParseError::UnexpectedEof),
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
            let span = left.span().cover(right.span());
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: binary_op,
                right: Box::new(right),
                span,
                ty: Type::Undetermined,
            });
        }

        match kind {
            TokenKind::LeftParen => {
                // Function call: identifier(args) or qualified: module.ident(args)
                let path = match left {
                    Expr::Identifier { span: name, .. } => vec![name],
                    Expr::FieldAccess { object, field, .. } => {
                        if let Expr::Identifier { span: module, .. } = *object {
                            vec![module, field]
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
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr::FunctionCall {
                    path: crate::NamePath { segments: path },
                    args,
                    ty: Type::Undetermined,
                })
            }
            TokenKind::Dot => {
                self.advance();
                let field_token =
                    self.consume(TokenKind::Identifier, "Expected field name after '.'")?;
                Ok(Expr::FieldAccess {
                    object: Box::new(left),
                    field: field_token.span,
                    ty: Type::Undetermined,
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
        let fn_token = self.consume(TokenKind::Fn, "Expected 'fn'")?;
        let fn_start = fn_token.span.start();

        // Parse function name
        let name_span = {
            let name_token = self.consume(TokenKind::Identifier, "Expected function name")?;
            name_token.span
        };
        let name_text = name_span.text(self.source);
        let name = self.interner.get_or_intern(name_text);

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
                span: name_span,
            });
        }

        // Parse either a declaration (with ';') or a definition with a body
        let (body, span_end) = if matches!(self.peek_kind(), Some(TokenKind::Semicolon)) {
            let semicolon = self.advance(); // consume ';'
            if runtime.is_none() {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "function declarations without body require @runtime attribute"
                        .to_string(),
                    span: name_span,
                });
            }
            (Vec::new(), semicolon.span.end())
        } else {
            if runtime.is_some() {
                return Err(ParseError::InvalidAttributeUsage {
                    message: "@runtime functions must not have a body".to_string(),
                    span: name_span,
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

        // Parse struct name
        let name_token = self.consume(TokenKind::Identifier, "Expected struct name")?;
        let name = name_token.span;

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
        let name_token = self.consume(TokenKind::Identifier, "Expected trait name")?;
        let name = name_token.span;
        self.consume(TokenKind::LeftBrace, "Expected '{' to start trait body")?;

        // Parse zero or more method signatures: fn name(params) [-> type] ;
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn)) {
            self.advance();
            let name_span = {
                let name_tok = self.consume(TokenKind::Identifier, "Expected method name")?;
                name_tok.span
            };
            let name_text = name_span.text(self.source);
            let mname = self.interner.get_or_intern(name_text);
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
        let impl_token = self.consume(TokenKind::Impl, "Expected 'impl'")?;
        let span_start = impl_token.span.start();
        let trait_tok = self.consume(TokenKind::Identifier, "Expected trait name after 'impl'")?;
        let trait_name = trait_tok.span;
        self.consume(TokenKind::For, "Expected 'for' in impl")?;
        let type_tok = self.consume(TokenKind::Identifier, "Expected type name after 'for'")?;
        let struct_name = type_tok.span;
        self.consume(TokenKind::LeftBrace, "Expected '{' to start impl body")?;

        // Parse zero or more method bodies: fn name(params) [-> type] { statements }
        let mut methods = Vec::new();
        while matches!(self.peek_kind(), Some(TokenKind::Fn)) {
            self.advance();
            let name_span = {
                let name_tok = self.consume(TokenKind::Identifier, "Expected method name")?;
                name_tok.span
            };
            let name_text = name_span.text(self.source);
            let mname = self.interner.get_or_intern(name_text);
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
        let name_token = self.consume(TokenKind::Identifier, "Expected field name")?;
        let name = name_token.span;

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
        let name_token = self.consume(TokenKind::Identifier, "Expected field name")?;
        let name = name_token.span;

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
        let name_token = self.consume(TokenKind::Identifier, "Expected parameter name")?;
        let name = name_token.span;

        self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
        let type_annotation = self.parse_type()?;

        Ok(Parameter {
            name,
            type_annotation,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let kind = self.peek_kind().ok_or(ParseError::UnexpectedEof)?;

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
                let token = self.advance();
                Ok(Type::Struct(token.span))
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
                    Some(_) => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "'const' or 'mut' after '*'".to_string(),
                            found: self.peek_kind().unwrap(),
                            span: self.current_span(),
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

        let name_token = self.consume(TokenKind::Identifier, "identifier")?;
        let name = name_token.span;

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
                span: tok.span,
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
        TokenKind::DoubleEquals => Some(BinaryOp::Equals),
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

    fn parse_expr(input: &str) -> Expr {
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, input);
        parser.parse_expression(Precedence::NONE).unwrap()
    }

    #[test]
    fn test_precedence_mul_binds_tighter() {
        // 2 + 3 * 4 should parse as 2 + (3 * 4)
        let Expr::Binary { op, right, .. } = parse_expr("2 + 3 * 4") else {
            panic!("expected binary");
        };
        assert!(matches!(op, BinaryOp::Add));
        assert!(matches!(
            *right,
            Expr::Binary {
                op: BinaryOp::Multiply,
                ..
            }
        ));
    }

    #[test]
    fn test_left_associativity() {
        // 1 + 2 + 3 should parse as (1 + 2) + 3
        let Expr::Binary { left, op, .. } = parse_expr("1 + 2 + 3") else {
            panic!("expected binary");
        };
        assert!(matches!(op, BinaryOp::Add));
        assert!(matches!(
            *left,
            Expr::Binary {
                op: BinaryOp::Add,
                ..
            }
        ));
    }
}
