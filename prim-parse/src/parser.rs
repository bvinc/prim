use crate::{BinaryOp, Expr, Function, Parameter, ParseError, Program, Span, Stmt, Type};
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
    tokens: Vec<Token<'a>>,
    current: usize,
    source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>, source: &'a str) -> Self {
        // Filter out comment tokens so the parser never sees them
        let tokens = tokens
            .into_iter()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();

        Self {
            tokens,
            current: 0,
            source,
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut functions = Vec::new();

        // Skip leading newlines
        self.skip_newlines();

        while !self.is_at_end() {
            // Only allow function definitions at the top level
            match self.peek().kind {
                TokenKind::Fn => {
                    let function = self.parse_function()?;
                    functions.push(function);
                    // Skip newlines between functions
                    self.skip_newlines();
                }
                _ => {
                    return Err(ParseError::StatementsOutsideFunction);
                }
            }
        }

        // Validate that a main function exists
        if !functions.iter().any(|f| self.span_text(&f.name) == "main") {
            return Err(ParseError::MissingMainFunction);
        }

        Ok(Program { functions })
    }

    /// Parse an expression with minimum precedence
    pub fn parse_expression(&mut self, min_precedence: Precedence) -> Result<Expr, ParseError> {
        // Parse prefix expression
        let mut left = self.parse_prefix()?;

        // Parse infix expressions while precedence is sufficient
        while get_precedence_for_token(self.peek().kind) > min_precedence {
            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    /// Parse a prefix expression - much simpler direct approach
    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.peek().kind {
            TokenKind::IntLiteral => {
                let token = self.advance();
                Ok(Expr::IntLiteral(Self::token_span(token)))
            }
            TokenKind::FloatLiteral => {
                let token = self.advance();
                Ok(Expr::FloatLiteral(Self::token_span(token)))
            }
            TokenKind::Identifier => {
                let token = self.advance();
                let name = Self::token_span(token);

                // Check if this is a function call
                if matches!(self.peek().kind, TokenKind::LeftParen) {
                    self.advance(); // consume '('
                    let args = self.parse_argument_list()?;
                    self.consume(TokenKind::RightParen, "Expected ')'")?;
                    Ok(Expr::FunctionCall { name, args })
                } else {
                    Ok(Expr::Identifier(name))
                }
            }
            TokenKind::Println => {
                let token = self.advance();
                let name_span = Self::token_span(token);
                self.consume(TokenKind::LeftParen, "Expected '(' after println")?;
                let args = self.parse_argument_list()?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr::FunctionCall {
                    name: name_span,
                    args,
                })
            }
            TokenKind::LeftParen => {
                self.advance(); // consume '('
                let expr = self.parse_expression(Precedence::NONE)?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(expr)
            }
            TokenKind::Minus => {
                self.advance(); // consume '-'
                let operand = self.parse_expression(Precedence::UNARY)?;
                // Represent unary minus as 0 - operand
                Ok(Expr::Binary {
                    left: Box::new(Expr::IntLiteral(Span::new(0, 1))), // placeholder "0"
                    op: BinaryOp::Subtract,
                    right: Box::new(operand),
                })
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: self.peek().kind,
                position: self.peek().position,
            }),
        }
    }

    /// Parse an infix expression - much simpler direct approach
    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        if let Some(binary_op) = token_to_binary_op(self.peek().kind) {
            let precedence = get_precedence_for_token(self.peek().kind);
            self.advance(); // consume operator
            let right = self.parse_expression(precedence)?;

            Ok(Expr::Binary {
                left: Box::new(left),
                op: binary_op,
                right: Box::new(right),
            })
        } else if matches!(self.peek().kind, TokenKind::LeftParen) {
            // Function call: identifier(args)
            if let Expr::Identifier(name) = left {
                self.advance(); // consume '('
                let args = self.parse_argument_list()?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr::FunctionCall { name, args })
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: "function name".to_string(),
                    found: self.peek().kind,
                    position: self.peek().position,
                })
            }
        } else {
            Ok(left) // No infix operator, return left as-is
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        if matches!(self.peek().kind, TokenKind::RightParen) {
            return Ok(args); // Empty argument list
        }

        // Parse first argument
        args.push(self.parse_expression(Precedence::NONE)?);

        // Parse remaining arguments
        while matches!(self.peek().kind, TokenKind::Comma) {
            self.advance(); // consume ','
            args.push(self.parse_expression(Precedence::NONE)?);
        }

        Ok(args)
    }

    // Helper methods
    fn parse_function(&mut self) -> Result<Function, ParseError> {
        // Consume 'fn' keyword
        self.consume(TokenKind::Fn, "Expected 'fn'")?;
        self.skip_newlines();

        // Parse function name
        let name_token = self.consume(TokenKind::Identifier, "Expected function name")?;
        let name = Self::token_span(name_token);
        self.skip_newlines();

        // Parse parameter list
        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
        self.skip_newlines();

        // Parse optional return type
        let return_type = if matches!(self.peek().kind, TokenKind::Arrow) {
            self.advance(); // consume '->'
            self.skip_newlines();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.skip_newlines();

        // Parse function body
        self.consume(TokenKind::LeftBrace, "Expected '{' to start function body")?;
        let body = self.parse_statement_list()?;
        self.consume(TokenKind::RightBrace, "Expected '}' to end function body")?;

        Ok(Function {
            name,
            parameters,
            return_type,
            body,
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();

        self.skip_newlines();

        // Handle empty parameter list
        if matches!(self.peek().kind, TokenKind::RightParen) {
            return Ok(parameters);
        }

        // Parse first parameter
        parameters.push(self.parse_parameter()?);

        // Parse remaining parameters
        while {
            self.skip_newlines();
            matches!(self.peek().kind, TokenKind::Comma)
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
        let name = Self::token_span(name_token);
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
        match self.peek().kind {
            TokenKind::U8 => {
                self.advance();
                Ok(Type::U8)
            }
            TokenKind::I8 => {
                self.advance();
                Ok(Type::I8)
            }
            TokenKind::U16 => {
                self.advance();
                Ok(Type::U16)
            }
            TokenKind::I16 => {
                self.advance();
                Ok(Type::I16)
            }
            TokenKind::U32 => {
                self.advance();
                Ok(Type::U32)
            }
            TokenKind::I32 => {
                self.advance();
                Ok(Type::I32)
            }
            TokenKind::U64 => {
                self.advance();
                Ok(Type::U64)
            }
            TokenKind::I64 => {
                self.advance();
                Ok(Type::I64)
            }
            TokenKind::Usize => {
                self.advance();
                Ok(Type::Usize)
            }
            TokenKind::Isize => {
                self.advance();
                Ok(Type::Isize)
            }
            TokenKind::F32 => {
                self.advance();
                Ok(Type::F32)
            }
            TokenKind::F64 => {
                self.advance();
                Ok(Type::F64)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "type".to_string(),
                found: self.peek().kind,
                position: self.peek().position,
            }),
        }
    }

    fn parse_statement_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        // Skip leading newlines
        self.skip_newlines();

        while !matches!(self.peek().kind, TokenKind::RightBrace | TokenKind::Eof) {
            statements.push(self.parse_statement()?);

            // After each statement, require a terminator or end of block
            self.consume_statement_terminator()?;

            // Skip any additional newlines
            self.skip_newlines();
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek().kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => {
                // Expression statement (no semicolon required)
                let expr = self.parse_expression(Precedence::NONE)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::Let, "Expected 'let'")?;
        self.skip_newlines();

        let name_token = self.consume(TokenKind::Identifier, "identifier")?;
        let name = Self::token_span(name_token);
        self.skip_newlines();

        // Optional type annotation
        let type_annotation = if matches!(self.peek().kind, TokenKind::Colon) {
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

    fn consume(&mut self, expected: TokenKind, message: &str) -> Result<&Token<'a>, ParseError> {
        if self.peek().kind == expected {
            Ok(self.advance())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: message.to_string(),
                found: self.peek().kind,
                position: self.peek().position,
            })
        }
    }

    fn advance(&mut self) -> &Token<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'a> {
        &self.tokens[self.current - 1]
    }

    fn token_span(token: &Token) -> Span {
        Span::new(token.position, token.position + token.text.len())
    }

    fn span_text(&self, span: &Span) -> &str {
        span.text(self.source)
    }

    /// Skip newline tokens (used when newlines are not significant)
    fn skip_newlines(&mut self) {
        while matches!(self.peek().kind, TokenKind::Newline) {
            self.advance();
        }
    }

    /// Consume a statement terminator (semicolon, newline, or end of block)
    fn consume_statement_terminator(&mut self) -> Result<(), ParseError> {
        match self.peek().kind {
            TokenKind::Semicolon => {
                self.advance();
                Ok(())
            }
            TokenKind::Newline => {
                self.advance();
                Ok(())
            }
            TokenKind::RightBrace | TokenKind::Eof => {
                // End of block terminates statement
                Ok(())
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "';', newline, or '}' after statement".to_string(),
                found: self.peek().kind,
                position: self.peek().position,
            }),
        }
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
