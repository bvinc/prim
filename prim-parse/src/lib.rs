use prim_tok::{Token, TokenKind, Tokenizer};

mod error;
pub use error::ParseError;

mod span;
pub use span::Span;

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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    IntLiteral(Span),
    FloatLiteral(Span),
    Identifier(Span),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    FunctionCall {
        name: Span,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: Span,
        type_annotation: Option<Type>,
        value: Expr,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Span,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Span,
    pub type_annotation: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
    source: &'a str, // Keep reference to source for span text extraction
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>, source: &'a str) -> Self {
        Self {
            tokens,
            current: 0,
            source,
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut functions = Vec::new();

        while !self.is_at_end() {
            // Skip whitespace and newlines
            if matches!(self.peek().kind, TokenKind::Whitespace | TokenKind::Newline) {
                self.advance();
                continue;
            }

            // Only allow function definitions at the top level
            match self.peek().kind {
                TokenKind::Fn => {
                    let function = self.parse_function()?;
                    functions.push(function);
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

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        self.consume(TokenKind::Fn, "Expected 'fn'")?;
        self.skip_whitespace();

        let name = match self.peek().kind {
            TokenKind::Identifier => {
                let token = self.advance();
                Self::token_span(token)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "function name".to_string(),
                    found: self.peek().kind,
                    position: self.peek().position,
                });
            }
        };

        self.skip_whitespace();
        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;

        // Parse parameters
        let parameters = self.parse_parameter_list()?;

        self.consume(TokenKind::RightParen, "Expected ')'")?;
        self.skip_whitespace();

        // Parse optional return type
        let return_type = if matches!(self.peek().kind, TokenKind::Arrow) {
            self.advance(); // consume '->'
            self.skip_whitespace();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.skip_whitespace();
        self.consume(TokenKind::LeftBrace, "Expected '{' to start function body")?;

        // Parse function body
        let mut body = Vec::new();
        while !matches!(self.peek().kind, TokenKind::RightBrace | TokenKind::Eof) {
            // Skip whitespace and newlines
            if matches!(self.peek().kind, TokenKind::Whitespace | TokenKind::Newline) {
                self.advance();
                continue;
            }

            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

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

        self.skip_whitespace();
        if matches!(self.peek().kind, TokenKind::RightParen) {
            return Ok(parameters); // Empty parameter list
        }

        // Parse first parameter
        parameters.push(self.parse_parameter()?);

        // Parse remaining parameters
        while {
            self.skip_whitespace();
            matches!(self.peek().kind, TokenKind::Comma)
        } {
            self.advance(); // consume ','
            self.skip_whitespace();
            parameters.push(self.parse_parameter()?);
        }

        Ok(parameters)
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let name = match self.peek().kind {
            TokenKind::Identifier => {
                let token = self.advance();
                Self::token_span(token)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "parameter name".to_string(),
                    found: self.peek().kind,
                    position: self.peek().position,
                });
            }
        };

        self.skip_whitespace();
        self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
        self.skip_whitespace();

        let type_annotation = self.parse_type()?;

        Ok(Parameter {
            name,
            type_annotation,
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek().kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => {
                let expr = self.parse_expression()?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::Let, "Expected 'let'")?;
        self.skip_whitespace();

        let name = match self.peek().kind {
            TokenKind::Identifier => {
                let token = self.advance();
                Self::token_span(token)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: self.peek().kind,
                    position: self.peek().position,
                });
            }
        };

        self.skip_whitespace();

        // Optional type annotation
        let type_annotation = if matches!(self.peek().kind, TokenKind::Colon) {
            self.advance(); // consume ':'
            self.skip_whitespace();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.skip_whitespace();
        self.consume(TokenKind::Equals, "Expected '='")?;
        self.skip_whitespace();

        let value = self.parse_expression()?;

        Ok(Stmt::Let {
            name,
            type_annotation,
            value,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let type_token = self.advance();
        let type_kind = match type_token.kind {
            TokenKind::U8 => Type::U8,
            TokenKind::I8 => Type::I8,
            TokenKind::U16 => Type::U16,
            TokenKind::I16 => Type::I16,
            TokenKind::U32 => Type::U32,
            TokenKind::I32 => Type::I32,
            TokenKind::U64 => Type::U64,
            TokenKind::I64 => Type::I64,
            TokenKind::Usize => Type::Usize,
            TokenKind::Isize => Type::Isize,
            TokenKind::F32 => Type::F32,
            TokenKind::F64 => Type::F64,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "type".to_string(),
                    found: type_token.kind,
                    position: type_token.position,
                });
            }
        };
        Ok(type_kind)
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_addition()?;

        loop {
            self.skip_whitespace();
            if !matches!(self.peek().kind, TokenKind::DoubleEquals) {
                break;
            }

            let op = BinaryOp::Equals;
            self.advance();
            self.skip_whitespace();
            let right = self.parse_addition()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_multiplication()?;

        loop {
            self.skip_whitespace();
            if !matches!(self.peek().kind, TokenKind::Plus | TokenKind::Minus) {
                break;
            }

            let token = self.advance();
            let op = match token.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Subtract,
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "binary operator".to_string(),
                        found: token.kind,
                        position: token.position,
                    });
                }
            };
            self.skip_whitespace();
            let right = self.parse_multiplication()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            self.skip_whitespace();
            if !matches!(self.peek().kind, TokenKind::Star | TokenKind::Slash) {
                break;
            }

            let token = self.advance();
            let op = match token.kind {
                TokenKind::Star => BinaryOp::Multiply,
                TokenKind::Slash => BinaryOp::Divide,
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "binary operator".to_string(),
                        found: token.kind,
                        position: token.position,
                    });
                }
            };
            self.skip_whitespace();
            let right = self.parse_primary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        self.skip_whitespace();
        let token = self.advance();

        match &token.kind {
            TokenKind::IntLiteral => Ok(Expr::IntLiteral(Self::token_span(token))),
            TokenKind::FloatLiteral => Ok(Expr::FloatLiteral(Self::token_span(token))),
            TokenKind::Identifier => {
                let name = Self::token_span(token);
                // Check if this is a function call
                self.skip_whitespace();
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
                // println is a built-in function
                let name_span = Self::token_span(token);
                self.skip_whitespace();
                self.consume(TokenKind::LeftParen, "Expected '(' after println")?;
                let args = self.parse_argument_list()?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr::FunctionCall {
                    name: name_span,
                    args,
                })
            }
            TokenKind::LeftParen => {
                self.skip_whitespace();
                let expr = self.parse_expression()?;
                self.skip_whitespace();
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(expr)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: token.kind,
                position: token.position,
            }),
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        self.skip_whitespace();
        if matches!(self.peek().kind, TokenKind::RightParen) {
            return Ok(args); // Empty argument list
        }

        // Parse first argument
        args.push(self.parse_expression()?);

        // Parse remaining arguments
        while {
            self.skip_whitespace();
            matches!(self.peek().kind, TokenKind::Comma)
        } {
            self.advance(); // consume ','
            self.skip_whitespace();
            args.push(self.parse_expression()?);
        }

        Ok(args)
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek().kind, TokenKind::Whitespace) {
            self.advance();
        }
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

    /// Create a span from a token (no self needed)
    fn token_span(token: &Token) -> Span {
        Span::new(token.position, token.position + token.text.len())
    }

    /// Get text from a span
    fn span_text(&self, span: &Span) -> &str {
        span.text(self.source)
    }
}

pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;
    let mut parser = Parser::new(tokens, input);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let source = "fn main() { let x: u32 = 42 }";
        let program = parse(source).unwrap();

        assert_eq!(program.functions.len(), 1);
        let main_func = &program.functions[0];
        assert_eq!(main_func.name.text(source), "main");
        assert_eq!(main_func.body.len(), 1);
        match &main_func.body[0] {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                assert_eq!(name.text(source), "x");
                assert_eq!(type_annotation, &Some(Type::U32));
                match value {
                    Expr::IntLiteral(span) => assert_eq!(span.text(source), "42"),
                    _ => panic!("Expected IntLiteral, got {:?}", value),
                }
            }
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_let_without_type() {
        let source = "fn main() { let x = 42 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                assert_eq!(name.text(source), "x");
                assert_eq!(type_annotation, &None);
                match value {
                    Expr::IntLiteral(span) => assert_eq!(span.text(source), "42"),
                    _ => panic!("Expected IntLiteral, got {:?}", value),
                }
            }
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression() {
        let source = "fn main() { let result = x + 5 * 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match left.as_ref() {
                        Expr::Identifier(span) => assert_eq!(span.text(source), "x"),
                        _ => panic!("Expected Identifier, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "2"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression_2() {
        let source = "fn main() { let result = x * 5 + 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "2"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::Identifier(span) => assert_eq!(span.text(source), "x"),
                                _ => panic!("Expected Identifier, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression, got {:?}", left),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_println() {
        let source = "fn main() { println(42) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { name, args }) => {
                assert_eq!(name.text(source), "println");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Expr::IntLiteral(span) => assert_eq!(span.text(source), "42"),
                    _ => panic!("Expected IntLiteral, got {:?}", &args[0]),
                }
            }
            _ => panic!(
                "Expected println function call, got {:?}",
                &main_func.body[0]
            ),
        }
    }

    #[test]
    fn test_parse_println_with_expression() {
        let source = "fn main() { println(x + 5) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { name, args }) => {
                assert_eq!(name.text(source), "println");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Expr::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Add);
                        match left.as_ref() {
                            Expr::Identifier(span) => assert_eq!(span.text(source), "x"),
                            _ => panic!("Expected Identifier, got {:?}", left),
                        }
                        match right.as_ref() {
                            Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                            _ => panic!("Expected IntLiteral, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression, got {:?}", &args[0]),
                }
            }
            _ => panic!(
                "Expected println function call, got {:?}",
                &main_func.body[0]
            ),
        }
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let result = parse("fn main() { let = 42 }");

        match result {
            Err(ParseError::UnexpectedToken {
                expected, found, ..
            }) => {
                assert_eq!(expected, "identifier");
                assert_eq!(found, TokenKind::Equals);
            }
            _ => panic!("Expected UnexpectedToken error, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_error_from_tokenizer() {
        let result = parse("fn main() { let x = @ }");

        match result {
            Err(ParseError::TokenError(prim_tok::TokenError::UnexpectedCharacter {
                ch, ..
            })) => {
                assert_eq!(ch, '@');
            }
            _ => panic!("Expected TokenError from tokenizer, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_error_missing_main() {
        let result = parse("fn foo() { let x = 42 }");

        match result {
            Err(ParseError::MissingMainFunction) => {}
            _ => panic!("Expected MissingMainFunction error, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_error_statements_outside_function() {
        let result = parse("let x = 42");

        match result {
            Err(ParseError::StatementsOutsideFunction) => {}
            _ => panic!("Expected StatementsOutsideFunction error, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_parentheses_basic() {
        let source = "fn main() { let result = (2 + 3) * 4 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Multiply);
                    // Left side should be the parenthesized expression (2 + 3)
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "2"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "3"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (2 + 3), got {:?}", left),
                    }
                    // Right side should be 4
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "4"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_parentheses_nested() {
        let source = "fn main() { let result = ((2 + 3) * 4) + 5 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    // Left side should be ((2 + 3) * 4)
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            // Inner left should be (2 + 3)
                            match left.as_ref() {
                                Expr::Binary { left, op, right } => {
                                    assert_eq!(op, &BinaryOp::Add);
                                    match left.as_ref() {
                                        Expr::IntLiteral(span) => {
                                            assert_eq!(span.text(source), "2")
                                        }
                                        _ => panic!("Expected IntLiteral, got {:?}", left),
                                    }
                                    match right.as_ref() {
                                        Expr::IntLiteral(span) => {
                                            assert_eq!(span.text(source), "3")
                                        }
                                        _ => panic!("Expected IntLiteral, got {:?}", right),
                                    }
                                }
                                _ => {
                                    panic!("Expected binary expression for (2 + 3), got {:?}", left)
                                }
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "4"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!(
                            "Expected binary expression for ((2 + 3) * 4), got {:?}",
                            left
                        ),
                    }
                    // Right side should be 5
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_parentheses_with_all_operators() {
        let source = "fn main() { let result = (x + y) * (a - b) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Multiply);
                    // Left side: (x + y)
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::Identifier(span) => assert_eq!(span.text(source), "x"),
                                _ => panic!("Expected Identifier, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::Identifier(span) => assert_eq!(span.text(source), "y"),
                                _ => panic!("Expected Identifier, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (x + y), got {:?}", left),
                    }
                    // Right side: (a - b)
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Subtract);
                            match left.as_ref() {
                                Expr::Identifier(span) => assert_eq!(span.text(source), "a"),
                                _ => panic!("Expected Identifier, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::Identifier(span) => assert_eq!(span.text(source), "b"),
                                _ => panic!("Expected Identifier, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (a - b), got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_parentheses_function_call_args() {
        let source = "fn main() { println((2 + 3) * 4) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { name, args }) => {
                assert_eq!(name.text(source), "println");
                assert_eq!(args.len(), 1);
                // Argument should be (2 + 3) * 4
                match &args[0] {
                    Expr::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Multiply);
                        match left.as_ref() {
                            Expr::Binary { left, op, right } => {
                                assert_eq!(op, &BinaryOp::Add);
                                match left.as_ref() {
                                    Expr::IntLiteral(span) => assert_eq!(span.text(source), "2"),
                                    _ => panic!("Expected IntLiteral, got {:?}", left),
                                }
                                match right.as_ref() {
                                    Expr::IntLiteral(span) => assert_eq!(span.text(source), "3"),
                                    _ => panic!("Expected IntLiteral, got {:?}", right),
                                }
                            }
                            _ => panic!("Expected binary expression for (2 + 3), got {:?}", left),
                        }
                        match right.as_ref() {
                            Expr::IntLiteral(span) => assert_eq!(span.text(source), "4"),
                            _ => panic!("Expected IntLiteral, got {:?}", right),
                        }
                    }
                    _ => panic!("Expected binary expression, got {:?}", &args[0]),
                }
            }
            _ => panic!("Expected println call, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_error_mismatched_parentheses_missing_close() {
        let result = parse("fn main() { let x = (2 + 3 }");

        match result {
            Err(ParseError::UnexpectedToken {
                expected, found, ..
            }) => {
                assert_eq!(expected, "Expected ')'");
                assert_eq!(found, TokenKind::RightBrace);
            }
            _ => panic!(
                "Expected UnexpectedToken error for missing ')', got {:?}",
                result
            ),
        }
    }

    #[test]
    fn test_parse_error_mismatched_parentheses_missing_open() {
        let result = parse("fn main() { let x = 2 + 3) }");

        match result {
            Err(ParseError::UnexpectedToken {
                expected, found, ..
            }) => {
                assert_eq!(expected, "expression");
                assert_eq!(found, TokenKind::RightParen);
            }
            _ => panic!(
                "Expected UnexpectedToken error for unexpected ')', got {:?}",
                result
            ),
        }
    }

    #[test]
    fn test_parse_empty_parentheses_error() {
        let result = parse("fn main() { let x = () }");

        match result {
            Err(ParseError::UnexpectedToken { expected, .. }) => {
                assert_eq!(expected, "expression");
            }
            _ => panic!(
                "Expected UnexpectedToken error for empty parentheses, got {:?}",
                result
            ),
        }
    }

    #[test]
    fn test_parse_subtraction_basic() {
        let source = "fn main() { let result = 10 - 3 }";
        let program = parse(source).unwrap();
        let debug_str = format!("{:#?}", program);

        assert!(debug_str.contains("Subtract"));
        assert!(debug_str.contains("IntLiteral"));
    }

    #[test]
    fn test_parse_subtraction_with_identifiers() {
        let source = "fn main() { let result = x - y }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    match left.as_ref() {
                        Expr::Identifier(span) => assert_eq!(span.text(source), "x"),
                        _ => panic!("Expected Identifier, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::Identifier(span) => assert_eq!(span.text(source), "y"),
                        _ => panic!("Expected Identifier, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_subtraction_precedence() {
        let source = "fn main() { let result = 10 - 3 * 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    match left.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "10"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be 3 * 2 (multiplication has higher precedence)
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "3"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "2"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for 3 * 2, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_subtraction_chained() {
        let source = "fn main() { let result = 20 - 5 - 3 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    // Left side should be (20 - 5) due to left associativity
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Subtract);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "20"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (20 - 5), got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "3"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_subtraction_with_parentheses() {
        let source = "fn main() { let result = 20 - (5 + 3) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Subtract);
                    match left.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "20"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be (5 + 3)
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "3"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (5 + 3), got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_basic() {
        let source = "fn main() { let result = 20 / 4 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    match left.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "20"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "4"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_with_identifiers() {
        let source = "fn main() { let result = numerator / denominator }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    match left.as_ref() {
                        Expr::Identifier(span) => assert_eq!(span.text(source), "numerator"),
                        _ => panic!("Expected Identifier, got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::Identifier(span) => assert_eq!(span.text(source), "denominator"),
                        _ => panic!("Expected Identifier, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_precedence_with_addition() {
        let source = "fn main() { let result = 10 + 20 / 4 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    match left.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "10"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be 20 / 4 (division has higher precedence)
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Divide);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "20"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "4"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for 20 / 4, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_chained() {
        let source = "fn main() { let result = 100 / 5 / 2 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    // Left side should be (100 / 5) due to left associativity
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Divide);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "100"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (100 / 5), got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "2"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_with_multiplication() {
        let source = "fn main() { let result = 8 * 6 / 3 }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    // Left side should be (8 * 6) due to left associativity
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "8"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "6"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (8 * 6), got {:?}", left),
                    }
                    match right.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "3"),
                        _ => panic!("Expected IntLiteral, got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_parse_division_with_parentheses() {
        let source = "fn main() { let result = 100 / (10 + 5) }";
        let program = parse(source).unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Divide);
                    match left.as_ref() {
                        Expr::IntLiteral(span) => assert_eq!(span.text(source), "100"),
                        _ => panic!("Expected IntLiteral, got {:?}", left),
                    }
                    // Right side should be (10 + 5)
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Add);
                            match left.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "10"),
                                _ => panic!("Expected IntLiteral, got {:?}", left),
                            }
                            match right.as_ref() {
                                Expr::IntLiteral(span) => assert_eq!(span.text(source), "5"),
                                _ => panic!("Expected IntLiteral, got {:?}", right),
                            }
                        }
                        _ => panic!("Expected binary expression for (10 + 5), got {:?}", right),
                    }
                }
                _ => panic!("Expected binary expression, got {:?}", value),
            },
            _ => panic!("Expected let statement, got {:?}", &main_func.body[0]),
        }
    }

    #[test]
    fn test_chained_function_calls() {
        let source = r#"
fn level4() -> i64 {
    println(4)
    42
}

fn level3() -> i64 {
    println(3)
    let result = level4()
    println(300 + result)
    result
}

fn level2() -> i64 {
    println(2)
    let result = level3()
    println(200 + result)
    result
}

fn level1() -> i64 {
    println(1)
    let result = level2()
    println(100 + result)
    result
}

fn main() {
    println(0)
    let final_result = level1()
    println(final_result)
}
"#;
        let program = parse(source).unwrap();

        // Check that we have all 5 functions
        assert_eq!(program.functions.len(), 5);

        // Check function names
        let function_names: Vec<&str> = program
            .functions
            .iter()
            .map(|f| f.name.text(source))
            .collect();
        assert!(function_names.contains(&"level4"));
        assert!(function_names.contains(&"level3"));
        assert!(function_names.contains(&"level2"));
        assert!(function_names.contains(&"level1"));
        assert!(function_names.contains(&"main"));

        // Check that main function has function calls
        let main_func = program
            .functions
            .iter()
            .find(|f| f.name.text(source) == "main")
            .expect("main function should exist");

        // Verify main has statements
        assert!(!main_func.body.is_empty());

        // Quick check that we have function calls in the AST
        let debug_str = format!("{:#?}", program);
        assert!(debug_str.contains("FunctionCall"));

        // Check that level1 function calls level2
        let level1_func = program
            .functions
            .iter()
            .find(|f| f.name.text(source) == "level1")
            .expect("level1 function should exist");

        // Find the function call to level2 in level1
        let has_level2_call = level1_func.body.iter().any(|stmt| match stmt {
            Stmt::Let {
                value: Expr::FunctionCall { name, .. },
                ..
            } => name.text(source) == "level2",
            _ => false,
        });
        assert!(has_level2_call, "level1 should call level2");
    }
}
