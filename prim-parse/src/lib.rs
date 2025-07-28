use prim_tok::{Token, TokenKind, Tokenizer};

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
    IntLiteral(String),
    FloatLiteral(String),
    Identifier(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Equals,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        type_annotation: Option<Type>,
        value: Expr,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

mod error;
pub use error::ParseError;

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
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
        if !functions.iter().any(|f| f.name == "main") {
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
                token.text.to_string()
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "function name".to_string(),
                    found: self.peek().kind.clone(),
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
                token.text.to_string()
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "parameter name".to_string(),
                    found: self.peek().kind.clone(),
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
                token.text.to_string()
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: self.peek().kind.clone(),
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
                    found: type_token.kind.clone(),
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
                        found: token.kind.clone(),
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
            if !matches!(self.peek().kind, TokenKind::Star) {
                break;
            }

            let op = BinaryOp::Multiply;
            self.advance();
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
            TokenKind::IntLiteral => Ok(Expr::IntLiteral(token.text.to_string())),
            TokenKind::FloatLiteral => Ok(Expr::FloatLiteral(token.text.to_string())),
            TokenKind::Identifier => {
                let name = token.text.to_string();
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
                self.skip_whitespace();
                self.consume(TokenKind::LeftParen, "Expected '(' after println")?;
                let args = self.parse_argument_list()?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                Ok(Expr::FunctionCall {
                    name: "println".to_string(),
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
                found: token.kind.clone(),
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
                found: self.peek().kind.clone(),
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
}

pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let program = parse("fn main() { let x: u32 = 42 }").unwrap();

        assert_eq!(program.functions.len(), 1);
        let main_func = &program.functions[0];
        assert_eq!(main_func.name, "main");
        assert_eq!(main_func.body.len(), 1);
        match &main_func.body[0] {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                assert_eq!(name, "x");
                assert_eq!(type_annotation, &Some(Type::U32));
                assert_eq!(value, &Expr::IntLiteral("42".to_string()));
            }
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_let_without_type() {
        let program = parse("fn main() { let x = 42 }").unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                assert_eq!(name, "x");
                assert_eq!(type_annotation, &None);
                assert_eq!(value, &Expr::IntLiteral("42".to_string()));
            }
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression() {
        let program = parse("fn main() { let result = x + 5 * 2 }").unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    assert_eq!(left.as_ref(), &Expr::Identifier("x".to_string()));
                    match right.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            assert_eq!(left.as_ref(), &Expr::IntLiteral("5".to_string()));
                            assert_eq!(right.as_ref(), &Expr::IntLiteral("2".to_string()));
                        }
                        _ => panic!("Expected binary expression"),
                    }
                }
                _ => panic!("Expected binary expression"),
            },
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression_2() {
        let program = parse("fn main() { let result = x * 5 + 2 }").unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Let { value, .. } => match value {
                Expr::Binary { left, op, right } => {
                    assert_eq!(op, &BinaryOp::Add);
                    assert_eq!(right.as_ref(), &Expr::IntLiteral("2".to_string()));
                    match left.as_ref() {
                        Expr::Binary { left, op, right } => {
                            assert_eq!(op, &BinaryOp::Multiply);
                            assert_eq!(left.as_ref(), &Expr::Identifier("x".to_string()));
                            assert_eq!(right.as_ref(), &Expr::IntLiteral("5".to_string()));
                        }
                        _ => panic!("Expected binary expression"),
                    }
                }
                _ => panic!("Expected binary expression"),
            },
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_println() {
        let program = parse("fn main() { println(42) }").unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { name, args }) => {
                assert_eq!(name, "println");
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], Expr::IntLiteral("42".to_string()));
            }
            _ => panic!("Expected println function call"),
        }
    }

    #[test]
    fn test_parse_println_with_expression() {
        let program = parse("fn main() { println(x + 5) }").unwrap();

        let main_func = &program.functions[0];
        match &main_func.body[0] {
            Stmt::Expr(Expr::FunctionCall { name, args }) => {
                assert_eq!(name, "println");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Expr::Binary { left, op, right } => {
                        assert_eq!(op, &BinaryOp::Add);
                        assert_eq!(left.as_ref(), &Expr::Identifier("x".to_string()));
                        assert_eq!(right.as_ref(), &Expr::IntLiteral("5".to_string()));
                    }
                    _ => panic!("Expected binary expression"),
                }
            }
            _ => panic!("Expected println function call"),
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
            _ => panic!("Expected UnexpectedToken error"),
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
            _ => panic!("Expected TokenError from tokenizer"),
        }
    }

    #[test]
    fn test_parse_error_missing_main() {
        let result = parse("fn foo() { let x = 42 }");

        match result {
            Err(ParseError::MissingMainFunction) => {}
            _ => panic!("Expected MissingMainFunction error"),
        }
    }

    #[test]
    fn test_parse_error_statements_outside_function() {
        let result = parse("let x = 42");

        match result {
            Err(ParseError::StatementsOutsideFunction) => {}
            _ => panic!("Expected StatementsOutsideFunction error"),
        }
    }
}
