mod error;
pub use error::TokenError;

mod span;
pub use span::{Span, byte_offset_to_line_col};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,
    MultilineStringSegment,

    // Keywords
    Let,
    Struct,
    Fn,
    Impl,
    Trait,
    Import,
    Mod,
    If,
    Else,
    Loop,
    While,
    Break,
    For,
    True,
    False,
    Const,
    Mut,

    // Types
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

    // Identifiers
    Identifier,

    // Operators
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    Percent,       // %
    Equals,        // =
    DoubleEquals,  // ==
    NotEquals,     // !=
    Greater,       // >
    GreaterEquals, // >=
    Less,          // <
    LessEquals,    // <=
    Arrow,         // ->
    UnaryPlus,     // +
    UnaryMinus,    // -
    UnaryStar,     // *
    PostfixStar,   // *

    // Punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Colon,        // :
    Semicolon,    // ;
    Ampersand,    // &
    Dot,          // .
    At,           // @

    // Special
    Comment,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

fn is_whitespace_char(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperatorSpacing {
    Prefix,
    Postfix,
    Infix,
}

pub struct Tokenizer<'a> {
    input: &'a str,
    chars: std::str::Chars<'a>,
    position: usize,
    current: Option<char>,
    prev_is_space: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current = chars.next();
        Self {
            input,
            chars,
            position: 0,
            current,
            prev_is_space: true,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenError> {
        let mut tokens = Vec::new();
        while let Some(tok) = self.next_token()? {
            tokens.push(tok);
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Option<Token>, TokenError> {
        // Skip whitespace
        while self.current.is_some_and(is_whitespace_char) {
            self.advance();
        }

        let start_pos = self.position;

        match self.current {
            None => Ok(None),
            Some('+') => self.read_operator("+", start_pos),
            Some('-') => {
                if self.peek() == Some('>') {
                    self.advance();
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::Arrow,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    self.read_operator("-", start_pos)
                }
            }
            Some('*') => self.read_operator("*", start_pos),
            Some('/') => {
                if self.peek() == Some('/') {
                    self.advance(); // first '/'
                    self.advance(); // second '/'
                    let end_pos = self.read_to_line_end();
                    Ok(Some(Token {
                        kind: TokenKind::Comment,
                        span: Span::new(start_pos, end_pos),
                    }))
                } else {
                    self.read_operator("/", start_pos)
                }
            }
            Some('%') => self.read_operator("%", start_pos),
            Some('=') => {
                if self.peek() == Some('=') {
                    self.read_operator("==", start_pos)
                } else {
                    self.read_operator("=", start_pos)
                }
            }
            Some('>') => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::GreaterEquals,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::Greater,
                        span: Span::new(start_pos, self.position),
                    }))
                }
            }
            Some('<') => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::LessEquals,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::Less,
                        span: Span::new(start_pos, self.position),
                    }))
                }
            }
            Some('!') => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::NotEquals,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    let end_pos = start_pos + 1;
                    Err(TokenError::UnexpectedCharacter {
                        ch: '!',
                        span: Span::new(start_pos, end_pos),
                    })
                }
            }
            Some('(') => self.emit_simple(TokenKind::LeftParen, start_pos),
            Some(')') => self.emit_simple(TokenKind::RightParen, start_pos),
            Some('{') => self.emit_simple(TokenKind::LeftBrace, start_pos),
            Some('}') => self.emit_simple(TokenKind::RightBrace, start_pos),
            Some('[') => self.emit_simple(TokenKind::LeftBracket, start_pos),
            Some(']') => self.emit_simple(TokenKind::RightBracket, start_pos),
            Some(',') => self.emit_simple(TokenKind::Comma, start_pos),
            Some(':') => self.emit_simple(TokenKind::Colon, start_pos),
            Some(';') => self.emit_simple(TokenKind::Semicolon, start_pos),
            Some('&') => self.emit_simple(TokenKind::Ampersand, start_pos),
            Some('@') => self.emit_simple(TokenKind::At, start_pos),
            Some('.') => {
                // Check if this is a standalone dot (for field access) or part of a number
                if self.peek().is_none_or(|c| !c.is_ascii_digit()) {
                    self.emit_simple(TokenKind::Dot, start_pos)
                } else {
                    // This is likely the start of a floating point number like .5
                    self.read_number(start_pos).map(Some)
                }
            }
            Some('\\') => {
                if self.peek() == Some('\\') {
                    self.advance(); // first '\'
                    self.advance(); // second '\'
                    let end_pos = self.read_to_line_end();
                    Ok(Some(Token {
                        kind: TokenKind::MultilineStringSegment,
                        span: Span::new(start_pos, end_pos),
                    }))
                } else {
                    Err(TokenError::UnexpectedCharacter {
                        ch: '\\',
                        span: Span::new(start_pos, start_pos + 1),
                    })
                }
            }
            Some('"') => self.read_string_literal(start_pos).map(Some),
            Some('\'') => self.read_char_literal(start_pos).map(Some),
            Some(c) if c.is_ascii_digit() => self.read_number(start_pos).map(Some),
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                Ok(Some(self.read_identifier(start_pos)))
            }
            Some(c) => {
                let end_pos = start_pos + c.len_utf8();
                Err(TokenError::UnexpectedCharacter {
                    ch: c,
                    span: Span::new(start_pos, end_pos),
                })
            }
        }
    }

    fn emit_simple(
        &mut self,
        kind: TokenKind,
        start_pos: usize,
    ) -> Result<Option<Token>, TokenError> {
        self.advance();
        Ok(Some(Token {
            kind,
            span: Span::new(start_pos, self.position),
        }))
    }

    fn read_operator(
        &mut self,
        op: &'static str,
        start_pos: usize,
    ) -> Result<Option<Token>, TokenError> {
        let left_space = self.prev_is_space;
        for _ in 0..op.len() {
            self.advance();
        }
        let spacing =
            Self::operator_spacing(left_space, self.current.is_some_and(is_whitespace_char));
        let kind = match (op, spacing) {
            ("+", OperatorSpacing::Prefix) => TokenKind::UnaryPlus,
            ("+", OperatorSpacing::Infix) => TokenKind::Plus,
            ("-", OperatorSpacing::Prefix) => TokenKind::UnaryMinus,
            ("-", OperatorSpacing::Infix) => TokenKind::Minus,
            ("*", OperatorSpacing::Prefix) => TokenKind::UnaryStar,
            ("*", OperatorSpacing::Postfix) => TokenKind::PostfixStar,
            ("*", OperatorSpacing::Infix) => TokenKind::Star,
            ("/", OperatorSpacing::Infix) => TokenKind::Slash,
            ("%", OperatorSpacing::Infix) => TokenKind::Percent,
            ("=", OperatorSpacing::Infix) => TokenKind::Equals,
            ("==", OperatorSpacing::Infix) => TokenKind::DoubleEquals,
            _ => {
                return Err(TokenError::InvalidOperatorSpacing {
                    op,
                    span: Span::new(start_pos, self.position),
                });
            }
        };
        Ok(Some(Token {
            kind,
            span: Span::new(start_pos, self.position),
        }))
    }

    fn operator_spacing(left_space: bool, right_space: bool) -> OperatorSpacing {
        match (left_space, right_space) {
            (true, false) => OperatorSpacing::Prefix,
            (false, true) => OperatorSpacing::Postfix,
            _ => OperatorSpacing::Infix,
        }
    }

    fn read_number(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        let mut is_float = false;
        let mut seen_digit = false;
        let mut seen_exp = false;

        let is_radix_digit: Option<fn(char) -> bool> = if self.current == Some('0') {
            match self.peek() {
                Some('b') => Some(|c| c == '0' || c == '1'),
                Some('o') => Some(|c: char| c.is_ascii_digit() && c < '8'),
                Some('x') => Some(|c: char| c.is_ascii_hexdigit()),
                _ => None,
            }
        } else {
            None
        };

        if let Some(is_digit) = is_radix_digit {
            self.advance(); // '0'
            self.advance(); // prefix
            while let Some(ch) = self.current {
                if is_digit(ch) {
                    seen_digit = true;
                    self.advance();
                    continue;
                }
                if ch == '_' {
                    if self.peek().is_some_and(|c| c.is_ascii_alphabetic()) {
                        break;
                    }
                    self.advance();
                    continue;
                }
                break;
            }
        } else {
            while let Some(ch) = self.current {
                if ch.is_ascii_digit() {
                    seen_digit = true;
                    self.advance();
                    continue;
                }
                if ch == '_' {
                    if self.peek().is_some_and(|c| c.is_ascii_alphabetic()) {
                        break;
                    }
                    self.advance();
                    continue;
                }
                if ch == '.' && !is_float {
                    is_float = true;
                    self.advance();
                    continue;
                }
                if (ch == 'e' || ch == 'E') && !seen_exp {
                    seen_exp = true;
                    is_float = true;
                    self.advance();
                    if self.current.is_some_and(|c| c == '+' || c == '-') {
                        self.advance();
                    }
                    continue;
                }
                break;
            }
        }

        // Handle type suffixes (e.g., 42u32, 3.14f64, 1e3_f64)
        while self
            .current
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.advance();
        }

        if !seen_digit {
            return Err(TokenError::InvalidNumber {
                text: self.input[start_pos..self.position].to_string(),
                span: Span::new(start_pos, self.position),
            });
        }

        Ok(Token {
            kind: if is_float {
                TokenKind::FloatLiteral
            } else {
                TokenKind::IntLiteral
            },
            span: Span::new(start_pos, self.position),
        })
    }

    fn read_identifier(&mut self, start_pos: usize) -> Token {
        while self
            .current
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.advance();
        }

        let text = &self.input[start_pos..self.position];
        let kind = match text {
            "let" => TokenKind::Let,
            "struct" => TokenKind::Struct,
            "fn" => TokenKind::Fn,
            "impl" => TokenKind::Impl,
            "trait" => TokenKind::Trait,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "loop" => TokenKind::Loop,
            "while" => TokenKind::While,
            "break" => TokenKind::Break,
            "import" => TokenKind::Import,
            "mod" => TokenKind::Mod,
            "for" => TokenKind::For,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "const" => TokenKind::Const,
            "mut" => TokenKind::Mut,
            "u8" => TokenKind::U8,
            "i8" => TokenKind::I8,
            "u16" => TokenKind::U16,
            "i16" => TokenKind::I16,
            "u32" => TokenKind::U32,
            "i32" => TokenKind::I32,
            "u64" => TokenKind::U64,
            "i64" => TokenKind::I64,
            "usize" => TokenKind::Usize,
            "isize" => TokenKind::Isize,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "bool" => TokenKind::Bool,
            _ => TokenKind::Identifier,
        };

        Token {
            kind,
            span: Span::new(start_pos, self.position),
        }
    }

    fn advance(&mut self) {
        if let Some(ch) = self.current {
            self.position += ch.len_utf8();
            self.prev_is_space = is_whitespace_char(ch);
            self.current = self.chars.next();
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn read_to_line_end(&mut self) -> usize {
        while self.current.is_some_and(|c| c != '\n' && c != '\r') {
            self.advance();
        }
        self.position
    }

    fn read_string_literal(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        self.advance(); // consume opening quote

        while let Some(ch) = self.current {
            match ch {
                '"' => {
                    self.advance(); // consume closing quote
                    return Ok(Token {
                        kind: TokenKind::StringLiteral,
                        span: Span::new(start_pos, self.position),
                    });
                }
                '\\' => {
                    self.advance(); // consume backslash
                    if self.current.is_some() {
                        self.advance(); // consume escaped character
                    }
                }
                '\n' | '\r' => {
                    return Err(TokenError::UnterminatedString {
                        span: Span::new(start_pos, self.position),
                    });
                }
                _ => self.advance(),
            }
        }

        Err(TokenError::UnterminatedString {
            span: Span::new(start_pos, self.position),
        })
    }

    fn read_char_literal(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        self.advance(); // consume opening quote

        if self.current.is_none() {
            return Err(TokenError::UnterminatedString {
                span: Span::new(start_pos, self.position),
            });
        }

        // Handle escaped characters
        if self.current == Some('\\') {
            self.advance(); // consume backslash
            if self.current.is_none() {
                return Err(TokenError::UnterminatedString {
                    span: Span::new(start_pos, self.position),
                });
            }
            self.advance(); // consume escaped character
        } else {
            self.advance(); // consume regular character
        }

        // Expect closing quote
        if self.current != Some('\'') {
            return Err(TokenError::UnterminatedString {
                span: Span::new(start_pos, self.position),
            });
        }

        self.advance(); // consume closing quote
        Ok(Token {
            kind: TokenKind::CharLiteral,
            span: Span::new(start_pos, self.position),
        })
    }
}
