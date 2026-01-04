mod error;
pub use error::TokenError;

mod span;
pub use span::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,

    // Keywords
    Let,
    Struct,
    Fn,
    Impl,
    Trait,
    Import,
    Mod,
    If,
    Loop,
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
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Equals,       // =
    DoubleEquals, // ==
    Arrow,        // ->
    UnaryPlus,    // +
    UnaryMinus,   // -
    UnaryStar,    // *
    PostfixPlus,  // +
    PostfixMinus, // -
    PostfixStar,  // *

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
        while self.current_char().is_some_and(is_whitespace_char) {
            self.advance();
        }

        let start_pos = self.position;

        match self.current_char() {
            None => Ok(None),
            Some('+') => self.read_plus_operator(start_pos),
            Some('-') => {
                if self.peek_ahead(1) == Some('>') {
                    self.advance();
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::Arrow,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    self.read_minus_operator(start_pos)
                }
            }
            Some('*') => self.read_star_operator(start_pos),
            Some('/') => {
                self.advance();
                match self.current_char() {
                    Some('/') => self.read_line_comment(start_pos).map(Some),
                    _ => Ok(Some(Token {
                        kind: self.read_binary_operator_kind("/", start_pos)?,
                        span: Span::new(start_pos, self.position),
                    })),
                }
            }
            Some('=') => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Ok(Some(Token {
                        kind: self.read_binary_operator_kind("==", start_pos)?,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    Ok(Some(Token {
                        kind: self.read_binary_operator_kind("=", start_pos)?,
                        span: Span::new(start_pos, self.position),
                    }))
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
                if self.peek_ahead(1).is_none_or(|c| !c.is_ascii_digit()) {
                    self.emit_simple(TokenKind::Dot, start_pos)
                } else {
                    // This is likely the start of a floating point number like .5
                    self.read_number(start_pos).map(Some)
                }
            }
            Some('"') => self.read_string_literal(start_pos).map(Some),
            Some('\'') => self.read_char_literal(start_pos).map(Some),
            Some(c) if c.is_ascii_digit() => self.read_number(start_pos).map(Some),
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                self.read_identifier(start_pos).map(Some)
            }
            Some(c) => Err(TokenError::UnexpectedCharacter {
                ch: c,
                position: start_pos,
            }),
        }
    }

    #[inline]
    fn emit_simple(
        &mut self,
        kind: TokenKind,
        start_pos: usize,
    ) -> Result<Option<Token>, TokenError> {
        let t = self.make_simple_token(kind, start_pos)?;
        Ok(Some(t))
    }

    fn read_plus_operator(&mut self, start_pos: usize) -> Result<Option<Token>, TokenError> {
        self.advance();
        let kind = match self.operator_spacing(start_pos, self.position) {
            OperatorSpacing::Prefix => TokenKind::UnaryPlus,
            OperatorSpacing::Postfix => TokenKind::PostfixPlus,
            OperatorSpacing::Infix => TokenKind::Plus,
        };
        Ok(Some(Token {
            kind,
            span: Span::new(start_pos, self.position),
        }))
    }

    fn read_minus_operator(&mut self, start_pos: usize) -> Result<Option<Token>, TokenError> {
        self.advance();
        let kind = match self.operator_spacing(start_pos, self.position) {
            OperatorSpacing::Prefix => TokenKind::UnaryMinus,
            OperatorSpacing::Postfix => TokenKind::PostfixMinus,
            OperatorSpacing::Infix => TokenKind::Minus,
        };
        Ok(Some(Token {
            kind,
            span: Span::new(start_pos, self.position),
        }))
    }

    fn read_star_operator(&mut self, start_pos: usize) -> Result<Option<Token>, TokenError> {
        self.advance();
        let kind = match self.operator_spacing(start_pos, self.position) {
            OperatorSpacing::Prefix => TokenKind::UnaryStar,
            OperatorSpacing::Postfix => TokenKind::PostfixStar,
            OperatorSpacing::Infix => TokenKind::Star,
        };
        Ok(Some(Token {
            kind,
            span: Span::new(start_pos, self.position),
        }))
    }

    fn read_binary_operator_kind(
        &self,
        op: &'static str,
        start_pos: usize,
    ) -> Result<TokenKind, TokenError> {
        if self.operator_spacing(start_pos, self.position) != OperatorSpacing::Infix {
            return Err(TokenError::InvalidOperatorSpacing {
                op,
                position: start_pos,
            });
        }
        match op {
            "/" => Ok(TokenKind::Slash),
            "=" => Ok(TokenKind::Equals),
            "==" => Ok(TokenKind::DoubleEquals),
            _ => unreachable!("unsupported binary operator {}", op),
        }
    }

    fn operator_spacing(&self, start_pos: usize, end_pos: usize) -> OperatorSpacing {
        let left_space = self.left_is_space(start_pos);
        let right_space = self.right_is_space(end_pos);
        if left_space && !right_space {
            OperatorSpacing::Prefix
        } else if !left_space && right_space {
            OperatorSpacing::Postfix
        } else {
            OperatorSpacing::Infix
        }
    }

    fn left_is_space(&self, pos: usize) -> bool {
        if pos == 0 {
            return true;
        }
        self.input[..pos]
            .chars()
            .next_back()
            .is_some_and(is_whitespace_char)
    }

    fn right_is_space(&self, pos: usize) -> bool {
        self.input[pos..]
            .chars()
            .next()
            .is_some_and(is_whitespace_char)
    }

    fn read_number(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        let mut is_float = false;
        let mut seen_digit = false;
        let mut seen_dot = false;
        let mut seen_exp = false;

        let mut radix: Option<u32> = None;
        if self.current_char() == Some('0') {
            if let Some(next) = self.peek_ahead(1) {
                radix = match next {
                    'b' | 'B' => Some(2),
                    'o' | 'O' => Some(8),
                    'x' | 'X' => Some(16),
                    _ => None,
                };
            }
        }

        if let Some(radix) = radix {
            self.advance(); // '0'
            self.advance(); // prefix
            while self.current.is_some() {
                let ch = self.current_char().unwrap_or('_');
                let is_digit = match radix {
                    2 => ch == '0' || ch == '1',
                    8 => ch.is_ascii_digit() && ch < '8',
                    16 => ch.is_ascii_hexdigit(),
                    _ => ch.is_ascii_digit(),
                };
                if is_digit {
                    seen_digit = true;
                    self.advance();
                    continue;
                }
                if ch == '_' {
                    if self.peek_ahead(1).is_some_and(|c| c.is_ascii_alphabetic()) {
                        break;
                    }
                    self.advance();
                    continue;
                }
                break;
            }
        } else {
            while self.current.is_some() {
                let ch = self.current_char().unwrap_or('_');
                if ch.is_ascii_digit() {
                    seen_digit = true;
                    self.advance();
                    continue;
                }
                if ch == '_' {
                    if self.peek_ahead(1).is_some_and(|c| c.is_ascii_alphabetic()) {
                        break;
                    }
                    self.advance();
                    continue;
                }
                if ch == '.' && !seen_dot && !seen_exp {
                    seen_dot = true;
                    is_float = true;
                    self.advance();
                    continue;
                }
                if (ch == 'e' || ch == 'E') && !seen_exp {
                    seen_exp = true;
                    is_float = true;
                    self.advance();
                    if self.current_char().is_some_and(|c| c == '+' || c == '-') {
                        self.advance();
                    }
                    continue;
                }
                break;
            }
        }

        // Handle type suffixes (e.g., 42u32, 3.14f64, 1e3_f64)
        while self
            .current_char()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.advance();
        }

        let text = &self.input[start_pos..self.position];

        // Basic validation - ensure we have at least one digit anywhere in the token
        if !seen_digit {
            return Err(TokenError::InvalidNumber {
                text: text.to_string(),
                position: start_pos,
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

    fn read_identifier(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        while self.current.is_some()
            && self
                .current_char()
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
            "loop" => TokenKind::Loop,
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

        Ok(Token {
            kind,
            span: Span::new(start_pos, self.position),
        })
    }

    fn current_char(&self) -> Option<char> {
        self.current
    }

    fn advance(&mut self) {
        if let Some(ch) = self.current {
            self.position += ch.len_utf8();
            self.current = self.chars.next();
        }
    }

    fn peek_ahead(&self, n: usize) -> Option<char> {
        let mut temp_chars = self.chars.clone();
        for _ in 1..n {
            temp_chars.next();
        }
        temp_chars.next()
    }

    fn read_line_comment(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        // We've already consumed the first '/', now consume the second '/'
        self.advance();

        // Read until newline or end of input
        while self.current.is_some() && !matches!(self.current_char(), Some('\n') | Some('\r')) {
            self.advance();
        }

        Ok(Token {
            kind: TokenKind::Comment,
            span: Span::new(start_pos, self.position),
        })
    }

    fn read_string_literal(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        self.advance(); // consume opening quote

        while self.current.is_some() {
            match self.current_char() {
                Some('"') => {
                    self.advance(); // consume closing quote
                    return Ok(Token {
                        kind: TokenKind::StringLiteral,
                        span: Span::new(start_pos, self.position),
                    });
                }
                Some('\\') => {
                    self.advance(); // consume backslash
                    if self.current.is_some() {
                        self.advance(); // consume escaped character
                    }
                }
                Some(_) => self.advance(),
                None => break,
            }
        }

        // Reached end of input without closing quote
        Err(TokenError::UnterminatedString {
            position: start_pos,
        })
    }

    fn read_char_literal(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        self.advance(); // consume opening quote

        if self.current.is_none() {
            return Err(TokenError::UnterminatedString {
                position: start_pos,
            });
        }

        // Handle escaped characters
        if self.current_char() == Some('\\') {
            self.advance(); // consume backslash
            if self.current.is_none() {
                return Err(TokenError::UnterminatedString {
                    position: start_pos,
                });
            }
            self.advance(); // consume escaped character
        } else {
            self.advance(); // consume regular character
        }

        // Expect closing quote
        if self.current_char() != Some('\'') {
            return Err(TokenError::UnterminatedString {
                position: start_pos,
            });
        }

        self.advance(); // consume closing quote
        Ok(Token {
            kind: TokenKind::CharLiteral,
            span: Span::new(start_pos, self.position),
        })
    }

    fn make_simple_token(
        &mut self,
        kind: TokenKind,
        start_pos: usize,
    ) -> Result<Token, TokenError> {
        self.advance();
        Ok(Token {
            kind,
            span: Span::new(start_pos, self.position),
        })
    }
}
