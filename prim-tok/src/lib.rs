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
    BoolLiteral,

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
    Println,
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
    Newline,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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
        // Skip whitespace (but not newlines - they're significant)
        while self.current_char().is_some_and(|c| c == ' ' || c == '\t') {
            self.advance();
        }

        let start_pos = self.position;

        match self.current_char() {
            None => Ok(None),
            Some('\n') | Some('\r') => self.emit_simple(TokenKind::Newline, start_pos),
            Some('+') => self.emit_simple(TokenKind::Plus, start_pos),
            Some('-') => {
                self.advance();
                if self.current_char() == Some('>') {
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::Arrow,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    Ok(Some(Token {
                        kind: TokenKind::Minus,
                        span: Span::new(start_pos, self.position),
                    }))
                }
            }
            Some('*') => self.emit_simple(TokenKind::Star, start_pos),
            Some('/') => {
                self.advance();
                match self.current_char() {
                    Some('/') => self.read_line_comment(start_pos).map(Some),
                    Some('*') => self.read_block_comment(start_pos).map(Some),
                    _ => Ok(Some(Token {
                        kind: TokenKind::Slash,
                        span: Span::new(start_pos, self.position),
                    })),
                }
            }
            Some('=') => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::DoubleEquals,
                        span: Span::new(start_pos, self.position),
                    }))
                } else {
                    Ok(Some(Token {
                        kind: TokenKind::Equals,
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
            Some('&') => {
                self.advance();
                Ok(Some(Token {
                    kind: TokenKind::Ampersand,
                    span: Span::new(start_pos, self.position),
                }))
            }
            Some('@') => {
                // Allow '@' only for top-level attributes like @runtime(...) or @repr(...)
                // Heuristics: must appear at start of input or right after a newline, ignoring spaces/tabs,
                // and must be followed by an identifier starting with a letter (e.g., runtime, repr).
                let mut is_line_start = true;
                for c in self.input[..start_pos].chars().rev() {
                    if c == ' ' || c == '\t' {
                        continue;
                    }
                    if c == '\n' || c == '\r' {
                        break;
                    }
                    is_line_start = false;
                    break;
                }
                // Peek identifier name after '@'
                let mut ahead = self.chars.clone();
                // current is '@'; skip it
                ahead.next();
                let mut ident = String::new();
                loop {
                    match ahead.clone().next() {
                        Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => {
                            ident.push(ch);
                            ahead.next();
                        }
                        _ => break,
                    }
                }
                if is_line_start {
                    self.emit_simple(TokenKind::At, start_pos)
                } else {
                    Err(TokenError::UnexpectedCharacter {
                        ch: '@',
                        position: start_pos,
                    })
                }
            }
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

    fn read_number(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        let mut is_float = false;

        while self.current.is_some()
            && self
                .current_char()
                .is_some_and(|c| c.is_ascii_digit() || c == '.')
        {
            if self.current_char() == Some('.') {
                if is_float {
                    break;
                }
                is_float = true;
            }
            self.advance();
        }

        // Handle type suffixes (e.g., 42u32, 3.14f64)
        if self.current.is_some() && self.current_char().is_some_and(|c| c.is_ascii_alphabetic()) {
            while self.current.is_some()
                && self
                    .current_char()
                    .is_some_and(|c| c.is_ascii_alphanumeric())
            {
                self.advance();
            }
        }

        let text = &self.input[start_pos..self.position];

        // Basic validation - ensure we have at least one digit anywhere in the token
        if !text.chars().any(|c| c.is_ascii_digit()) {
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
            "println" => TokenKind::Println,
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

    fn read_block_comment(&mut self, start_pos: usize) -> Result<Token, TokenError> {
        // We've already consumed the '/', now consume the '*'
        self.advance();

        // Read until we find '*/' or end of input
        while self.current.is_some() {
            if self.current_char() == Some('*') {
                self.advance();
                if self.current_char() == Some('/') {
                    self.advance();
                    break;
                }
            } else {
                self.advance();
            }
        }

        // Check if we reached end of input without closing the comment
        let text = &self.input[start_pos..self.position];
        if !text.ends_with("*/") {
            return Err(TokenError::UnterminatedComment {
                position: start_pos,
            });
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
