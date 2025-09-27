mod error;
pub use error::TokenError;

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
    If,
    Println,
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
    LeftParen,  // (
    RightParen, // )
    LeftBrace,  // {
    RightBrace, // }
    Comma,      // ,
    Colon,      // :
    Semicolon,  // ;
    Ampersand,  // &
    Dot,        // .
    At,         // @

    // Special
    Comment,
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub position: usize,
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

    pub fn tokenize(&mut self) -> Result<Vec<Token<'a>>, TokenError> {
        let mut tokens = Vec::new();

        while self.current.is_some() {
            let token = self.next_token()?;
            tokens.push(token);
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            text: "",
            position: self.position,
        });

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token<'a>, TokenError> {
        // Skip whitespace (but not newlines - they're significant)
        while self.current_char().is_some_and(|c| c == ' ' || c == '\t') {
            self.advance();
        }

        let start_pos = self.position;

        match self.current_char() {
            Some('\n') | Some('\r') => self.make_simple_token(TokenKind::Newline, start_pos),
            Some('+') => self.make_simple_token(TokenKind::Plus, start_pos),
            Some('-') => {
                self.advance();
                if self.current_char() == Some('>') {
                    self.advance();
                    Ok(Token {
                        kind: TokenKind::Arrow,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    })
                } else {
                    Ok(Token {
                        kind: TokenKind::Minus,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    })
                }
            }
            Some('*') => self.make_simple_token(TokenKind::Star, start_pos),
            Some('/') => {
                self.advance();
                match self.current_char() {
                    Some('/') => self.read_line_comment(start_pos),
                    Some('*') => self.read_block_comment(start_pos),
                    _ => Ok(Token {
                        kind: TokenKind::Slash,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    }),
                }
            }
            Some('=') => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Ok(Token {
                        kind: TokenKind::DoubleEquals,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    })
                } else {
                    Ok(Token {
                        kind: TokenKind::Equals,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    })
                }
            }
            Some('(') => self.make_simple_token(TokenKind::LeftParen, start_pos),
            Some(')') => self.make_simple_token(TokenKind::RightParen, start_pos),
            Some('{') => self.make_simple_token(TokenKind::LeftBrace, start_pos),
            Some('}') => self.make_simple_token(TokenKind::RightBrace, start_pos),
            Some(',') => self.make_simple_token(TokenKind::Comma, start_pos),
            Some(':') => self.make_simple_token(TokenKind::Colon, start_pos),
            Some(';') => self.make_simple_token(TokenKind::Semicolon, start_pos),
            Some('&') => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Ampersand,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                })
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
                let is_known_attr = ident == "runtime" || ident == "repr";
                if is_line_start && is_known_attr {
                    self.make_simple_token(TokenKind::At, start_pos)
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
                    self.make_simple_token(TokenKind::Dot, start_pos)
                } else {
                    // This is likely the start of a floating point number like .5
                    self.read_number(start_pos)
                }
            }
            Some('"') => self.read_string_literal(start_pos),
            Some('\'') => self.read_char_literal(start_pos),
            Some(c) if c.is_ascii_digit() => self.read_number(start_pos),
            Some(c) if c.is_ascii_alphabetic() || c == '_' => self.read_identifier(start_pos),
            Some(c) => Err(TokenError::UnexpectedCharacter {
                ch: c,
                position: start_pos,
            }),
            None => Err(TokenError::UnexpectedCharacter {
                ch: '\0',
                position: start_pos,
            }),
        }
    }

    fn read_number(&mut self, start_pos: usize) -> Result<Token<'a>, TokenError> {
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
            text,
            position: start_pos,
        })
    }

    fn read_identifier(&mut self, start_pos: usize) -> Result<Token<'a>, TokenError> {
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
            "if" => TokenKind::If,
            "println" => TokenKind::Println,
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
            text,
            position: start_pos,
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

    fn read_line_comment(&mut self, start_pos: usize) -> Result<Token<'a>, TokenError> {
        // We've already consumed the first '/', now consume the second '/'
        self.advance();

        // Read until newline or end of input
        while self.current.is_some() && !matches!(self.current_char(), Some('\n') | Some('\r')) {
            self.advance();
        }

        Ok(Token {
            kind: TokenKind::Comment,
            text: &self.input[start_pos..self.position],
            position: start_pos,
        })
    }

    fn read_block_comment(&mut self, start_pos: usize) -> Result<Token<'a>, TokenError> {
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
            text,
            position: start_pos,
        })
    }

    fn read_string_literal(&mut self, start_pos: usize) -> Result<Token<'a>, TokenError> {
        self.advance(); // consume opening quote

        while self.current.is_some() {
            match self.current_char() {
                Some('"') => {
                    self.advance(); // consume closing quote
                    return Ok(Token {
                        kind: TokenKind::StringLiteral,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
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

    fn read_char_literal(&mut self, start_pos: usize) -> Result<Token<'a>, TokenError> {
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
            text: &self.input[start_pos..self.position],
            position: start_pos,
        })
    }

    fn make_simple_token(
        &mut self,
        kind: TokenKind,
        start_pos: usize,
    ) -> Result<Token<'a>, TokenError> {
        self.advance();
        Ok(Token {
            kind,
            text: &self.input[start_pos..self.position],
            position: start_pos,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut tokenizer = Tokenizer::new("let x = 42");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[0].text, "let");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "x");
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[2].text, "=");
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[3].text, "42");
    }

    #[test]
    fn test_type_annotations() {
        let mut tokenizer = Tokenizer::new("let x: u32 = 0u32");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[0].text, "let");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "x");
        assert_eq!(tokens[2].kind, TokenKind::Colon);
        assert_eq!(tokens[2].text, ":");
        assert_eq!(tokens[3].kind, TokenKind::U32);
        assert_eq!(tokens[3].text, "u32");
        assert_eq!(tokens[4].kind, TokenKind::Equals);
        assert_eq!(tokens[4].text, "=");
        assert_eq!(tokens[5].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[5].text, "0u32");
    }

    #[test]
    fn test_function_signature() {
        let mut tokenizer = Tokenizer::new("fn double(x: u32) -> u32");
        let tokens = tokenizer.tokenize().unwrap();

        // Whitespace is now automatically filtered out during tokenization
        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(tokens[0].text, "fn");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "double");
        assert_eq!(tokens[2].kind, TokenKind::LeftParen);
        assert_eq!(tokens[2].text, "(");
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(tokens[3].text, "x");
        assert_eq!(tokens[4].kind, TokenKind::Colon);
        assert_eq!(tokens[4].text, ":");
        assert_eq!(tokens[5].kind, TokenKind::U32);
        assert_eq!(tokens[5].text, "u32");
        assert_eq!(tokens[6].kind, TokenKind::RightParen);
        assert_eq!(tokens[6].text, ")");
        assert_eq!(tokens[7].kind, TokenKind::Arrow);
        assert_eq!(tokens[7].text, "->");
        assert_eq!(tokens[8].kind, TokenKind::U32);
        assert_eq!(tokens[8].text, "u32");
    }

    #[test]
    fn test_line_comments() {
        let mut tokenizer = Tokenizer::new("let x = 42 // this is a comment");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].kind, TokenKind::Comment);
        assert_eq!(tokens[4].text, "// this is a comment");
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_line_comment_at_end_with_newline() {
        let mut tokenizer = Tokenizer::new("let x = 42 // comment\nlet y = 5");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].kind, TokenKind::Comment);
        assert_eq!(tokens[4].text, "// comment");
        assert_eq!(tokens[5].kind, TokenKind::Newline);
        assert_eq!(tokens[6].kind, TokenKind::Let);
        assert_eq!(tokens[7].kind, TokenKind::Identifier);
        assert_eq!(tokens[8].kind, TokenKind::Equals);
        assert_eq!(tokens[9].kind, TokenKind::IntLiteral);
    }

    #[test]
    fn test_block_comments() {
        let mut tokenizer = Tokenizer::new("let x = /* block comment */ 42");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::Comment);
        assert_eq!(tokens[3].text, "/* block comment */");
        assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].text, "42");
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_multiline_block_comment() {
        let mut tokenizer = Tokenizer::new("let x = /* multi\nline\ncomment */ 42");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::Comment);
        assert_eq!(tokens[3].text, "/* multi\nline\ncomment */");
        assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].text, "42");
    }

    #[test]
    fn test_block_comment_with_slash_star_inside() {
        // Block comments end at the first */ - no nesting support
        let mut tokenizer = Tokenizer::new("/* outer /* inner */ remaining");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(tokens[0].text, "/* outer /* inner */");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "remaining");
        assert_eq!(tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn test_unterminated_block_comment() {
        let mut tokenizer = Tokenizer::new("let x = /* unterminated comment");
        let result = tokenizer.tokenize();

        match result {
            Err(TokenError::UnterminatedComment { position }) => {
                assert_eq!(position, 8); // Position of /*
            }
            _ => panic!("Expected UnterminatedComment error, got {:?}", result),
        }
    }

    #[test]
    fn test_division_vs_comments() {
        let mut tokenizer = Tokenizer::new("let x = a / b");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(tokens[4].kind, TokenKind::Slash);
        assert_eq!(tokens[5].kind, TokenKind::Identifier);
    }

    #[test]
    fn test_pointer_keywords() {
        let mut tokenizer = Tokenizer::new("*const u8 *mut i32");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Star);
        assert_eq!(tokens[0].text, "*");
        assert_eq!(tokens[1].kind, TokenKind::Const);
        assert_eq!(tokens[1].text, "const");
        assert_eq!(tokens[2].kind, TokenKind::U8);
        assert_eq!(tokens[2].text, "u8");
        assert_eq!(tokens[3].kind, TokenKind::Star);
        assert_eq!(tokens[3].text, "*");
        assert_eq!(tokens[4].kind, TokenKind::Mut);
        assert_eq!(tokens[4].text, "mut");
        assert_eq!(tokens[5].kind, TokenKind::I32);
        assert_eq!(tokens[5].text, "i32");
    }

    #[test]
    fn test_string_literals() {
        let mut tokenizer = Tokenizer::new(r#"let s = "hello world""#);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[3].text, r#""hello world""#);
        assert_eq!(tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn test_string_with_escapes() {
        let mut tokenizer = Tokenizer::new(r#"let s = "hello\nworld\t!""#);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[3].text, r#""hello\nworld\t!""#);
    }

    #[test]
    fn test_string_with_escaped_quote() {
        let mut tokenizer = Tokenizer::new(r#"let s = "say \"hello\"""#);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[3].text, r#""say \"hello\"""#);
    }

    #[test]
    fn test_empty_string() {
        let mut tokenizer = Tokenizer::new(r#"let s = """#);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[3].text, r#""""#);
    }

    #[test]
    fn test_unterminated_string() {
        let mut tokenizer = Tokenizer::new(r#"let s = "unterminated"#);
        let result = tokenizer.tokenize();

        match result {
            Err(TokenError::UnterminatedString { position }) => {
                assert_eq!(position, 8); // Position of opening quote
            }
            _ => panic!("Expected UnterminatedString error, got {:?}", result),
        }
    }

    #[test]
    fn test_char_literals() {
        let mut tokenizer = Tokenizer::new("let c = 'a'");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::CharLiteral);
        assert_eq!(tokens[3].text, "'a'");
        assert_eq!(tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn test_escaped_char_literals() {
        let test_cases = vec![
            ("'\\n'", r"'\n'"),
            ("'\\t'", r"'\t'"),
            ("'\\''", r"'\''"),
            ("'\\\\'", r"'\\'"),
        ];

        for (input, expected) in test_cases {
            let input_string = format!("let c = {}", input);
            let mut tokenizer = Tokenizer::new(&input_string);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens[3].kind, TokenKind::CharLiteral);
            assert_eq!(tokens[3].text, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_unterminated_char_literal() {
        let mut tokenizer = Tokenizer::new("let c = 'a");
        let result = tokenizer.tokenize();

        match result {
            Err(TokenError::UnterminatedString { position }) => {
                assert_eq!(position, 8); // Position of opening quote
            }
            _ => panic!("Expected UnterminatedString error, got {:?}", result),
        }
    }

    #[test]
    fn test_if_keyword() {
        let mut tokenizer = Tokenizer::new("if x == 42");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::If);
        assert_eq!(tokens[0].text, "if");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::DoubleEquals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    }

    #[test]
    fn test_mixed_literals() {
        let mut tokenizer = Tokenizer::new(r#"let x = 42; let s = "hello"; let c = 'a'"#);
        let tokens = tokenizer.tokenize().unwrap();

        // First assignment: let x = 42
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].kind, TokenKind::Semicolon);

        // Second assignment: let s = "hello"
        assert_eq!(tokens[5].kind, TokenKind::Let);
        assert_eq!(tokens[6].kind, TokenKind::Identifier);
        assert_eq!(tokens[7].kind, TokenKind::Equals);
        assert_eq!(tokens[8].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[8].text, r#""hello""#);
        assert_eq!(tokens[9].kind, TokenKind::Semicolon);

        // Third assignment: let c = 'a'
        assert_eq!(tokens[10].kind, TokenKind::Let);
        assert_eq!(tokens[11].kind, TokenKind::Identifier);
        assert_eq!(tokens[12].kind, TokenKind::Equals);
        assert_eq!(tokens[13].kind, TokenKind::CharLiteral);
        assert_eq!(tokens[13].text, "'a'");
    }

    #[test]
    fn test_boolean_literals() {
        let mut tokenizer = Tokenizer::new("let flag = true");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::True);
        assert_eq!(tokens[3].text, "true");
        assert_eq!(tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn test_false_literal() {
        let mut tokenizer = Tokenizer::new("let flag = false");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::False);
        assert_eq!(tokens[3].text, "false");
    }

    #[test]
    fn test_bool_type() {
        let mut tokenizer = Tokenizer::new("let flag: bool = true");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Colon);
        assert_eq!(tokens[3].kind, TokenKind::Bool);
        assert_eq!(tokens[3].text, "bool");
        assert_eq!(tokens[4].kind, TokenKind::Equals);
        assert_eq!(tokens[5].kind, TokenKind::True);
    }

    #[test]
    fn test_boolean_expressions() {
        let mut tokenizer = Tokenizer::new("if flag == true");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::If);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::DoubleEquals);
        assert_eq!(tokens[3].kind, TokenKind::True);
    }

    #[test]
    fn test_mixed_boolean_and_other_literals() {
        let mut tokenizer = Tokenizer::new(r#"let x = 42; let flag = true; let name = "test""#);
        let tokens = tokenizer.tokenize().unwrap();

        // First: let x = 42
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].kind, TokenKind::Semicolon);

        // Second: let flag = true
        assert_eq!(tokens[5].kind, TokenKind::Let);
        assert_eq!(tokens[6].kind, TokenKind::Identifier);
        assert_eq!(tokens[7].kind, TokenKind::Equals);
        assert_eq!(tokens[8].kind, TokenKind::True);
        assert_eq!(tokens[9].kind, TokenKind::Semicolon);

        // Third: let name = "test"
        assert_eq!(tokens[10].kind, TokenKind::Let);
        assert_eq!(tokens[11].kind, TokenKind::Identifier);
        assert_eq!(tokens[12].kind, TokenKind::Equals);
        assert_eq!(tokens[13].kind, TokenKind::StringLiteral);
    }

    #[test]
    fn test_boolean_in_function_parameters() {
        let mut tokenizer = Tokenizer::new("fn test(active: bool, count: u32)");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::LeftParen);
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(tokens[4].kind, TokenKind::Colon);
        assert_eq!(tokens[5].kind, TokenKind::Bool);
        assert_eq!(tokens[6].kind, TokenKind::Comma);
        assert_eq!(tokens[7].kind, TokenKind::Identifier);
        assert_eq!(tokens[8].kind, TokenKind::Colon);
        assert_eq!(tokens[9].kind, TokenKind::U32);
        assert_eq!(tokens[10].kind, TokenKind::RightParen);
    }

    #[test]
    fn test_boolean_keywords_case_sensitive() {
        // Should tokenize as identifiers, not boolean keywords
        let mut tokenizer = Tokenizer::new("True False BOOL");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].text, "True");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "False");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "BOOL");
    }

    #[test]
    fn test_boolean_as_part_of_identifier() {
        // Should tokenize as identifiers, not split into keywords
        let mut tokenizer = Tokenizer::new("truthy falsey boolean");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].text, "truthy");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "falsey");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "boolean");
    }

    #[test]
    fn test_error_unexpected_character() {
        let mut tokenizer = Tokenizer::new("let x = @");
        let result = tokenizer.tokenize();

        match result {
            Err(TokenError::UnexpectedCharacter { ch, position }) => {
                assert_eq!(ch, '@');
                assert_eq!(position, 8);
            }
            _ => panic!("Expected UnexpectedCharacter error, got {:?}", result),
        }
    }

    #[test]
    fn test_dot_token() {
        let mut tokenizer = Tokenizer::new("point.x");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].text, "point");
        assert_eq!(tokens[1].kind, TokenKind::Dot);
        assert_eq!(tokens[1].text, ".");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "x");
        assert_eq!(tokens[3].kind, TokenKind::Eof);
    }

    #[test]
    fn test_dot_vs_float() {
        // Test that standalone dots are tokenized as dots
        let mut tokenizer = Tokenizer::new("x.field");
        let tokens = tokenizer.tokenize().unwrap();
        assert_eq!(tokens[1].kind, TokenKind::Dot);

        // Test that dots in numbers are part of float literals
        let mut tokenizer = Tokenizer::new("3.14");
        let tokens = tokenizer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
        assert_eq!(tokens[0].text, "3.14");
    }

    #[test]
    fn test_leading_dot_float() {
        let mut tokenizer = Tokenizer::new(".5");
        let tokens = tokenizer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
        assert_eq!(tokens[0].text, ".5");
    }

    #[test]
    fn test_struct_tokens() {
        let mut tokenizer = Tokenizer::new("struct Point { x: i32, y: i32 }");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Struct);
        assert_eq!(tokens[0].text, "struct");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "Point");
        assert_eq!(tokens[2].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(tokens[3].text, "x");
        assert_eq!(tokens[4].kind, TokenKind::Colon);
        assert_eq!(tokens[5].kind, TokenKind::I32);
        assert_eq!(tokens[6].kind, TokenKind::Comma);
        assert_eq!(tokens[7].kind, TokenKind::Identifier);
        assert_eq!(tokens[7].text, "y");
        assert_eq!(tokens[8].kind, TokenKind::Colon);
        assert_eq!(tokens[9].kind, TokenKind::I32);
        assert_eq!(tokens[10].kind, TokenKind::RightBrace);
    }

    #[test]
    fn test_field_access_tokens() {
        let mut tokenizer = Tokenizer::new("p.x");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].text, "p");
        assert_eq!(tokens[1].kind, TokenKind::Dot);
        assert_eq!(tokens[1].text, ".");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "x");
        assert_eq!(tokens[3].kind, TokenKind::Eof);
    }

    #[test]
    fn test_struct_literal_tokens() {
        let mut tokenizer = Tokenizer::new("Point { x: 10, y: 20 }");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].text, "Point");
        assert_eq!(tokens[1].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "x");
        assert_eq!(tokens[3].kind, TokenKind::Colon);
        assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].text, "10");
        assert_eq!(tokens[5].kind, TokenKind::Comma);
        assert_eq!(tokens[6].kind, TokenKind::Identifier);
        assert_eq!(tokens[6].text, "y");
        assert_eq!(tokens[7].kind, TokenKind::Colon);
        assert_eq!(tokens[8].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[8].text, "20");
        assert_eq!(tokens[9].kind, TokenKind::RightBrace);
    }
}
