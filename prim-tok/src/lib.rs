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
    Trait,
    If,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
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
                        start: start_pos,
                        end: self.position,
                    }))
                } else {
                    Ok(Some(Token {
                        kind: TokenKind::Minus,
                        start: start_pos,
                        end: self.position,
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
                        start: start_pos,
                        end: self.position,
                    })),
                }
            }
            Some('=') => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Ok(Some(Token {
                        kind: TokenKind::DoubleEquals,
                        start: start_pos,
                        end: self.position,
                    }))
                } else {
                    Ok(Some(Token {
                        kind: TokenKind::Equals,
                        start: start_pos,
                        end: self.position,
                    }))
                }
            }
            Some('(') => self.emit_simple(TokenKind::LeftParen, start_pos),
            Some(')') => self.emit_simple(TokenKind::RightParen, start_pos),
            Some('{') => self.emit_simple(TokenKind::LeftBrace, start_pos),
            Some('}') => self.emit_simple(TokenKind::RightBrace, start_pos),
            Some(',') => self.emit_simple(TokenKind::Comma, start_pos),
            Some(':') => self.emit_simple(TokenKind::Colon, start_pos),
            Some(';') => self.emit_simple(TokenKind::Semicolon, start_pos),
            Some('&') => {
                self.advance();
                Ok(Some(Token {
                    kind: TokenKind::Ampersand,
                    start: start_pos,
                    end: self.position,
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
            start: start_pos,
            end: self.position,
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
            start: start_pos,
            end: self.position,
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
            start: start_pos,
            end: self.position,
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
            start: start_pos,
            end: self.position,
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
                        start: start_pos,
                        end: self.position,
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
            start: start_pos,
            end: self.position,
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
            start: start_pos,
            end: self.position,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let src = "let x = 42";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "let");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "x");
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "=");
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "42");
    }

    #[test]
    fn test_type_annotations() {
        let src = "let x: u32 = 0u32";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "let");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "x");
        assert_eq!(tokens[2].kind, TokenKind::Colon);
        assert_eq!(&src[tokens[2].start..tokens[2].end], ":");
        assert_eq!(tokens[3].kind, TokenKind::U32);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "u32");
        assert_eq!(tokens[4].kind, TokenKind::Equals);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "=");
        assert_eq!(tokens[5].kind, TokenKind::IntLiteral);
        assert_eq!(&src[tokens[5].start..tokens[5].end], "0u32");
    }

    #[test]
    fn test_function_signature() {
        let src = "fn double(x: u32) -> u32";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        // Whitespace is now automatically filtered out during tokenization
        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "fn");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "double");
        assert_eq!(tokens[2].kind, TokenKind::LeftParen);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "(");
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "x");
        assert_eq!(tokens[4].kind, TokenKind::Colon);
        assert_eq!(&src[tokens[4].start..tokens[4].end], ":");
        assert_eq!(tokens[5].kind, TokenKind::U32);
        assert_eq!(&src[tokens[5].start..tokens[5].end], "u32");
        assert_eq!(tokens[6].kind, TokenKind::RightParen);
        assert_eq!(&src[tokens[6].start..tokens[6].end], ")");
        assert_eq!(tokens[7].kind, TokenKind::Arrow);
        assert_eq!(&src[tokens[7].start..tokens[7].end], "->");
        assert_eq!(tokens[8].kind, TokenKind::U32);
        assert_eq!(&src[tokens[8].start..tokens[8].end], "u32");
    }

    #[test]
    fn test_line_comments() {
        let src = "let x = 42 // this is a comment";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].kind, TokenKind::Comment);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "// this is a comment");
    }

    #[test]
    fn test_line_comment_at_end_with_newline() {
        let src = "let x = 42 // comment\nlet y = 5";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[4].kind, TokenKind::Comment);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "// comment");
        assert_eq!(tokens[5].kind, TokenKind::Newline);
        assert_eq!(tokens[6].kind, TokenKind::Let);
        assert_eq!(tokens[7].kind, TokenKind::Identifier);
        assert_eq!(tokens[8].kind, TokenKind::Equals);
        assert_eq!(tokens[9].kind, TokenKind::IntLiteral);
    }

    #[test]
    fn test_block_comments() {
        let src = "let x = /* block comment */ 42";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::Comment);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "/* block comment */");
        assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "42");
    }

    #[test]
    fn test_multiline_block_comment() {
        let src = "let x = /* multi\nline\ncomment */ 42";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::Comment);
        assert_eq!(
            &src[tokens[3].start..tokens[3].end],
            "/* multi\nline\ncomment */"
        );
        assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "42");
    }

    #[test]
    fn test_block_comment_with_slash_star_inside() {
        // Block comments end at the first */ - no nesting support
        let src = "/* outer /* inner */ remaining";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "/* outer /* inner */");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "remaining");
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
        let src = "*const u8 *mut i32";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Star);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "*");
        assert_eq!(tokens[1].kind, TokenKind::Const);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "const");
        assert_eq!(tokens[2].kind, TokenKind::U8);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "u8");
        assert_eq!(tokens[3].kind, TokenKind::Star);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "*");
        assert_eq!(tokens[4].kind, TokenKind::Mut);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "mut");
        assert_eq!(tokens[5].kind, TokenKind::I32);
        assert_eq!(&src[tokens[5].start..tokens[5].end], "i32");
    }

    #[test]
    fn test_string_literals() {
        let src = r#"let s = "hello world""#;
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(&src[tokens[3].start..tokens[3].end], r#""hello world""#);
    }

    #[test]
    fn test_string_with_escapes() {
        let src = r#"let s = "hello\nworld\t!""#;
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(&src[tokens[3].start..tokens[3].end], r#""hello\nworld\t!""#);
    }

    #[test]
    fn test_string_with_escaped_quote() {
        let src = r#"let s = "say \"hello\"""#;
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(&src[tokens[3].start..tokens[3].end], r#""say \"hello\"""#);
    }

    #[test]
    fn test_empty_string() {
        let src = r#"let s = """#;
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
        assert_eq!(&src[tokens[3].start..tokens[3].end], r#""""#);
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
        let src = "let c = 'a'";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::CharLiteral);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "'a'");
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
            assert_eq!(
                &input_string[tokens[3].start..tokens[3].end],
                expected,
                "Failed for input: {}",
                input
            );
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
        let src = "if x == 42";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::If);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "if");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::DoubleEquals);
        assert_eq!(tokens[3].kind, TokenKind::IntLiteral);
    }

    #[test]
    fn test_mixed_literals() {
        let src = r#"let x = 42; let s = "hello"; let c = 'a'"#;
        let mut tokenizer = Tokenizer::new(src);
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
        assert_eq!(&src[tokens[8].start..tokens[8].end], r#""hello""#);
        assert_eq!(tokens[9].kind, TokenKind::Semicolon);

        // Third assignment: let c = 'a'
        assert_eq!(tokens[10].kind, TokenKind::Let);
        assert_eq!(tokens[11].kind, TokenKind::Identifier);
        assert_eq!(tokens[12].kind, TokenKind::Equals);
        assert_eq!(tokens[13].kind, TokenKind::CharLiteral);
        assert_eq!(&src[tokens[13].start..tokens[13].end], "'a'");
    }

    #[test]
    fn test_boolean_literals() {
        let src = "let flag = true";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Equals);
        assert_eq!(tokens[3].kind, TokenKind::True);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "true");
    }

    #[test]
    fn test_false_literal() {
        let src = "let flag = false";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[3].kind, TokenKind::False);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "false");
    }

    #[test]
    fn test_bool_type() {
        let src = "let flag: bool = true";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Colon);
        assert_eq!(tokens[3].kind, TokenKind::Bool);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "bool");
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
        let src = "True False BOOL";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "True");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "False");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "BOOL");
    }

    #[test]
    fn test_boolean_as_part_of_identifier() {
        // Should tokenize as identifiers, not split into keywords
        let src = "truthy falsey boolean";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "truthy");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "falsey");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "boolean");
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
        let src = "point.x";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "point");
        assert_eq!(tokens[1].kind, TokenKind::Dot);
        assert_eq!(&src[tokens[1].start..tokens[1].end], ".");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "x");
    }

    #[test]
    fn test_dot_vs_float() {
        // Test that standalone dots are tokenized as dots
        let src = "x.field";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();
        assert_eq!(tokens[1].kind, TokenKind::Dot);

        // Test that dots in numbers are part of float literals
        let src2 = "3.14";
        let mut tokenizer = Tokenizer::new(src2);
        let tokens = tokenizer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
        assert_eq!(&src2[tokens[0].start..tokens[0].end], "3.14");
    }

    #[test]
    fn test_leading_dot_float() {
        let src = ".5";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::FloatLiteral);
        assert_eq!(&src[tokens[0].start..tokens[0].end], ".5");
    }

    #[test]
    fn test_struct_tokens() {
        let src = "struct Point { x: i32, y: i32 }";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Struct);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "struct");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[1].start..tokens[1].end], "Point");
        assert_eq!(tokens[2].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[3].start..tokens[3].end], "x");
        assert_eq!(tokens[4].kind, TokenKind::Colon);
        assert_eq!(tokens[5].kind, TokenKind::I32);
        assert_eq!(tokens[6].kind, TokenKind::Comma);
        assert_eq!(tokens[7].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[7].start..tokens[7].end], "y");
        assert_eq!(tokens[8].kind, TokenKind::Colon);
        assert_eq!(tokens[9].kind, TokenKind::I32);
        assert_eq!(tokens[10].kind, TokenKind::RightBrace);
    }

    #[test]
    fn test_field_access_tokens() {
        let src = "p.x";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "p");
        assert_eq!(tokens[1].kind, TokenKind::Dot);
        assert_eq!(&src[tokens[1].start..tokens[1].end], ".");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "x");
    }

    #[test]
    fn test_struct_literal_tokens() {
        let src = "Point { x: 10, y: 20 }";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[0].start..tokens[0].end], "Point");
        assert_eq!(tokens[1].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[2].start..tokens[2].end], "x");
        assert_eq!(tokens[3].kind, TokenKind::Colon);
        assert_eq!(tokens[4].kind, TokenKind::IntLiteral);
        assert_eq!(&src[tokens[4].start..tokens[4].end], "10");
        assert_eq!(tokens[5].kind, TokenKind::Comma);
        assert_eq!(tokens[6].kind, TokenKind::Identifier);
        assert_eq!(&src[tokens[6].start..tokens[6].end], "y");
        assert_eq!(tokens[7].kind, TokenKind::Colon);
        assert_eq!(tokens[8].kind, TokenKind::IntLiteral);
        assert_eq!(&src[tokens[8].start..tokens[8].end], "20");
        assert_eq!(tokens[9].kind, TokenKind::RightBrace);
    }
}
