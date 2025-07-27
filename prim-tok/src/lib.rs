#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    IntLiteral,
    FloatLiteral,
    
    // Keywords
    Let,
    Struct,
    Fn,
    Impl,
    If,
    Println,
    
    // Types
    U8, I8, U16, I16, U32, I32, U64, I64, Usize, Isize,
    F32, F64,
    
    // Identifiers
    Identifier,
    
    // Operators
    Plus,      // +
    Minus,     // -
    Star,      // *
    Equals,    // =
    DoubleEquals, // ==
    Arrow,     // ->
    
    // Punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    Comma,        // ,
    Colon,        // :
    Semicolon,    // ;
    Ampersand,    // &
    
    // Special
    Whitespace,
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
    
    pub fn tokenize(&mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();
        
        while self.current.is_some() {
            let token = self.next_token();
            tokens.push(token);
        }
        
        tokens.push(Token {
            kind: TokenKind::Eof,
            text: "",
            position: self.position,
        });
        
        tokens
    }
    
    fn next_token(&mut self) -> Token<'a> {
        let start_pos = self.position;
        let ch = self.current_char();
        
        match ch {
            ' ' | '\t' => {
                self.advance();
                Token {
                    kind: TokenKind::Whitespace,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '\n' | '\r' => {
                self.advance();
                Token {
                    kind: TokenKind::Newline,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '+' => {
                self.advance();
                Token {
                    kind: TokenKind::Plus,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '-' => {
                self.advance();
                if self.current_char() == '>' {
                    self.advance();
                    Token {
                        kind: TokenKind::Arrow,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    }
                } else {
                    Token {
                        kind: TokenKind::Minus,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    }
                }
            }
            '*' => {
                self.advance();
                Token {
                    kind: TokenKind::Star,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '=' => {
                self.advance();
                if self.current_char() == '=' {
                    self.advance();
                    Token {
                        kind: TokenKind::DoubleEquals,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    }
                } else {
                    Token {
                        kind: TokenKind::Equals,
                        text: &self.input[start_pos..self.position],
                        position: start_pos,
                    }
                }
            }
            '(' => {
                self.advance();
                Token {
                    kind: TokenKind::LeftParen,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            ')' => {
                self.advance();
                Token {
                    kind: TokenKind::RightParen,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '{' => {
                self.advance();
                Token {
                    kind: TokenKind::LeftBrace,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '}' => {
                self.advance();
                Token {
                    kind: TokenKind::RightBrace,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            ',' => {
                self.advance();
                Token {
                    kind: TokenKind::Comma,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            ':' => {
                self.advance();
                Token {
                    kind: TokenKind::Colon,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            ';' => {
                self.advance();
                Token {
                    kind: TokenKind::Semicolon,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            '&' => {
                self.advance();
                Token {
                    kind: TokenKind::Ampersand,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
            _ if ch.is_ascii_digit() => self.read_number(start_pos),
            _ if ch.is_ascii_alphabetic() || ch == '_' => self.read_identifier(start_pos),
            _ => {
                self.advance();
                Token {
                    kind: TokenKind::Identifier,
                    text: &self.input[start_pos..self.position],
                    position: start_pos,
                }
            }
        }
    }
    
    fn read_number(&mut self, start_pos: usize) -> Token<'a> {
        let mut is_float = false;
        
        while self.current.is_some() && (self.current_char().is_ascii_digit() || self.current_char() == '.') {
            if self.current_char() == '.' {
                if is_float {
                    break;
                }
                is_float = true;
            }
            self.advance();
        }
        
        // Handle type suffixes (e.g., 42u32, 3.14f64)
        if self.current.is_some() && self.current_char().is_ascii_alphabetic() {
            while self.current.is_some() && (self.current_char().is_ascii_alphanumeric()) {
                self.advance();
            }
        }
        
        Token {
            kind: if is_float {
                TokenKind::FloatLiteral
            } else {
                TokenKind::IntLiteral
            },
            text: &self.input[start_pos..self.position],
            position: start_pos,
        }
    }
    
    fn read_identifier(&mut self, start_pos: usize) -> Token<'a> {
        while self.current.is_some() && (self.current_char().is_ascii_alphanumeric() || self.current_char() == '_') {
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
            _ => TokenKind::Identifier,
        };
        
        Token {
            kind,
            text,
            position: start_pos,
        }
    }
    
    fn current_char(&self) -> char {
        self.current.unwrap_or('\0')
    }
    
    fn advance(&mut self) {
        if self.current.is_some() {
            self.position += self.current.unwrap().len_utf8();
            self.current = self.chars.next();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut tokenizer = Tokenizer::new("let x = 42");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[0].text, "let");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "x");
        assert_eq!(tokens[4].kind, TokenKind::Equals);
        assert_eq!(tokens[4].text, "=");
        assert_eq!(tokens[6].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[6].text, "42");
    }
    
    #[test]
    fn test_type_annotations() {
        let mut tokenizer = Tokenizer::new("let x: u32 = 0u32");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[0].text, "let");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].text, "x");
        assert_eq!(tokens[3].kind, TokenKind::Colon);
        assert_eq!(tokens[3].text, ":");
        assert_eq!(tokens[5].kind, TokenKind::U32);
        assert_eq!(tokens[5].text, "u32");
    }
    
    #[test]
    fn test_function_signature() {
        let mut tokenizer = Tokenizer::new("fn double(x: u32) -> u32");
        let tokens = tokenizer.tokenize();
        
        // Filter out whitespace for easier testing
        let non_whitespace: Vec<_> = tokens.into_iter()
            .filter(|t| !matches!(t.kind, TokenKind::Whitespace))
            .collect();
        
        assert_eq!(non_whitespace[0].kind, TokenKind::Fn);
        assert_eq!(non_whitespace[0].text, "fn");
        assert_eq!(non_whitespace[1].kind, TokenKind::Identifier);
        assert_eq!(non_whitespace[1].text, "double");
        assert_eq!(non_whitespace[2].kind, TokenKind::LeftParen);
        assert_eq!(non_whitespace[2].text, "(");
        assert_eq!(non_whitespace[3].kind, TokenKind::Identifier);
        assert_eq!(non_whitespace[3].text, "x");
        assert_eq!(non_whitespace[4].kind, TokenKind::Colon);
        assert_eq!(non_whitespace[4].text, ":");
        assert_eq!(non_whitespace[5].kind, TokenKind::U32);
        assert_eq!(non_whitespace[5].text, "u32");
        assert_eq!(non_whitespace[6].kind, TokenKind::RightParen);
        assert_eq!(non_whitespace[6].text, ")");
        assert_eq!(non_whitespace[7].kind, TokenKind::Arrow);
        assert_eq!(non_whitespace[7].text, "->");
        assert_eq!(non_whitespace[8].kind, TokenKind::U32);
        assert_eq!(non_whitespace[8].text, "u32");
    }
}
