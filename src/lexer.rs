use crate::{
    position::Position,
    program::Program,
    token::{Token, TokenKind, TokenLiteralValue},
};

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

#[cfg(test)]
mod tests {
    use crate::token::TokenKind;

    use super::Lexer;

    #[test]
    fn lex_punctuators() {
        let source = "{ } ( ) . , ;";
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 8);
        assert_eq!(lexer.tokens[0].kind, TokenKind::LeftBrace);
        assert_eq!(lexer.tokens[1].kind, TokenKind::RightBrace);
        assert_eq!(lexer.tokens[2].kind, TokenKind::LeftParen);
        assert_eq!(lexer.tokens[3].kind, TokenKind::RightParen);
        assert_eq!(lexer.tokens[4].kind, TokenKind::Dot);
        assert_eq!(lexer.tokens[5].kind, TokenKind::Comma);
        assert_eq!(lexer.tokens[6].kind, TokenKind::SemiColon);
        assert_eq!(lexer.tokens[7].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_math_operators() {
        let source = "+ - / *";
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 5);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Plus);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Minus);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Slash);
        assert_eq!(lexer.tokens[3].kind, TokenKind::Star);
        assert_eq!(lexer.tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_bool_operators() {
        let source = "> < >= <= != == = !";
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 9);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Greater);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Less);
        assert_eq!(lexer.tokens[2].kind, TokenKind::GreaterEqual);
        assert_eq!(lexer.tokens[3].kind, TokenKind::LessEqual);
        assert_eq!(lexer.tokens[4].kind, TokenKind::BangEqual);
        assert_eq!(lexer.tokens[5].kind, TokenKind::EqualEqual);
        assert_eq!(lexer.tokens[6].kind, TokenKind::Equal);
        assert_eq!(lexer.tokens[7].kind, TokenKind::Bang);
        assert_eq!(lexer.tokens[8].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_string() {
        let source = r#""Hello World!"
        "Hello,
              World!""#;
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 3);
        assert_eq!(lexer.tokens[0].kind, TokenKind::String);
        assert_eq!(lexer.tokens[1].kind, TokenKind::String);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_char() {
        let source = r#"'a'
        '1'"#;
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 3);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Char);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Char);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_identifiers() {
        let source = r#"abc
        _abc
        num1
        "#;
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 4);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Identifier);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Identifier);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Identifier);
        assert_eq!(lexer.tokens[3].kind, TokenKind::Eof);
    }

    
    #[test]
    fn lex_numbers() {
        let source = r#"11
        5.5
        "#;
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens().unwrap();

        assert_eq!(lexer.tokens.len(), 3);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Integer);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Float);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Eof);
    }
}

impl Lexer {
    pub fn new(source: &String) -> Self {
        Self {
            source: source.clone(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn read_tokens(&mut self) -> Result<Vec<Token>, String> {
        while !self.is_eof() {
            self.start = self.current;

            if let Err(error) = self.read_token() {
                return Err(error);
            };
        }

        self.push_token(TokenKind::Eof, None);
        Ok(self.tokens.clone())
    }

    fn read_token(&mut self) -> Result<(), String> {
        let c: char = self.next();

        if c == '\0' {
            return Ok(());
        }

        match c {
            '(' => self.push_token(TokenKind::LeftParen, None),
            ')' => self.push_token(TokenKind::RightParen, None),
            '{' => self.push_token(TokenKind::LeftBrace, None),
            '}' => self.push_token(TokenKind::RightBrace, None),
            '.' => self.push_token(TokenKind::Dot, None),
            ';' => self.push_token(TokenKind::SemiColon, None),
            ',' => self.push_token(TokenKind::Comma, None),
            '-' => self.push_token(TokenKind::Minus, None),
            '+' => self.push_token(TokenKind::Plus, None),
            '*' => self.push_token(TokenKind::Star, None),
            '!' => {
                if self.check_next('=') {
                    self.push_token(TokenKind::BangEqual, None)
                } else {
                    self.push_token(TokenKind::Bang, None)
                }
            }
            '>' => {
                if self.check_next('=') {
                    self.push_token(TokenKind::GreaterEqual, None)
                } else {
                    self.push_token(TokenKind::Greater, None)
                }
            }
            '<' => {
                if self.check_next('=') {
                    self.push_token(TokenKind::LessEqual, None)
                } else {
                    self.push_token(TokenKind::Less, None)
                }
            }
            '=' => {
                if self.check_next('=') {
                    self.push_token(TokenKind::EqualEqual, None)
                } else {
                    self.push_token(TokenKind::Equal, None)
                }
            }
            '/' => {
                if self.check_next('/') {
                    while self.peek() != '\n' && !self.is_eof() {
                        self.next();
                    }
                } else {
                    self.push_token(TokenKind::Slash, None)
                }
            }
            '"' => self.read_string()?,
            '\'' => self.read_char()?,
            '0'..='9' => self.read_number()?,
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier()?,
            '\r' | '\t' | ' ' => return Ok(()),
            '\n' => {
                self.line += 1;
                return Ok(());
            }
            _ => {
                let position = Position {
                    line: self.line,
                    cstart: self.start,
                    cend: self.current,
                };
                let invalid_token = self.source[self.start..self.current].to_string().clone();

                return Err(Program::report(
                    position,
                    "lexer",
                    format!("unexpected token found while lexing: {:?}", invalid_token).as_str(),
                ));
            }
        }
        Ok(())
    }

    fn read_string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_eof() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.next();
        }

        if self.is_eof() {
            return Err(Program::report(
                self.position(),
                "lexer",
                "unterminated string founded while lexing",
            ));
        }

        self.next();

        let string = &self.source[self.start + 1..self.current-1];
        let value = TokenLiteralValue::String(string.to_string());
        self.push_token(TokenKind::String, Some(value));
        Ok(())
    }

    fn read_char(&mut self) -> Result<(), String> {
        if self.is_eof() {
            return Err(Program::report(
                self.position(),
                "lexer",
                "unterminated char founded while lexing.",
            ));
        }

        let char = self.next();

        if self.next() != '\'' {
            return Err(Program::report(
                self.position(),
                "lexer",
                "unterminated char founded while lexing.",
            ));
        };

        let value = TokenLiteralValue::Char(char);
        self.push_token(TokenKind::Char, Some(value));
        Ok(())
    }

    fn read_number(&mut self) -> Result<(), String> {
        let kind: TokenKind;
        let value: TokenLiteralValue;

        while self.peek().is_digit(10) {
            self.next();
        }

        if self.peek() == '.' && self.offset_peek(1).is_digit(10) {
            self.next();
            while self.peek().is_digit(10) {
                self.next();
            }

            let lexeme = &self.source[self.start..self.current];
            if let Ok(float) = lexeme.parse() {
                kind = TokenKind::Float;
                value = TokenLiteralValue::Float(float);
            } else {
                return Err(Program::report(
                    self.position(),
                    "lexer",
                    "unexpected internal error ocurred while parsing float.",
                ));
            }
        } else {
            let lexeme = &self.source[self.start..self.current];
            if let Ok(integer) = lexeme.parse() {
                kind = TokenKind::Integer;
                value = TokenLiteralValue::Integer(integer);
            } else {
                return Err(Program::report(
                    self.position(),
                    "lexer",
                    "unexpected internal error ocurred while parsing integer.",
                ));
            }
        }

        self.push_token(kind, Some(value));
        Ok(())
    }

    fn read_identifier(&mut self) -> Result<(), String> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.next();
        }

        self.push_token(TokenKind::Identifier, None);
        Ok(())
    }

    fn push_token(&mut self, kind: TokenKind, value: Option<TokenLiteralValue>) -> () {
        let lexeme = self.source[self.start..self.current].to_string().clone();
        let token = Token::new(kind, lexeme.to_string(), value, self.position());
        self.tokens.push(token);
    }

    fn check_next(&mut self, expected: char) -> bool {
        if self.is_eof() {
            return false;
        }

        if self.source.as_bytes()[self.current] as char != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn next(&mut self) -> char {
        if let Some(next_char) = self.source.chars().nth(self.current) {
            self.current += 1;
            return next_char;
        } else {
            return '\0';
        }
    }

    fn peek(&mut self) -> char {
        self.offset_peek(0)
    }

    fn offset_peek(&mut self, offset: usize) -> char {
        if self.is_eof() || self.current + offset >= self.source.len() {
            return '\0';
        }

        self.source.as_bytes()[self.current + offset] as char
    }

    fn position(&self) -> Position {
        Position {
            line: self.line,
            cstart: self.start,
            cend: self.current,
        }
    }
    fn is_eof(&self) -> bool {
        self.current >= self.source.len()
    }
}
