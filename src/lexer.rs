use crate::{
    error::BeeError,
    position::Position,
    token::{Token, TokenKind, TokenLiteralValue},
};

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    cstart: usize,
    cend: usize,
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::TokenKind;

    #[test]
    fn lex_punctuators() {
        let source = "{ } ( ) . , ;";
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens();

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
        lexer.read_tokens();

        assert_eq!(lexer.tokens.len(), 5);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Plus);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Minus);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Slash);
        assert_eq!(lexer.tokens[3].kind, TokenKind::Star);
        assert_eq!(lexer.tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_bool_operators() {
        let source = "> < >= <= != == = ! && ||";
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens();

        assert_eq!(lexer.tokens.len(), 11);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Greater);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Less);
        assert_eq!(lexer.tokens[2].kind, TokenKind::GreaterEqual);
        assert_eq!(lexer.tokens[3].kind, TokenKind::LessEqual);
        assert_eq!(lexer.tokens[4].kind, TokenKind::BangEqual);
        assert_eq!(lexer.tokens[5].kind, TokenKind::EqualEqual);
        assert_eq!(lexer.tokens[6].kind, TokenKind::Equal);
        assert_eq!(lexer.tokens[7].kind, TokenKind::Bang);
        assert_eq!(lexer.tokens[8].kind, TokenKind::And);
        assert_eq!(lexer.tokens[9].kind, TokenKind::Or);
        assert_eq!(lexer.tokens[10].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_string() {
        let source = r#""Hello World!"
        "Hello,
              World!""#;
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens();

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
        lexer.read_tokens();

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
        lexer.read_tokens();

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
        lexer.read_tokens();

        assert_eq!(lexer.tokens.len(), 3);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Integer);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Float);
        assert_eq!(lexer.tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn lex_keywords() {
        let source =
            r#"class else false fun for if nil echo return super this true mut while"#;
        let mut lexer = Lexer::new(&source.to_string());
        lexer.read_tokens();

        assert_eq!(lexer.tokens.len(), 15);
        assert_eq!(lexer.tokens[0].kind, TokenKind::Class);
        assert_eq!(lexer.tokens[1].kind, TokenKind::Else);
        assert_eq!(lexer.tokens[2].kind, TokenKind::False);
        assert_eq!(lexer.tokens[3].kind, TokenKind::Fun);
        assert_eq!(lexer.tokens[4].kind, TokenKind::For);
        assert_eq!(lexer.tokens[5].kind, TokenKind::If);
        assert_eq!(lexer.tokens[6].kind, TokenKind::Nil);
        assert_eq!(lexer.tokens[7].kind, TokenKind::Echo);
        assert_eq!(lexer.tokens[8].kind, TokenKind::Return);
        assert_eq!(lexer.tokens[9].kind, TokenKind::Super);
        assert_eq!(lexer.tokens[10].kind, TokenKind::This);
        assert_eq!(lexer.tokens[11].kind, TokenKind::True);
        assert_eq!(lexer.tokens[12].kind, TokenKind::Mut);
        assert_eq!(lexer.tokens[13].kind, TokenKind::While);
        assert_eq!(lexer.tokens[14].kind, TokenKind::Eof);
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
            cstart: 1,
            cend: 1,
        }
    }

    pub fn read_tokens(&mut self) -> Result<Vec<Token>, BeeError> {
        while !self.is_eof() {
            self.start = self.current;

            if let Err(message) = self.read_token() {
                let error = BeeError::report(
                    &self.position(),
                    message.as_str(),
                    "lexer",
                    self.source.clone(),
                );

                return Err(error);
            };

            self.cstart = self.cend;
        }

        let eof = Token::new(TokenKind::Eof, "\0".to_string(), None, self.position());
        self.tokens.push(eof);
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
            '?' => self.push_token(TokenKind::Optional, None),
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
            },
            '&' => {
                if self.check_next('&') {
                    self.push_token(TokenKind::And, None);
                } else {
                    return Err(format!("unexpected token found while lexing: '&'"));
                }
            }
            '|' => {
                if self.check_next('|') {
                    self.push_token(TokenKind::Or, None);
                } else {
                    return Err(format!("unexpected token found while lexing: '|'"));
                }
            },
            ':' => {
                if self.check_next(':') {
                    self.push_token(TokenKind::As, None);
                } else {
                    return Err(format!("unexpected token found while lexing: ':'"));
                }
            }
            '"' => self.read_string()?,
            '\'' => self.read_char()?,
            '0'..='9' => self.read_number()?,
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier()?,
            '\r' | '\t' | ' ' => return Ok(()),
            '\n' => {
                self.line += 1;
                self.cend = 1;
                self.cstart = 1;
                return Ok(());
            }
            _ => {
                let invalid_token = self.source[self.start..self.current].to_string().clone();

                return Err(format!("unexpected token found while lexing: '{:?}'", invalid_token));
            }
        }
        Ok(())
    }

    fn read_string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_eof() {
            if self.peek() == '\n' {
                self.line += 1;
                self.cstart = 0;
                self.cend = 0;
            }
            self.next();
        }

        if self.is_eof() {
            return Err("unterminated string founded while lexing".to_string());
        }

        self.next();

        let string = &self.source[self.start + 1..self.current - 1];
        let value = TokenLiteralValue::String(string.to_string());
        self.push_token(TokenKind::String, Some(value));
        Ok(())
    }

    fn read_char(&mut self) -> Result<(), String> {
        if self.is_eof() {
            return Err("unterminated char founded while lexing.".to_string());
        }

        let char = self.next();

        if char == '\'' {
            return Err("empty char founded while lexing.".to_string());
        }

        if self.next() != '\'' {
            return Err("unterminated char founded while lexing.".to_string());
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
                return Err("unexpected internal error ocurred while parsing float.".to_string());
            }
        } else {
            let lexeme = &self.source[self.start..self.current];
            if let Ok(integer) = lexeme.parse() {
                kind = TokenKind::Integer;
                value = TokenLiteralValue::Integer(integer);
            } else {
                return Err("unexpected internal error ocurred while parsing integer.".to_string());
            }
        }

        self.push_token(kind, Some(value));
        Ok(())
    }

    fn read_identifier(&mut self) -> Result<(), String> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.next();
        }

        let lexeme = &self.source[self.start..self.current];

        if let Some(kind) = Token::get_keyword(lexeme) {
            self.push_token(kind, None);
        } else {
            self.push_token(TokenKind::Identifier, None);
        };

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
            self.cend += 1;
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
            cstart: self.cstart,
            cend: self.cend,
        }
    }
    fn is_eof(&self) -> bool {
        self.current >= self.source.len()
    }
}
