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
          '\r' | '\t' | ' ' => return Ok(()),  
          '\n' => {
            self.line += 1;
            return Ok(());
          },
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

  fn push_token(&mut self, kind: TokenKind, value: Option<TokenLiteralValue>) -> () {
      let lexeme = self.source[self.start..self.current].to_string().clone();
      let position = Position {
          line: self.line,
          cstart: self.start,
          cend: self.current,
      };
      let token = Token::new(kind, lexeme.to_string(), value, position);
      self.tokens.push(token);
  }

  fn next(&mut self) -> char {
      if let Some(next_char) = self.source.chars().nth(self.current) {
          self.current += 1;
          return next_char;
      } else {
          return '\0';
      }
  }

  fn is_eof(&self) -> bool {
      self.current >= self.source.len().try_into().unwrap()
  }
}
