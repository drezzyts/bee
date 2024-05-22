use std::collections::HashMap;

use crate::position::Position;

#[derive(Debug, Clone)]
pub struct Token {
  pub kind: TokenKind,
  pub lexeme: String,
  pub value: Option<TokenLiteralValue>, 
  pub position: Position
}

impl Token {
  pub fn new(kind: TokenKind, lexeme: String, value: Option<TokenLiteralValue>, position: Position) -> Self {
    Self { kind, lexeme, value, position }
  }

  pub fn get_keyword(lexeme: &str) -> Option<TokenKind> {
    let keywords: HashMap<&str, TokenKind> = HashMap::from([
      ("and", TokenKind::And),
      ("class", TokenKind::Class),
      ("else", TokenKind::Else),
      ("false", TokenKind::False),
      ("fun", TokenKind::Fun),
      ("for", TokenKind::For),
      ("if", TokenKind::If),
      ("none", TokenKind::None),
      ("or", TokenKind::Or),
      ("puts", TokenKind::Puts),
      ("return", TokenKind::Return),
      ("super", TokenKind::Super),
      ("this", TokenKind::This),
      ("true", TokenKind::True),
      ("let", TokenKind::Let),
      ("while", TokenKind::While),
    ]);

    if let Some(kind) = keywords.get(lexeme) {
      Some(kind.clone())
    } else {
      None
    }
  }
}

#[derive(Debug, Clone)]
pub enum TokenLiteralValue {
  Integer(i64),
  Float(f64),
  Char(char),
  String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Punctuators
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // Operators
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Char,
    Float,
    Integer,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    None,
    Or,
    Puts,
    Return,
    Super,
    This,
    True,
    Let,
    While,

    // Others
    Eof,
}
