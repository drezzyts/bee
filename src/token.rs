use std::collections::HashMap;

use crate::{expressions::LiteralValue, position::Position};

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
      ("class", TokenKind::Class),
      ("else", TokenKind::Else),
      ("false", TokenKind::False),
      ("fun", TokenKind::Fun),
      ("for", TokenKind::For),
      ("if", TokenKind::If),
      ("nil", TokenKind::Nil),
      ("echo", TokenKind::Echo),
      ("return", TokenKind::Return),
      ("super", TokenKind::Super),
      ("this", TokenKind::This),
      ("true", TokenKind::True),
      ("mut", TokenKind::Mut),
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

#[allow(unreachable_patterns)]
impl TokenLiteralValue {
  pub fn to_literal_value(&self) -> LiteralValue {
    match self {
      TokenLiteralValue::Integer(val) => LiteralValue::Integer(val.clone()),
      TokenLiteralValue::Float(val) => LiteralValue::Float(val.clone()),
      TokenLiteralValue::Char(val) => LiteralValue::Char(val.clone()),
      TokenLiteralValue::String(val) => LiteralValue::String(val.clone()),
      _ => unreachable!()
    }
  }
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
    Optional,

    // Operators
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Or,
    And,

    // Literals.
    Identifier,
    String,
    Char,
    Float,
    Integer,

    // Keywords.
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Echo,
    Return,
    Super,
    This,
    True,
    Mut,
    While,

    // Others
    Eof,
}
