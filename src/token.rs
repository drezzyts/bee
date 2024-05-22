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
}

#[derive(Debug, Clone)]
pub enum TokenLiteralValue {
  Integer(i64),
  Float(f64),
  Char(char),
  String(String),
}

#[derive(Debug, Clone, PartialEq)]
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