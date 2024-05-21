use crate::position::Position;

#[derive(Debug, Clone)]
pub struct Token {
  kind: TokenKind,
  lexeme: String,
  value: Option<TokenLiteralValue>, 
  position: Position
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
  String(String),
  Identifier(String)
}

#[derive(Debug, Clone)]
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
