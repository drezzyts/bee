use core::fmt;

pub struct Position {
  pub line: i32,
  pub cstart: i32,
  pub cend: i32
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(l:{}) (c:{}..{})", self.line, self.cstart, self.cend)
    }
}