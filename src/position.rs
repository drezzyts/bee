use core::fmt;

#[derive(Debug, Clone)]
pub struct Position {
  pub line: usize,
  pub cstart: usize,
  pub cend: usize
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(l:{}) (c:{}..{})", self.line, self.cstart, self.cend)
    }
}