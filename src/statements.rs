use crate::{expressions::Expression, visitors::{StatementVisitable, StatementVisitor}};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    Puts(PutsStatement),
}

#[allow(unreachable_patterns)]
impl<T> StatementVisitable<T> for Statement {
  fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
      match self {
        Statement::Expression(expr) => visitor.visit_expr_stmt(expr),
        Statement::Puts(expr) => visitor.visit_puts_stmt(expr),
        _ => unimplemented!()
      }
  }
}
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
  pub expression: Box<Expression>,
}

impl ExpressionStatement {
  pub fn new(expression: Box<Expression>) -> Self {
    Self { expression }
  }
}

#[derive(Debug, Clone)]
pub struct PutsStatement {
  pub expression: Box<Expression>
}

impl PutsStatement {
  pub fn new(expression: Box<Expression>) -> Self {
    Self { expression }
  }
}