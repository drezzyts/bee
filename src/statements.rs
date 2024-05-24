use crate::{expressions::Expression, visitors::{StatementVisitable, StatementVisitor}};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    Echo(EchoStatement),
}

#[allow(unreachable_patterns)]
impl<T> StatementVisitable<T> for Statement {
  fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
      match self {
        Statement::Expression(expr) => visitor.visit_expr_stmt(expr),
        Statement::Echo(expr) => visitor.visit_echo_stmt(expr),
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
pub struct EchoStatement {
  pub expression: Box<Expression>
}

impl EchoStatement {
  pub fn new(expression: Box<Expression>) -> Self {
    Self { expression }
  }
}