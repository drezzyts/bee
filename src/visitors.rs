use std::ops::Deref;

use crate::expressions::{self, BinaryExpression, Expression, GroupExpression, LiteralExpression, LiteralValue, UnaryExpression};

pub trait ExpressionVisitor<T> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> T;
    fn visit_group_expr(&mut self, expr: &GroupExpression) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> T;
    fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> T;
}

pub trait ExpressionVisitable<T> {
    fn accept(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T;
}
pub struct PrinterVisitor {
  indent_level: usize,
}

impl PrinterVisitor {
  pub fn new() -> Self {
      Self { indent_level: 1 }
  }

  fn indent(&self) -> String {
      "\t".repeat(self.indent_level)
  }

  pub fn print(&mut self, expr: &Expression) -> String {
      expr.accept(self as &mut dyn ExpressionVisitor<String>)
  }
}

impl ExpressionVisitor<String> for PrinterVisitor {
  fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> String {
      self.indent_level += 1;
      let left = expr.left.accept(self);
      let operator = expr.operator.lexeme.clone();
      let right = expr.right.accept(self);
      self.indent_level -= 1;

      format!(
          "(Binary Expression) -> {}\n{}Left:\n{}{}\n{}Right:\n{}{}",
          operator,
          self.indent(),
          self.indent(),
          left,
          self.indent(),
          self.indent(),
          right
      )
  }

  fn visit_group_expr(&mut self, expr: &GroupExpression) -> String {
      self.indent_level += 1;
      let expression = expr.expr.accept(self);
      self.indent_level -= 1;

      format!(
          "(Group Expression) --> Expression: (\n{}{}\n{})",
          self.indent(),
          expression,
          self.indent()
      )
  }

  fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> String {
      self.indent_level += 1;
      let operator = expr.operator.lexeme.clone();
      let expression = expr.right.accept(self);
      self.indent_level -= 1;

      format!(
          "(Unary Expression) --> \n{}Expression: \n{}{}{}",
          self.indent(),
          self.indent(),
          operator,
          expression
      )
  }

  fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> String {
      match &expr.value {
          LiteralValue::Float(value) => format!("{:?}", value),
          LiteralValue::Integer(value) => format!("{}", value),
          LiteralValue::String(value) => format!("\"{}\"", value),
          LiteralValue::Char(value) => format!("'{}'", value),
          LiteralValue::Nil => String::from("nil"),
          LiteralValue::True => String::from("true"),
          LiteralValue::False => String::from("false"),
          LiteralValue::NaN => String::from("nan")
      }
  }
}