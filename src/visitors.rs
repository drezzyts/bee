use std::ops::Deref;

use crate::expressions::{BinaryExpression, Expression, GroupExpression, LiteralExpression, LiteralValue, UnaryExpression};

pub trait ExpressionVisitor<T> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> T;
    fn visit_group_expr(&mut self, expr: &GroupExpression) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> T;
    fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> T;
}

pub trait ExpressionVisitable<T> {
    fn accept(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T;
}

struct PrinterVisitor;

impl ExpressionVisitor<String> for PrinterVisitor {
    fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> String {
        let mut printer = PrinterVisitor;
        let exprs = vec![&expr.left, &expr.right];
        printer.write("Binary Expression", exprs)
    }

    fn visit_group_expr(&mut self, expr: &GroupExpression) -> String {
        let mut printer = PrinterVisitor;
        let exprs = vec![&expr.expr];
        printer.write("Group Expression", exprs)
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> String {
      match &expr.value {
        LiteralValue::Float(value) => format!("{value}"),
        LiteralValue::Integer(value) => format!("{value}"),
        LiteralValue::String(value) => format!("\"{value}\""),
        LiteralValue::Char(value) => format!("'{value}'"),
        LiteralValue::Nil => String::from("nil"),
        LiteralValue::True => String::from("true"),
        LiteralValue::False => String::from("false")
      }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> String {
        let mut printer = PrinterVisitor;
        let exprs = vec![&expr.right];
        printer.write("Binary Expression", exprs)
    }
}

impl PrinterVisitor {
  pub fn write(&mut self, name: &str, exprs: Vec<&Box<Expression>>) -> String {
    let mut content = String::new();

    content += format!("[{name}] --> \n").as_str();

    for expr in exprs {
      let parsed = expr.deref().accept(self as &mut dyn ExpressionVisitor<String>);
      content += format!("\t [{parsed}]: \n").as_str();
    }

    content
  }
}
