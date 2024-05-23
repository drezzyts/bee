use crate::{
    token::Token,
    visitors::{ExpressionVisitable, ExpressionVisitor},
};

#[derive(Debug)]
pub enum Expression {
    Binary(BinaryExpression),
    Group(GroupExpression),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
}

#[allow(unreachable_patterns)]
impl<T> ExpressionVisitable<T> for Expression {
  fn accept(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T {
      match self {
        Expression::Binary(expr) => visitor.visit_binary_expr(expr),
        Expression::Group(expr) => visitor.visit_group_expr(expr),
        Expression::Literal(expr) => visitor.visit_literal_expr(expr),
        Expression::Unary(expr) => visitor.visit_unary_expr(expr),
        _ => unimplemented!()
      }
  }
}

#[derive(Debug)]
pub enum LiteralValue {
    Float(f64),
    Integer(i64),
    Char(char),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct GroupExpression {
    pub left: Token,
    pub expr: Box<Expression>,
    pub right: Token,
}

#[derive(Debug)]
pub struct LiteralExpression {
    pub token: Token,
    pub value: LiteralValue,
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operator: Token,
    pub right: Box<Expression>,
}

impl BinaryExpression {
    pub fn new(left: Box<Expression>, operator: Token, right: Box<Expression>) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

impl GroupExpression {
    pub fn new(left: Token, expr: Box<Expression>, right: Token) -> Self {
        Self { left, expr, right }
    }
}

impl LiteralExpression {
    pub fn new(value: LiteralValue, token: Token) -> Self {
        Self { value, token }
    }
}

impl UnaryExpression {
    pub fn new(operator: Token, right: Box<Expression>) -> Self {
        Self { operator, right }
    }
}
