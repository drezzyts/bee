use crate::{
    token::Token,
    visitors::{ExpressionVisitable, ExpressionVisitor},
};

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

pub enum LiteralValue {
    Float(f64),
    Integer(i64),
    Char(char),
    String(String),
    True,
    False,
    Nil,
}

pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

pub struct GroupExpression {
    pub left: Token,
    pub expr: Box<Expression>,
    pub right: Token,
}

pub struct LiteralExpression {
    pub token: Token,
    pub value: LiteralValue,
}

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
