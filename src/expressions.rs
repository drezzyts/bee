use crate::{
    position::{self, Position}, token::{Token, TokenKind}, visitors::{ExpressionVisitable, ExpressionVisitor}
};

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(BinaryExpression),
    Group(GroupExpression),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Variable(VariableExpression),
    Assignment(AssignmentExpression)
}

impl Expression {
    pub fn position(expr: Expression) -> Position {
        match expr {
            Expression::Binary(expr) => {
                Position { line: Expression::position(*expr.left.clone()).line, cstart: Expression::position(*expr.left).cstart, cend: Expression::position(*expr.right).cend }
            }
            Expression::Group(expr) => {
                Position { line: expr.left.position.line, cstart: expr.left.position.cstart, cend: expr.right.position.cend }
            }
            Expression::Literal(expr) => {
                expr.token.position.clone()
            }
            Expression::Unary(expr) => {
                Position { line: expr.operator.position.line, cstart: expr.operator.position.cstart, cend: Expression::position(*expr.right).cend }
            }
            Expression::Variable(expr) => {
                expr.name.position.clone()
            }
            Expression::Assignment(expr) => {
                Position { line: expr.name.position.line, cstart: expr.name.position.cstart, cend: Expression::position(*expr.value).cend }
            }
        }
    }


    pub fn eq_kind(&self, expr: Expression) -> bool {
        match (self, expr) {
            (Expression::Binary(_), Expression::Binary(_)) => true,
            (Expression::Group(_), Expression::Group(_)) => true,
            (Expression::Literal(_), Expression::Literal(_)) => true,
            (Expression::Unary(_), Expression::Unary(_)) => true,
            (Expression::Variable(_), Expression::Variable(_)) => true,
            (Expression::Assignment(_), Expression::Assignment(_)) => true,
            _ => false
        }
    }

    pub fn kind(&self) -> &str {
        match self {
            Expression::Binary(_) => "Binary",
            Expression::Group(_) => "Group",
            Expression::Literal(_) => "Literal",
            Expression::Unary(_) => "Unary",
            Expression::Variable(_) => "Variable",
            Expression::Assignment(_) => "Assignment",
        }
    }
}
impl<T> ExpressionVisitable<T> for Expression {
    fn accept(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T {
        match self {
            Expression::Binary(expr) => visitor.visit_binary_expr(expr),
            Expression::Group(expr) => visitor.visit_group_expr(expr),
            Expression::Literal(expr) => visitor.visit_literal_expr(expr),
            Expression::Unary(expr) => visitor.visit_unary_expr(expr),
            Expression::Variable(expr) => visitor.visit_var_expr(expr),
            Expression::Assignment(expr) => visitor.visit_assignment_expr(expr)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Float(f64),
    Integer(i64),
    Char(char),
    String(String),
    True,
    False,
    Nil,
    NaN,
}

impl LiteralValue {
    pub fn operate(&self, operator: TokenKind, other: &LiteralValue) -> LiteralValue {
        match operator {
            TokenKind::Plus => self.sum(other),
            TokenKind::Minus => self.sub(other),
            TokenKind::Star => self.multi(other),
            TokenKind::Slash => self.divide(other),
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::EqualEqual
            | TokenKind::BangEqual => self.comparison(operator, other),
            _ => LiteralValue::NaN,
        }
    }

    pub fn sum(&self, other: &LiteralValue) -> LiteralValue {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => 
                LiteralValue::Integer(i1 + i2),
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) =>
                LiteralValue::Integer(*i1 + *f1 as i64),
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) =>
                LiteralValue::Float(*f1 + *i1 as f64),
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => 
                LiteralValue::Float(f1 + f2),
            (LiteralValue::String(s1), LiteralValue::String(s2)) =>
                LiteralValue::String(format!("{}{}", s1, s2)),
            (LiteralValue::String(s1), LiteralValue::Char(c1)) =>
                LiteralValue::String(format!("{}{}", s1, c1)),
            (LiteralValue::Char(c1), LiteralValue::String(s1)) =>
                LiteralValue::String(format!("{}{}", c1, s1)),
            (LiteralValue::Char(c1), LiteralValue::Char(c2)) =>
                LiteralValue::String(format!("{}{}", c1, c2)),
            _ => LiteralValue::NaN,
        }
    }

    pub fn sub(&self, other: &LiteralValue) -> LiteralValue {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) =>
                LiteralValue::Integer(i1 - i2),
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) =>
                LiteralValue::Float(f1 - f2),
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) =>
                LiteralValue::Integer(*i1 - *f1 as i64),
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) =>
                LiteralValue::Float(*f1 - *i1 as f64),
            _ => LiteralValue::NaN,
        }
    }

    pub fn multi(&self, other: &LiteralValue) -> LiteralValue {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) =>
                LiteralValue::Integer(i1 * i2),
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => 
                LiteralValue::Float(f1 * f2),
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) =>
                LiteralValue::Integer(*i1 * *f1 as i64),
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) =>
                LiteralValue::Float(*f1 * *i1 as f64),
            _ => LiteralValue::NaN,
        }
    }

    pub fn divide(&self, other: &LiteralValue) -> LiteralValue {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => 
                LiteralValue::Integer(i1 / i2),
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => 
                LiteralValue::Float(f1 / f2),
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) =>
                LiteralValue::Integer(*i1 / *f1 as i64),
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) =>
                LiteralValue::Float(*f1 / *i1 as f64),
            _ => LiteralValue::NaN,
        }
    }

    pub fn comparison(&self, operator: TokenKind, other: &LiteralValue) -> LiteralValue {
        let result = match operator {
            TokenKind::Greater => self.greater_than(other),
            TokenKind::GreaterEqual => self.greater_than_or_equal(other),
            TokenKind::Less => self.less_than(other),
            TokenKind::LessEqual => self.less_than_or_equal(other),
            TokenKind::EqualEqual => self.eq(other),
            TokenKind::BangEqual => !self.eq(other),
            _ => return LiteralValue::NaN,
        };

        self.to_bool(result)
    }

    pub fn greater_than(&self, other: &LiteralValue) -> bool {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => i1 > i2,
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => f1 > f2,
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) => *i1 as f64 > *f1,
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) => *f1 > *i1 as f64,
            _ => false,
        }
    }

    pub fn greater_than_or_equal(&self, other: &LiteralValue) -> bool {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => i1 >= i2,
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => f1 >= f2,
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) => *i1 as f64 >= *f1,
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) => *f1 >= *i1 as f64,
            _ => false,
        }
    }

    pub fn less_than(&self, other: &LiteralValue) -> bool {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => i1 < i2,
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => f1 < f2,
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) => (*i1 as f64) < *f1,
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) => *f1 < *i1 as f64,
            _ => false,
        }
    }

    pub fn less_than_or_equal(&self, other: &LiteralValue) -> bool {
        match (self, other) {
            (LiteralValue::Integer(i1), LiteralValue::Integer(i2)) => i1 <= i2,
            (LiteralValue::Float(f1), LiteralValue::Float(f2)) => f1 <= f2,
            (LiteralValue::Integer(i1), LiteralValue::Float(f1)) => *i1 as f64 <= *f1,
            (LiteralValue::Float(f1), LiteralValue::Integer(i1)) => *f1 <= *i1 as f64,
            _ => false,
        }
    }

    pub fn to_bool(&self, cond: bool) -> LiteralValue {
        if cond {
            LiteralValue::True
        } else {
            LiteralValue::False
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
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

#[derive(Debug, Clone)]
pub struct GroupExpression {
    pub left: Token,
    pub expr: Box<Expression>,
    pub right: Token,
}

impl GroupExpression {
    pub fn new(left: Token, expr: Box<Expression>, right: Token) -> Self {
        Self { left, expr, right }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralExpression {
    pub token: Token,
    pub value: LiteralValue,
}

impl LiteralExpression {
    pub fn new(value: LiteralValue, token: Token) -> Self {
        Self { value, token }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: Token,
    pub right: Box<Expression>,
}

impl UnaryExpression {
    pub fn new(operator: Token, right: Box<Expression>) -> Self {
        Self { operator, right }
    }
}

#[derive(Debug, Clone)]
pub struct VariableExpression {
    pub name: Token,
}

impl VariableExpression {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}


#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    pub name: Token,
    pub value: Box<Expression>
}

impl AssignmentExpression {
    pub fn new(name: Token, value: Box<Expression>) -> Self {
        Self { name, value }
    }
}