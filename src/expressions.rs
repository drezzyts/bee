use core::fmt;
use std::{fmt::Debug, rc::Rc};

use crate::{
    error::BeeError, position::Position, token::{Token, TokenKind}, visitors::{ExpressionVisitable, ExpressionVisitor}
};

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(BinaryExpression),
    Group(GroupExpression),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Variable(VariableExpression),
    Assignment(AssignmentExpression),
    Logical(LogicalExpression),
    Call(CallExpression),
    Cast(CastExpression),
    Object(ObjectExpression),
    Get(GetExpression)
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
                match expr.value.clone() {
                    LiteralValue::String(_) => {
                        let mut pos = expr.token.position.clone();
                        pos.cstart -= 3;
                        pos
                    },
                    _ => {
                        let mut pos = expr.token.position.clone();
                        pos.cstart -= 2;
                        pos
                    }
                }
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
            Expression::Logical(expr) => {
                let l = Expression::position(*expr.left.clone());
                let r = Expression::position(*expr.right.clone());

                Position { line: l.line, cstart: l.cstart, cend: r.cend }
            }
            Expression::Call(expr) => {
                Expression::position(*expr.callee.clone())
            }
            Expression::Cast(expr) => {
                expr.typing.position.clone()
            },
            Expression::Object(expr) => expr.open.position.clone(),
            Expression::Get(expr) => Expression::position(*expr.left),
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
            (Expression::Logical(_), Expression::Logical(_)) => true,
            (Expression::Call(_), Expression::Call(_)) => true,
            (Expression::Cast(_), Expression::Cast(_)) => true,
            (Expression::Object(_), Expression::Object(_)) => true,
            (Expression::Get(_), Expression::Get(_)) => true,
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
            Expression::Logical(_) => "Logical",
            Expression::Call(_) => "Call",
            Expression::Cast(_) => "Cast",
            Expression::Object(_) => "Object",
            Expression::Get(_) => "Get",
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
            Expression::Assignment(expr) => visitor.visit_assignment_expr(expr),
            Expression::Logical(expr) => visitor.visit_logical_expr(expr),
            Expression::Call(expr) => visitor.visit_call_expr(expr),
            Expression::Cast(expr) => visitor.visit_cast_expr(expr),
            Expression::Object(expr) => visitor.visit_obj_expr(expr),
            Expression::Get(expr) => visitor.visit_get_expr(expr)
        }
    }
}

#[derive(Clone)]
pub enum LiteralValue {
    Float(f64),
    Integer(i64),
    Char(char),
    String(String),
    True,
    False,
    Nil,
    NaN,
    Callable { 
        arity: usize, 
        name: String, 
        fun: Rc<dyn Fn(Vec<LiteralValue>) -> LiteralValue> 
    },
    Struct {
        name: String,
        properties: Vec<String>
    },
    Object(Vec<LiteralValue>),
    Instance {
        typing: String,
        properties: Vec<(String, LiteralValue)>
    }
}

impl Debug for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Char(arg0) => f.debug_tuple("Char").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::Nil => write!(f, "Nil"),
            Self::NaN => write!(f, "NaN"),
            Self::Callable { arity, name, fun: _ } => write!(f, "{name}:{arity}"),
            Self::Struct { name, properties: _ } => write!(f, "{name}"),
            Self::Object(values) => {
                let val: Vec<String> = values.iter().map(|p| -> String {
                    p.to_string()
                }).collect();

                let content = String::from("{") + val.join(", ").as_str() + "}";

                write!(f, "{}", content)
            }
            Self::Instance { typing, properties } => {
                let val: Vec<String> = properties.iter().map(|(name, value)| -> String {
                    format!("{name}: {}", value.to_string())
                }).collect();

                let content = String::from("{") + val.join(", ").as_str() + "}";

                write!(f, "{}::{}", typing, content)
            }
        }
    }
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Float(arg0) => write!(f, "{}", arg0),
            Self::Integer(arg0) => write!(f, "{}", arg0),
            Self::Char(arg0) => write!(f, "{}", arg0),
            Self::String(arg0) => write!(f, "{}", arg0),
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::Nil => write!(f, "Nil"),
            Self::NaN => write!(f, "NaN"),
            Self::Callable { arity, name, fun: _ } => write!(f, "{name}:{arity}"),
            Self::Struct { name, properties: _ } => write!(f, "{name}"),
            Self::Object(values) => {
                let val: Vec<String> = values.iter().map(|p| -> String {
                    p.to_string()
                }).collect();

                let content = String::from("{") + val.join(", ").as_str() + "}";

                write!(f, "{}", content)
            }
            Self::Instance { typing, properties } => {
                let val: Vec<String> = properties.iter().map(|(name, value)| -> String {
                    format!("{name}: {}", value.to_string())
                }).collect();

                let content = String::from("{") + val.join(", ").as_str() + "}";

                write!(f, "{}::{}", typing, content)
            }
        }
    }
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl LiteralValue {
    pub fn cast_str(&self) -> Result<LiteralValue, String> {
        match self {
            LiteralValue::Float(v) => Ok(LiteralValue::String(v.to_string())),
            LiteralValue::Integer(v) => Ok(LiteralValue::String(v.to_string())),
            LiteralValue::Char(v) => Ok(LiteralValue::String(v.to_string())),
            LiteralValue::String(_) => Ok(self.clone()),
            LiteralValue::True => Ok(LiteralValue::String("true".to_string())),
            LiteralValue::False => Ok(LiteralValue::String("false".to_string())),
            _ => Err(format!("cannot cast this type to 'str'"))
        }
    }

    pub fn cast_int(&self) -> Result<LiteralValue, String> {
        match self {
            LiteralValue::Integer(_) => Ok(self.clone()),
            LiteralValue::Float(v) => Ok(LiteralValue::Integer(v.clone() as i64)),
            LiteralValue::True => Ok(LiteralValue::Integer(1)),
            LiteralValue::False => Ok(LiteralValue::Integer(0)),
            _ => Err(format!("cannot cast this type to 'int'"))
        }
    }

    pub fn cast_float(&self) -> Result<LiteralValue, String> {
        match self {
            LiteralValue::Float(_) => Ok(self.clone()),
            LiteralValue::Integer(v) => Ok(LiteralValue::Float(v.clone() as f64)),
            LiteralValue::True => Ok(LiteralValue::Float(1.0)),
            LiteralValue::False => Ok(LiteralValue::Float(0.0)),
            _ => Err(format!("cannot cast this type to 'float'"))
        }
    }

    pub fn cast_bool(&self) -> Result<LiteralValue, String> {
        match self {
            LiteralValue::True | LiteralValue::False=> Ok(self.clone()),
            LiteralValue::Integer(v) => {
                if v.clone() == 0 {
                    Ok(LiteralValue::False)
                } else {
                    Ok(LiteralValue::True)
                }
            },
            _ => Err(format!("cannot cast this type to 'bool'"))
        }
    }

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
            (LiteralValue::Integer(n1), LiteralValue::String(s1)) => 
                LiteralValue::String(format!("{}{}", n1, s1)),
            (LiteralValue::Float(n1), LiteralValue::String(s1)) => 
                LiteralValue::String(format!("{}{}", n1, s1)),
            (LiteralValue::String(s1), LiteralValue::Integer(n1)) => 
                LiteralValue::String(format!("{}{}", s1, n1)),
            (LiteralValue::String(n1), LiteralValue::Float(s1)) => 
                LiteralValue::String(format!("{}{}", s1, n1)),
            (LiteralValue::Integer(n1), LiteralValue::Char(s1)) => 
                LiteralValue::String(format!("{}{}", n1, s1)),
            (LiteralValue::Float(n1), LiteralValue::Char(s1)) => 
                LiteralValue::String(format!("{}{}", n1, s1)),
            (LiteralValue::Char(s1), LiteralValue::Integer(n1)) => 
                LiteralValue::String(format!("{}{}", s1, n1)),
            (LiteralValue::Char(n1), LiteralValue::Float(s1)) => 
                LiteralValue::String(format!("{}{}", s1, n1)),
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

#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

impl LogicalExpression {
    pub fn new(
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>
    ) -> Self {
        Self { left, operator, right }
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub paren: Token,
    pub args: Vec<Box<Expression>>,
}

impl CallExpression {
    pub fn new(
        callee: Box<Expression>,
        paren: Token,
        args: Vec<Box<Expression>>
    ) -> Self {
        Self { callee, paren, args }
    }
}

#[derive(Debug, Clone)]
pub struct CastExpression {
    pub typing: Token,
    pub casted: Box<Expression>,
}

impl CastExpression {
    pub fn new(typing: Token, casted: Box<Expression>) -> Self {
        Self { typing, casted }
    }
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub open: Token,
    pub values: Vec<Expression>,
    pub close: Token,
}

impl ObjectExpression {
    pub fn new(open: Token, values: Vec<Expression>, close: Token) -> Self {
        Self { values, open, close }
    }
}

#[derive(Debug, Clone)]
pub struct GetExpression {
    pub right: Box<Expression>,
    pub left: Box<Expression>
}

impl GetExpression {
    pub fn new(right: Expression, left: Expression) -> Self {
        Self { right: Box::new(right), left: Box::new(left) }
    }
}