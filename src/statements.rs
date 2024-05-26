use crate::{
    expressions::{self, Expression}, position::Position, token::Token, visitors::{StatementVisitable, StatementVisitor}
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    Echo(EchoStatement),
    Variable(VariableStatement),
    Block(BlockStatement)
}

impl Statement {
    pub fn position(&self) -> Position {
        match self {
            Statement::Expression(stmt) => {
                Expression::position(*stmt.expression.clone()).clone()
            }
            Statement::Echo(stmt) => {
                Expression::position(*stmt.expression.clone()).clone()
            }
            Statement::Variable(stmt) => {
                stmt.name.position.clone()
            }
            Statement::Block(_) => unreachable!()
        }
    }
}

#[allow(unreachable_patterns)]
impl<T> StatementVisitable<T> for Statement {
    fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
        match self {
            Statement::Expression(stmt) => visitor.visit_expr_stmt(stmt),
            Statement::Echo(stmt) => visitor.visit_echo_stmt(stmt),
            Statement::Variable(stmt) => visitor.visit_var_stmt(stmt),
            Statement::Block(stmt) => visitor.visit_block_stmt(stmt),
            _ => unimplemented!(),
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
    pub expression: Box<Expression>,
}

impl EchoStatement {
    pub fn new(expression: Box<Expression>) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct VariableStatement {
    pub name: Token,
    pub initializer: Option<Expression>,
    pub constant: bool
}

impl VariableStatement {
    pub fn new(name: Token, initializer: Option<Expression>, constant: bool) -> Self {
        Self { name, initializer, constant }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub left: Token,
    pub statements: Vec<Box<Statement>>,
    pub right: Token
}

impl BlockStatement {
    pub fn new(left: Token, statements: Vec<Box<Statement>>, right: Token) -> Self {
        Self { left, statements, right }
    }
}
