use crate::{
    expressions::Expression, token::Token, visitors::{StatementVisitable, StatementVisitor}
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    Echo(EchoStatement),
    Variable(VariableStatement)
}

#[allow(unreachable_patterns)]
impl<T> StatementVisitable<T> for Statement {
    fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
        match self {
            Statement::Expression(stmt) => visitor.visit_expr_stmt(stmt),
            Statement::Echo(stmt) => visitor.visit_echo_stmt(stmt),
            Statement::Variable(stmt) => visitor.visit_var_stmt(stmt),
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