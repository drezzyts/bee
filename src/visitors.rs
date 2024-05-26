use crate::{enviroment::Enviroment, expressions::*, statements::*};

pub trait ExpressionVisitor<T> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> T;
    fn visit_group_expr(&mut self, expr: &GroupExpression) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> T;
    fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> T;
    fn visit_var_expr(&mut self, expr: &VariableExpression) -> T;
    fn visit_assignment_expr(&mut self, expr: &AssignmentExpression) -> T;
}

pub trait ExpressionVisitable<T> {
    fn accept(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_expr_stmt(&mut self, stmt: &ExpressionStatement) -> T;
    fn visit_echo_stmt(&mut self, stmt: &EchoStatement) -> T;
    fn visit_var_stmt(&mut self, stmt: &VariableStatement) -> T;
    fn visit_block_stmt(&mut self, stmt: &BlockStatement) -> T;
}

pub trait StatementVisitable<T> {
    fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T;
}