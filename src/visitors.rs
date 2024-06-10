use crate::{expressions::*, statements::*};

pub trait ExpressionVisitor<T> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> T;
    fn visit_group_expr(&mut self, expr: &GroupExpression) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> T;
    fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> T;
    fn visit_var_expr(&mut self, expr: &VariableExpression) -> T;
    fn visit_assignment_expr(&mut self, expr: &AssignmentExpression) -> T;
    fn visit_logical_expr(&mut self, expr: &LogicalExpression) -> T;
    fn visit_call_expr(&mut self, expr: &CallExpression) -> T;
    fn visit_cast_expr(&mut self, expr: &CastExpression) -> T;
    fn visit_obj_expr(&mut self, expr: &ObjectExpression) -> T;
    fn visit_get_expr(&mut self, expr: &GetExpression) -> T;
}

pub trait ExpressionVisitable<T> {
    fn accept(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_expr_stmt(&mut self, stmt: &ExpressionStatement) -> T;
    fn visit_echo_stmt(&mut self, stmt: &EchoStatement) -> T;
    fn visit_var_stmt(&mut self, stmt: &VariableStatement) -> T;
    fn visit_block_stmt(&mut self, stmt: &BlockStatement) -> T;
    fn visit_if_stmt(&mut self, stmt: &IfStatement) -> T;
    fn visit_while_stmt(&mut self, stmt: &WhileStatement) -> T;
    fn visit_fun_stmt(&mut self, stmt: &FunctionStatement) -> T;
    fn visit_return_stmt(&mut self, stmt: &ReturnStatement) -> T;
    fn visit_struct_stmt(&mut self, stmt: &StructStatement) -> T;
}

pub trait StatementVisitable<T> {
    fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T;
}