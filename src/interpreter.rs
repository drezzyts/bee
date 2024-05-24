use crate::{
    enviroment::{self, Enviroment}, expressions::{AssignmentExpression, Expression, LiteralExpression, LiteralValue, VariableExpression}, position::Position, statements::{EchoStatement, ExpressionStatement, Statement, VariableStatement}, token::TokenKind, visitors::{ExpressionVisitable, ExpressionVisitor, StatementVisitable, StatementVisitor}
};

pub struct Interpreter {
    pub enviroment: Enviroment
}

impl Interpreter {
    pub fn new() -> Self {
        Self { enviroment: Enviroment::new() }
    }
    pub fn interpret(&mut self, program: Vec<Statement>) -> () {
        for stmt in program {
            self.execute(stmt);
        }
    }

    fn evaluate(&mut self, expression: Expression) -> LiteralValue {
        expression.accept(self as &mut dyn ExpressionVisitor<LiteralValue>)
    }

    fn execute(&mut self, statement: Statement) -> () {
        statement.accept(self as &mut dyn StatementVisitor<()>)
    }

    fn is_truthy(&self, lit: LiteralValue) -> bool {
        match lit {
            LiteralValue::Nil => false,
            LiteralValue::True => true,
            LiteralValue::False => false,
            LiteralValue::Float(f) => {
                if f == 0.0 {
                    false
                } else {
                    true
                }
            }
            LiteralValue::Integer(i) => {
                if i == 0 {
                    false
                } else {
                    true
                }
            }
            _ => true,
        }
    }
}

impl ExpressionVisitor<LiteralValue> for Interpreter {
    fn visit_binary_expr(&mut self, expr: &crate::expressions::BinaryExpression) -> LiteralValue {
        let left = self.evaluate(*expr.left.clone());
        let right = self.evaluate(*expr.right.clone());

        left.operate(expr.operator.kind.clone(), &right)
    }

    fn visit_group_expr(&mut self, expr: &crate::expressions::GroupExpression) -> LiteralValue {
        self.evaluate(*expr.expr.clone())
    }
    fn visit_literal_expr(&mut self, expr: &crate::expressions::LiteralExpression) -> LiteralValue {
        expr.value.clone()
    }
    fn visit_unary_expr(&mut self, expr: &crate::expressions::UnaryExpression) -> LiteralValue {
        let right = self.evaluate(*expr.right.clone());

        match expr.operator.kind {
            TokenKind::Minus => match right {
                LiteralValue::Integer(int) => LiteralValue::Integer(-int),
                LiteralValue::Float(float) => LiteralValue::Float(-float),
                _ => LiteralValue::NaN,
            },
            TokenKind::Bang => {
                if self.is_truthy(right) {
                    LiteralValue::False
                } else {
                    LiteralValue::True
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_var_expr(&mut self, expr: &VariableExpression) -> LiteralValue {
        self.enviroment.get(expr.name.lexeme.clone())
    }
    
    fn visit_assignment_expr(&mut self, expr: &AssignmentExpression) -> LiteralValue {
        let value = self.evaluate(*expr.value.clone());
        self.enviroment.assign(expr.name.lexeme.clone(), value.clone());
        value
    }
}

impl StatementVisitor<()> for Interpreter {
    fn visit_expr_stmt(&mut self, stmt: &ExpressionStatement) -> () {
        self.evaluate(*stmt.expression.clone());
    }

    fn visit_echo_stmt(&mut self, stmt: &EchoStatement) -> () {
      let value = self.evaluate(*stmt.expression.clone());
      
      let response = match &value {
        LiteralValue::Float(value) => format!("{:?}", value),
        LiteralValue::Integer(value) => format!("{}", value),
        LiteralValue::String(value) => format!("{}", value),
        LiteralValue::Char(value) => format!("{}", value),
        LiteralValue::Nil => String::from("nil"),
        LiteralValue::True => String::from("true"),
        LiteralValue::False => String::from("false"),
        LiteralValue::NaN => String::from("nan"),
      };

      println!("{response}");
    }
    
    fn visit_var_stmt(&mut self, stmt: &VariableStatement) -> () {
        let value = if let Some(initializer) = stmt.initializer.clone() {
            self.evaluate(initializer)
        } else {
            LiteralValue::Nil
        };

        self.enviroment.define(stmt.name.lexeme.clone(), value);
    }
}
