use crate::{
    enviroment::Enviroment, error::BeeError, expressions::*, statements::*, token::TokenKind, visitors::{ExpressionVisitable, ExpressionVisitor, StatementVisitable, StatementVisitor}
};

pub struct Interpreter {
    pub enviroment: Enviroment,
    pub source: String
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            enviroment: Enviroment::new(),
            source: "".to_string()
        }
    }
    pub fn interpret(&mut self, program: Vec<Statement>, source: String) -> Result<(), BeeError> {
        self.source = source.clone();

        for stmt in program {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn evaluate(&mut self, expression: Expression) -> Result<LiteralValue, BeeError> {
        expression.accept(self as &mut dyn ExpressionVisitor<Result<LiteralValue, BeeError>>)
    }

    fn execute(&mut self, statement: Statement) -> Result<(), BeeError> {
        statement.accept(self as &mut dyn StatementVisitor<Result<(), BeeError>>)
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

impl ExpressionVisitor<Result<LiteralValue, BeeError>> for Interpreter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpression) -> Result<LiteralValue, BeeError> {
        let left = self.evaluate(*expr.left.clone())?;
        let right = self.evaluate(*expr.right.clone())?;

        Ok(left.operate(expr.operator.kind.clone(), &right))
    }

    fn visit_group_expr(&mut self, expr: &GroupExpression) -> Result<LiteralValue, BeeError> {
        self.evaluate(*expr.expr.clone())
    }
    fn visit_literal_expr(&mut self, expr: &LiteralExpression) -> Result<LiteralValue, BeeError> {
        Ok(expr.value.clone())
    }
    fn visit_unary_expr(&mut self, expr: &UnaryExpression) -> Result<LiteralValue, BeeError> {
        let right = self.evaluate(*expr.right.clone())?;

        let value = match expr.operator.kind {
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
        };

        Ok(value)
    }

    fn visit_var_expr(&mut self, expr: &VariableExpression) -> Result<LiteralValue, BeeError> {
        Ok(self.enviroment.get(expr.name.lexeme.clone()))
    }

    fn visit_assignment_expr(&mut self, expr: &AssignmentExpression) -> Result<LiteralValue, BeeError> {
        let value = self.evaluate(*expr.value.clone())?;
        if let Err(message) = self.enviroment.assign(expr.name.lexeme.clone(), value.clone()) {
            let error = BeeError::report(
                &Expression::position(Expression::Assignment(expr.clone())),
                &message,
                "interpreter",
                self.source.clone()
            );

            return Err(error);
        };

        Ok(value)
    }
}

impl StatementVisitor<Result<(), BeeError>> for Interpreter {
    fn visit_expr_stmt(&mut self, stmt: &ExpressionStatement) -> Result<(), BeeError> {
        self.evaluate(*stmt.expression.clone())?;
        Ok(())
    }

    fn visit_echo_stmt(&mut self, stmt: &EchoStatement) -> Result<(), BeeError> {
        let value = self.evaluate(*stmt.expression.clone())?;

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
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &VariableStatement) -> Result<(), BeeError> {
        let value = if let Some(initializer) = stmt.initializer.clone() {
            self.evaluate(initializer)?
        } else {
            LiteralValue::Nil
        };

        if let Err(message) = self.enviroment.define(stmt.name.lexeme.clone(), stmt.constant, value) {
            let error = BeeError::report(
                &Statement::Variable(stmt.clone()).position(),
                &message,
                "interpreter",
                self.source.clone()
            );

            return Err(error);
        };

        Ok(())
    }
}
