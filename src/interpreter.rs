use crate::{expressions::{Expression, LiteralValue}, statements::{ExpressionStatement, PutsStatement, Statement}, token::TokenKind, visitors::{ExpressionVisitable, ExpressionVisitor, StatementVisitable, StatementVisitor}};

pub struct Interpreter;

impl Interpreter {
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
      },
      LiteralValue::Integer(i) => {
        if i == 0 {
          false
        } else {
          true
        }
      },
      _ => true
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
      TokenKind::Minus => {
        match right {
          LiteralValue::Integer(int) => LiteralValue::Integer(-int),
          LiteralValue::Float(float) => LiteralValue::Float(-float),
          _ => LiteralValue::NaN  
        }
      },
      TokenKind::Bang => {
        if self.is_truthy(right) {
          LiteralValue::False
        } else {
          LiteralValue::True
        }
      },
      _ => unreachable!()
    }
  }
}

impl StatementVisitor<()> for Interpreter {
  fn visit_expr_stmt(&mut self, stmt: &ExpressionStatement) -> () {
      self.evaluate(*stmt.expression.clone());
  }

  fn visit_puts_stmt(&mut self, stmt: &PutsStatement) -> () {
      let value = self.evaluate(*stmt.expression.clone());
      println!("{:?}", value);
  }
} 