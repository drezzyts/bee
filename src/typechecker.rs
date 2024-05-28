use crate::{enviroment::TypeEnviroment, expressions::{AssignmentExpression, BinaryExpression, Expression, LiteralExpression, LiteralValue}, statements::{Statement, VariableStatement}, token::TokenKind};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Type {
  Str,
  Int,
  Float,
  Char,
  Untyped,
}

impl Type {
  pub fn to_string(&self) -> String {
    let name = match self {
        Type::Str => "str",
        Type::Int => "int",
        Type::Float => "float",
        Type::Char => "char",
        Type::Untyped => "untyped",
    };

    name.to_string()
  }

  pub fn equals(&self, other: &Type) -> bool {
    self.to_string() == other.to_string()
  }

  pub fn to_type(identifier: &String) -> Result<Type, String> {
    match identifier.as_str() {
      "str" => Ok(Type::Str),
      "int" => Ok(Type::Int),
      "float" => Ok(Type::Float),
      "char" => Ok(Type::Char),
      "untyped" => Ok(Type::Untyped),
      _ => Err(format!("this type {identifier} is not defined"))
    }
  }
}

pub struct TypeChecker;

impl TypeChecker {
  pub fn new() -> Self {
    Self {}
  }
  
  pub fn exec(&self, stmt: &Statement, env: &mut TypeEnviroment) -> Result<Type, String> {
    match stmt {
      Statement::Expression(v) => self.infer(&v.expression, env),
      Statement::Variable(v) => self.var_stmt(&v, env),
      _ => unimplemented!()
    }
  }

  fn var_stmt(&self, stmt: &VariableStatement, env: &mut TypeEnviroment) -> Result<Type, String> {
    if let Some(token_t) = &stmt.typing {
      let expected_typing = Type::to_type(&token_t.lexeme)?;
      
      match &stmt.initializer {
        None => {
          env.define(stmt.name.lexeme.clone(), stmt.constant, expected_typing);
          return Ok(expected_typing);
        }
        Some(initializer) => {
          let inferred_typing = self.infer(&initializer, env)?;
          let typing = self.expects(&inferred_typing, &expected_typing)?;
          
          env.define(stmt.name.lexeme.clone(), stmt.constant, expected_typing);
          return Ok(typing);
        }
      }
    } else {
      match &stmt.initializer {
        Some(initializer) => {
          let inferred_typing = self.infer(&initializer, env)?;
          env.define(stmt.name.lexeme.clone(), stmt.constant, inferred_typing);
          return Ok(inferred_typing);
        }
        None => {
          Err(format!("an variable that hasn't initialized must have a type declaration."))
        }
      }
    } 
  }

  pub fn infer(&self, expr: &Expression, env: &mut TypeEnviroment) -> Result<Type, String> {
    match expr {
        Expression::Binary(expr) => self.binary(&expr, env),
        Expression::Literal(expr) => self.literal(&expr),
        Expression::Assignment(expr) => self.assignment(&expr, env),
        _ => Ok(Type::Untyped), // Err("founded invalid expression".to_string())
    }
  }

  fn assignment(&self, expr: &AssignmentExpression, env: &mut TypeEnviroment) -> Result<Type, String> {
    let expected_typing = env.lookup(expr.name.lexeme.clone())?;
    let inferred_typing = self.infer(&expr.value, env)?;

    self.expects(&inferred_typing, &expected_typing)
  }

  fn binary(&self, expr: &BinaryExpression, env: &mut TypeEnviroment) -> Result<Type, String> {
    let left = self.infer(&expr.left, env)?;
    let right = self.infer(&expr.right, env)?;

    let operator = expr.operator.clone();
    let operator_allowed_types = self.get_operator_allowed_types(&operator.kind)?;

    self.validate_binary_operation_type(left, &operator.kind, &operator_allowed_types)?;
    self.validate_binary_operation_type(right, &operator.kind, &operator_allowed_types)?;

    self.expects(&right, &left)
  }

  fn literal(&self, expr: &LiteralExpression) -> Result<Type, String> {
    match expr.value {
        LiteralValue::Float(_) => Ok(Type::Float),
        LiteralValue::Integer(_) => Ok(Type::Int),
        LiteralValue::Char(_) => Ok(Type::Char),
        LiteralValue::String(_) => Ok(Type::Str),
        _ => Err("founded invalid type".to_string())
    }
  }

  fn get_operator_allowed_types(&self, operator: &TokenKind) -> Result<Vec<Type>, String> {
    match operator {
      TokenKind::Plus => Ok(vec![Type::Str, Type::Int, Type::Float]),
      TokenKind::Minus => Ok(vec![Type::Int, Type::Float]),
      TokenKind::Star => Ok(vec![Type::Int, Type::Float]),
      TokenKind::Slash => Ok(vec![Type::Int, Type::Float]),
      _ => Err("invalid operator has provided".to_string())
    }
  }

  fn validate_binary_operation_type(&self, ctype: Type, operator: &TokenKind, types: &Vec<Type>) -> Result<(), String> {
    if !types.contains(&ctype) {
      return Err(format!("the {:?} operation is not allowed for {} type.", operator, ctype.to_string()))
    }

    Ok(())
  }

  fn expects(&self, current: &Type, expected: &Type) -> Result<Type, String> {
    if !expected.equals(current) {
      Err(format!("expected '{}' type, but received '{}'.", expected.to_string(), current.to_string()))
    } else {
      Ok(current.clone())
    }
  }
}
mod tests {
  use crate::{
      enviroment::TypeEnviroment, expressions::{BinaryExpression, Expression, LiteralExpression, LiteralValue}, lexer::{self, Lexer}, parser::Parser, statements::Statement, token::TokenKind, typechecker::{Type, TypeChecker}
  };

  #[test]
  fn test_binary_expr() {
      let source = r#"
          15 + 20;
          15.2 + 20.3;
          "Hello, " + "World";
          10 - 5;
          10.5 - 5.5;
          10 * 2;
          10.5 * 2.5;
          20 / 5;
          20.5 / 4.5;
      "#
      .to_string();

      let tc = TypeChecker::new();
      let mut lexer = Lexer::new(&source);
      let mut parser = Parser::new(lexer.read_tokens().unwrap(), source.clone());
      let program = parser.parse().unwrap();
      let mut env = TypeEnviroment::new();

      fn test_expression(tc: &TypeChecker, stmt: &Statement, expected_type: Type, env: &mut TypeEnviroment) {
          if let Statement::Expression(stmt) = stmt {
              let binary = stmt.expression.clone();
              assert_eq!(tc.infer(&binary, env).unwrap(), expected_type);
          }
      }

      test_expression(&tc, &program.get(0).unwrap(), Type::Int, &mut env);
      test_expression(&tc, &program.get(1).unwrap(), Type::Float, &mut env);
      test_expression(&tc, &program.get(2).unwrap(), Type::Str, &mut env);
      test_expression(&tc, &program.get(3).unwrap(), Type::Int, &mut env);
      test_expression(&tc, &program.get(4).unwrap(), Type::Float, &mut env);
      test_expression(&tc, &program.get(5).unwrap(), Type::Int, &mut env);
      test_expression(&tc, &program.get(6).unwrap(), Type::Float, &mut env);
      test_expression(&tc, &program.get(7).unwrap(), Type::Int, &mut env);
      test_expression(&tc, &program.get(8).unwrap(), Type::Float, &mut env);
  }

  #[test]
  fn test_invalid_binary_expr() {
      let source = r#"
          15 + 'c';
          15.2 - "Hello";
          'c' * 20;
          "Hello" / 3;
      "#
      .to_string();

      let tc = TypeChecker::new();
      let mut lexer = Lexer::new(&source);
      let mut parser = Parser::new(lexer.read_tokens().unwrap(), source.clone());
      let program = parser.parse().unwrap();
      let mut env = TypeEnviroment::new();

      fn test_invalid_expression(tc: &TypeChecker, stmt: &Statement, expected_err: &str, env: &mut TypeEnviroment) {
          if let Statement::Expression(stmt) = stmt {
              let binary = stmt.expression.clone();
              assert_eq!(tc.infer(&binary, env).unwrap_err(), expected_err);
          }
      }

      test_invalid_expression(
          &tc,
          &program.get(0).unwrap(),
          "the Plus operation is not allowed for char type.",
          &mut env
      );
      test_invalid_expression(
          &tc,
          &program.get(1).unwrap(),
          "the Minus operation is not allowed for str type.",
          &mut env
      );
      test_invalid_expression(
          &tc,
          &program.get(2).unwrap(),
          "the Star operation is not allowed for char type.",
          &mut env
      );
      test_invalid_expression(
          &tc,
          &program.get(3).unwrap(),
          "the Slash operation is not allowed for str type.",
          &mut env
      );
  }
}
