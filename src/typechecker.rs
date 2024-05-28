use std::{cell::RefCell, rc::Rc};

use crate::{
    enviroment::TypeEnviroment,
    error::BeeError,
    expressions::{
        AssignmentExpression, BinaryExpression, Expression, LiteralExpression, LiteralValue,
    },
    statements::{BlockStatement, Statement, VariableStatement},
    token::TokenKind,
};

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
            _ => Err(format!("this type '{identifier}' is not defined")),
        }
    }
}

pub struct TypeChecker {
    source: String,
}

impl TypeChecker {
    pub fn new(source: String) -> Self {
        Self { source }
    }

    pub fn exec(&self, stmt: &Statement, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        match stmt {
            Statement::Expression(v) => self.infer(&v.expression, env),
            Statement::Variable(v) => self.var_stmt(&v, env),
            Statement::Block(v) => self.block_stmt(&v, env),
            _ => unimplemented!(),
        }
    }

    fn block_stmt(
        &self,
        stmt: &BlockStatement,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        let mut enviroment = TypeEnviroment::new();
        enviroment.enclosing = Some(Rc::new(RefCell::new(env.clone())));

        for statement in stmt.statements.clone() {
            self.exec(&statement, &mut enviroment)?;
        }

        Ok(Type::Untyped)
    }

    fn var_stmt(
        &self,
        stmt: &VariableStatement,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        if let Some(token_t) = &stmt.typing {
            let expected_typing = match Type::to_type(&token_t.lexeme) {
                Ok(t) => t,
                Err(message) => {
                    let error = BeeError::report(
                        &stmt.typing.clone().unwrap().position,
                        message.as_str(),
                        "type-checker",
                        self.source.clone(),
                    );

                    return Err(error);
                }
            };

            match &stmt.initializer {
                None => {
                    env.define(stmt.name.lexeme.clone(), stmt.constant, expected_typing);
                    return Ok(expected_typing);
                }
                Some(initializer) => {
                    let inferred_typing = self.infer(&initializer, env)?;
                    let typing = match self.expects(&inferred_typing, &expected_typing) {
                        Ok(v) => v,
                        Err(message) => {
                            let error = BeeError::report(
                                &Expression::position(initializer.clone()),
                                message.as_str(),
                                "type-checker",
                                self.source.clone(),
                            );

                            return Err(error);
                        }
                    };

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
                    let error = BeeError::report(
                        &stmt.name.position,
                        "an variable that hasn't initialized must have a type declaration.",
                        "type-checker",
                        self.source.clone(),
                    );

                    return Err(error);
                }
            }
        }
    }

    pub fn infer(&self, expr: &Expression, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        match expr {
            Expression::Binary(expr) => self.binary(&expr, env),
            Expression::Literal(expr) => Ok(self.literal(&expr)),
            Expression::Assignment(expr) => self.assignment(&expr, env),
            _ => unimplemented!(),
        }
    }

    fn assignment(
        &self,
        expr: &AssignmentExpression,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        let expected_typing = match env.lookup(expr.name.lexeme.clone()) {
            Ok(v) => v,
            Err(message) => {
                let pos = Expression::position(*expr.value.clone());
                
                

                let error = BeeError::report(
                    &pos,
                    message.as_str(),
                    "type-checker",
                    self.source.clone(),
                );

                return Err(error);
            }
        };

        let inferred_typing = self.infer(&expr.value, env)?;

        match self.expects(&inferred_typing, &expected_typing) {
          Ok(v) => Ok(v),
          Err(message) => {
            let pos = Expression::position(*expr.value.clone());
            
            let error = BeeError::report(
              &pos,
              message.as_str(),
              "type-checker",
              self.source.clone(),
            );

            return Err(error);
          }
        }
    }

    fn binary(&self, expr: &BinaryExpression, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        let left = self.infer(&expr.left, env)?;
        let right = self.infer(&expr.right, env)?;

        let operator = expr.operator.clone();
        let operator_allowed_types = self.get_operator_allowed_types(&operator.kind);

        if let Err(message) = self.validate_binary_operation_type(left, &operator.kind, &operator_allowed_types) {
          let error = BeeError::report(
            &Expression::position(*expr.left.clone()),
            message.as_str(),
            "type-checker",
            self.source.clone(),
          );

          return Err(error);
        }
        if let Err(message) = self.validate_binary_operation_type(right, &operator.kind, &operator_allowed_types) {
          let error = BeeError::report(
            &Expression::position(*expr.right.clone()),
            message.as_str(),
            "type-checker",
            self.source.clone(),
          );

          return Err(error);
        }

        match self.expects(&right, &left) {
          Ok(v) => Ok(v),
          Err(message) => {
            let error = BeeError::report(
              &Expression::position(*expr.right.clone()),
              message.as_str(),
              "type-checker",
              self.source.clone(),
            );

            return Err(error);
          }
        }
    }

    fn literal(&self, expr: &LiteralExpression) -> Type {
        match expr.value {
            LiteralValue::Float(_) => Type::Float,
            LiteralValue::Integer(_) => Type::Int,
            LiteralValue::Char(_) => Type::Char,
            LiteralValue::String(_) => Type::Str,
            _ => unimplemented!(),
        }
    }

    fn get_operator_allowed_types(&self, operator: &TokenKind) -> Vec<Type> {
        match operator {
            TokenKind::Plus => vec![Type::Str, Type::Int, Type::Float],
            TokenKind::Minus => vec![Type::Int, Type::Float],
            TokenKind::Star => vec![Type::Int, Type::Float],
            TokenKind::Slash => vec![Type::Int, Type::Float],
            _ => unimplemented!(),
        }
    }

    fn validate_binary_operation_type(
        &self,
        ctype: Type,
        operator: &TokenKind,
        types: &Vec<Type>,
    ) -> Result<(), String> {
        if !types.contains(&ctype) {
            return Err(format!(
                "the {:?} operation is not allowed for {} type.",
                operator,
                ctype.to_string()
            ));
        }

        Ok(())
    }

    fn expects(&self, current: &Type, expected: &Type) -> Result<Type, String> {
        if !expected.equals(current) {
            Err(format!(
                "expected '{}' type, but received '{}'.",
                expected.to_string(),
                current.to_string()
            ))
        } else {
            Ok(current.clone())
        }
    }
}
mod tests {
    use crate::{
        enviroment::TypeEnviroment,
        expressions::{BinaryExpression, Expression, LiteralExpression, LiteralValue},
        lexer::{self, Lexer},
        parser::Parser,
        statements::Statement,
        token::TokenKind,
        typechecker::{Type, TypeChecker},
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

        let tc = TypeChecker::new(source.clone());
        let mut lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer.read_tokens().unwrap(), source.clone());
        let program = parser.parse().unwrap();
        let mut env = TypeEnviroment::new();

        fn test_expression(
            tc: &TypeChecker,
            stmt: &Statement,
            expected_type: Type,
            env: &mut TypeEnviroment,
        ) {
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
}
