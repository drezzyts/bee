use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    enviroment::TypeEnviroment,
    error::BeeError,
    expressions::{
        AssignmentExpression, BinaryExpression, CallExpression, CastExpression, Expression, LiteralExpression, LiteralValue, LogicalExpression, VariableExpression
    },
    statements::{BlockStatement, FunctionStatement, IfStatement, ReturnStatement, Statement, VariableStatement, WhileStatement},
    token::TokenKind,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionParamType {
    pub name: String, 
    pub typing: Type 
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionType {
    pub name: String,
    pub params: Vec<FunctionParamType>,
    pub typing: Box<Type>,
    pub env: TypeEnviroment
} 

impl FunctionType {
    pub fn new(params: Vec<FunctionParamType>, typing: Box<Type>, env: TypeEnviroment, name: String) -> Self {
        Self { params, typing, env, name }
    }

    pub fn to_string(&self) -> String {
        let mut content = String::new();
        content += "Fun";
        
        if self.params.len() > 0 {
            content += "<";
        }

        for param in self.params.clone() {
            if content.len() > 4 {
                content += ",";
            }

            content += param.typing.to_string().as_str();
        }

        
        if self.params.len() > 0 {
            content += ">";
        }

        content += " -> ";
        content += self.typing.to_string().as_str();

        content
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Str,
    Int,
    Float,
    Char,
    Untyped,
    Bool,
    Function(FunctionType)
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Str => "str".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Char => "char".to_string(),
            Type::Untyped => "untyped".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Function(f) => f.clone().to_string(),
        }
    }

    pub fn equals(&self, other: &Type) -> bool {
        self.to_string() == other.to_string()
    }
}

pub struct TypeChecker {
    source: String,
    types: HashMap<String, Type>
}

impl TypeChecker {
    pub fn to_type(&self, identifier: &String) -> Result<Type, String> {
        match self.types.get(identifier) {
            Some(t) => Ok(t.clone()),
            None => Err(format!("this type '{identifier}' is not defined")),
        }
    }

    pub fn new(source: String) -> Self {
        let mut types = HashMap::new();
        types.insert("str".to_string(), Type::Str);
        types.insert("char".to_string(), Type::Char);
        types.insert("int".to_string(), Type::Int);
        types.insert("float".to_string(), Type::Float);
        types.insert("bool".to_string(), Type::Bool);
        types.insert("untyped".to_string(), Type::Untyped);
        
        Self { source, types }
    }

    pub fn exec(&mut self, stmt: &Statement, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        match stmt {
            Statement::Expression(v) => self.infer(&v.expression, env),
            Statement::Variable(v) => self.var_stmt(v, env),
            Statement::Block(v) => self.block_stmt(v, env),
            Statement::Echo(v) => self.infer(&v.expression, env),
            Statement::If(v) => self.if_stmt(v, env),
            Statement::While(v) => self.while_stmt(v, env),
            Statement::Function(v) => self.function_stmt(v, env),
            _ => unimplemented!(),
        }
    }

    fn function_stmt(&mut self, stmt: &FunctionStatement, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        let mut fun_env = TypeEnviroment::new();
        fun_env.enclosing = Some(Rc::new(RefCell::new(env.clone())));

        let mut params: Vec<FunctionParamType> = vec![];

        for (name, typing) in stmt.params.clone() {
            let t = match self.to_type(&typing.lexeme) {
                Ok(r) => r,
                Err(message) => {
                    return Err(BeeError::report(
                        &typing.position,
                        message.as_str(),
                        "type-checker",
                        self.source.clone()
                    ))
                }
            };
            
            let param = FunctionParamType { 
                name: name.lexeme.clone(),
                typing: t
            };

            params.push(param.clone());
            fun_env.define(param.name, true, param.typing);
        };

        let block = match *stmt.body.clone() {
            Statement::Block(b) => b,
            _ => unreachable!()
        };

        let mut enviroment = TypeEnviroment::new();
        enviroment.enclosing = Some(Rc::new(RefCell::new(fun_env.clone())));
        
        let typing = match stmt.typing.clone() {
            Some(t) => self.to_type(&t.lexeme),
            None => self.to_type(&String::from("untyped"))
        };

        let func = match typing.clone() {
            Ok(t) => FunctionType::new(params, Box::new(t), fun_env, stmt.name.lexeme.clone()),
            Err(message) => {
                return Err(BeeError::report(
                    &stmt.typing.clone().unwrap().position,
                    message.as_str(),
                    "type-checker",
                    self.source.clone()
                ))
            }
        };

        for statement in block.statements {
            match *statement.clone() {
                Statement::Return(r) => {
                    let rtype = match r.value.clone() {
                        None => Type::Untyped,
                        Some(expr) => self.infer(&expr, &mut enviroment)?,
                    };

                    match self.expects(&rtype, &typing.clone().unwrap()) {
                        Ok(_) => continue,
                        Err(_) => {
                            let error = BeeError::report(
                                &r.keyword.position,
                                format!("this function expects a return type of '{}', but received '{}'.", typing.unwrap().to_string(), rtype.to_string()).as_str(),
                                "type-checker",
                                self.source.clone()
                            );

                            return Err(error);
                        }
                    }
                },
                _ => {
                    self.exec(&statement, &mut enviroment)?;
                }
            }
        }

        let t = Type::Function(func.clone());
        self.types.insert(func.name.clone(), t.clone());
        env.define(func.name, true, t);

        Ok(Type::Untyped)
    }

    fn while_stmt(&mut self, stmt: &WhileStatement, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        let condition = self.infer(&stmt.condition, env)?;
        
        if let Err(message) = self.expects(&condition, &Type::Bool) {
            let error = BeeError::report(
                &Expression::position(*stmt.condition.clone()),
                message.as_str(),
                "type-checker",
                self.source.clone()
            );

            return Err(error);
        }

        self.exec(&stmt.body, env)?;

        Ok(Type::Untyped)
    }

    fn if_stmt(&mut self, stmt: &IfStatement, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        let condition = self.infer(&stmt.condition, env)?;

        if let Err(message) = self.expects(&condition, &Type::Bool) {
            let error = BeeError::report(
                &Expression::position(*stmt.condition.clone()),
                message.as_str(),
                "type-checker",
                self.source.clone()
            );

            return Err(error);
        }

        self.exec(&stmt.consequent.clone(), env)?;

        if let Some(alternate) = stmt.alternate.clone() {
            return self.exec(&alternate, env);
        }

        Ok(Type::Untyped)
    }

    fn block_stmt(
        &mut self,
        stmt: &BlockStatement,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        let mut enviroment = TypeEnviroment::new();
        enviroment.enclosing = Some(Rc::new(RefCell::new(env.clone())));

        for statement in &stmt.statements {
            self.exec(statement, &mut enviroment)?;
        }

        Ok(Type::Untyped)
    }

    fn var_stmt(
        &self,
        stmt: &VariableStatement,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        if let Some(token_t) = &stmt.typing {
            let expected_typing = match self.to_type(&token_t.lexeme) {
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
                    env.define(stmt.name.lexeme.clone(), stmt.constant, expected_typing.clone());
                    return Ok(expected_typing);
                }
                Some(initializer) => {
                    let inferred_typing = self.infer(initializer, env)?;
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
                    let inferred_typing = self.infer(initializer, env)?;
                    env.define(stmt.name.lexeme.clone(), stmt.constant, inferred_typing.clone());
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
            Expression::Binary(expr) => self.binary(expr, env),
            Expression::Literal(expr) => Ok(self.literal(&expr)),
            Expression::Assignment(expr) => self.assignment(expr, env),
            Expression::Cast(expr) => self.cast(expr, env),
            Expression::Variable(expr) => self.variable(expr, env),
            Expression::Group(expr) => self.infer(&expr.expr, env),
            Expression::Logical(expr) => self.logical(expr, env),
            Expression::Call(expr) => match self.call(expr, env) {
                Ok(name) => Ok(self.to_type(&name).unwrap()),
                Err(err) => Err(err) 
            },
            _ => unimplemented!(),
        }
    }

    fn call(&self, expr: &CallExpression, env: &mut TypeEnviroment) -> Result<String, BeeError> {
        let callee = match *expr.callee.clone() {
            Expression::Variable(v) => v.name.lexeme.clone(),
            Expression::Call(c) => self.call(&c, env)?,
            _ => {
                let error = BeeError::report(
                    &Expression::position(*expr.callee.clone()),
                    "called using a callee that is invalid",
                    "type-checker",
                    self.source.clone(),
                );

                return Err(error);
            }
        };

        let typing = match env.lookup(callee.clone()) {
            Err(_) => {
                let error = BeeError::report(
                    &Expression::position(*expr.callee.clone()),
                    "called using a callee that is not defined",
                    "type-checker",
                    self.source.clone(),
                );

                return Err(error);
            }
            Ok(f) => f.clone()
        };

        let fun = match typing {
            Type::Function(func) => func,
            _ => {
                let error = BeeError::report(
                    &Expression::position(*expr.callee.clone()),
                    "called using a callee that is not a function",
                    "type-checker",
                    self.source.clone(),
                );

                return Err(error);
            }
        };

        if expr.args.len() > fun.params.len() {
            let invalid_param = expr.args.get(fun.params.len()).unwrap();
            let position = Expression::position(*invalid_param.clone());

            let error = BeeError::report(
                &position,
                format!("this argument is not expectede in {} function", callee.clone()).as_str(),
                "type-checker",
                self.source.clone(),
            );

            return Err(error);
        }

        let mut i = 0;
        for arg in expr.args.clone() {
            let value = self.infer(&arg, env)?;
            let expected = &fun.params.get(i).unwrap().typing;

            if let Err(message) = self.expects(&value, &expected) {
                let error = BeeError::report(
                    &Expression::position(*arg.clone()),
                    message.as_str(),
                    "type-checker",
                    self.source.clone(),
                );
    
                return Err(error);
            }

            i += 1;
        }

        Ok(fun.typing.to_string().clone())
    }

    fn logical(
        &self,
        expr: &LogicalExpression,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        let left = self.infer(&expr.left, env)?;
        let right = self.infer(&expr.right, env)?;

        match self.expects(&right, &left) {
            Ok(_) => Ok(Type::Bool),
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

    fn variable(
        &self,
        expr: &VariableExpression,
        env: &mut TypeEnviroment,
    ) -> Result<Type, BeeError> {
        match env.lookup(expr.name.lexeme.clone()) {
            Ok(typing) => Ok(typing),
            Err(message) => {
                let error = BeeError::report(
                    &expr.name.clone().position,
                    message.as_str(),
                    "type-checker",
                    self.source.clone(),
                );

                Err(error)
            }
        }
    }

    fn cast(&self, expr: &CastExpression, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        let typing = match self.to_type(&expr.typing.lexeme) {
            Ok(typing) => typing,
            Err(message) => {
                let error = BeeError::report(
                    &expr.typing.clone().position,
                    message.as_str(),
                    "type-checker",
                    self.source.clone(),
                );

                return Err(error);
            }
        };

        let value = self.infer(&expr.casted, env)?;
        let allowed_casts = self.get_type_cast_allowed_types(value.clone());

        if typing.equals(&value) {
            return Ok(value);
        }

        if !allowed_casts.contains(&typing) {
            let error = BeeError::report(
                &Expression::position(*expr.casted.clone()),
                format!(
                    "the cast operation of {} to {} is not allowed",
                    value.to_string(),
                    typing.to_string()
                )
                .as_str(),
                "type-checker",
                self.source.clone(),
            );

            return Err(error);
        }

        Ok(typing)
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

                let error =
                    BeeError::report(&pos, message.as_str(), "type-checker", self.source.clone());

                return Err(error);
            }
        };

        let inferred_typing = self.infer(&expr.value, env)?;

        match self.expects(&inferred_typing, &expected_typing) {
            Ok(v) => Ok(v),
            Err(message) => {
                let pos = Expression::position(*expr.value.clone());

                let error =
                    BeeError::report(&pos, message.as_str(), "type-checker", self.source.clone());

                return Err(error);
            }
        }
    }

    fn binary(&self, expr: &BinaryExpression, env: &mut TypeEnviroment) -> Result<Type, BeeError> {
        let left = self.infer(&expr.left, env)?;
        let right = self.infer(&expr.right, env)?;

        let operator = expr.operator.clone();
        let operator_allowed_types = self.get_operator_allowed_types(&operator.kind);

        if let Err(message) =
            self.validate_binary_operation_type(left.clone(), &operator.kind, &operator_allowed_types)
        {
            let error = BeeError::report(
                &Expression::position(*expr.left.clone()),
                message.as_str(),
                "type-checker",
                self.source.clone(),
            );

            return Err(error);
        }
        if let Err(message) =
            self.validate_binary_operation_type(right.clone(), &operator.kind, &operator_allowed_types)
        {
            let error = BeeError::report(
                &Expression::position(*expr.right.clone()),
                message.as_str(),
                "type-checker",
                self.source.clone(),
            );

            return Err(error);
        }

        match self.expects(&right, &left) {
            Ok(v) => {
                match operator.lexeme.as_str() {
                    ">" | ">=" | "<" | "<=" | "==" | "!=" => Ok(Type::Bool),
                    _ => Ok(v)
                }
            },
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
            LiteralValue::True | LiteralValue::False => Type::Bool,
            _ => Type::Untyped,
        }
    }

    fn get_operator_allowed_types(&self, operator: &TokenKind) -> Vec<Type> {
        match operator {
            TokenKind::Plus => vec![Type::Str, Type::Int, Type::Float],
            TokenKind::Minus => vec![Type::Int, Type::Float],
            TokenKind::Star => vec![Type::Int, Type::Float],
            TokenKind::Slash => vec![Type::Int, Type::Float],
            TokenKind::Greater | TokenKind::GreaterEqual | 
            TokenKind::Less | TokenKind::LessEqual => vec![Type::Int, Type::Float],
            TokenKind::EqualEqual | TokenKind::BangEqual => vec![Type::Int, Type::Float, Type::Char, Type::Bool, Type::Untyped, Type::Str], 
            _ => unimplemented!(),
        }
    }

    fn get_type_cast_allowed_types(&self, cast: Type) -> Vec<Type> {
        let allowed = match cast {
            Type::Str => vec![],
            Type::Int => vec![Type::Str, Type::Float, Type::Bool],
            Type::Float => vec![Type::Int, Type::Str],
            Type::Char => vec![Type::Str],
            Type::Bool => vec![Type::Int, Type::Float, Type::Str],
            _ => vec![],
        };

        allowed
    }

    fn validate_binary_operation_type(
        &self,
        ctype: Type,
        operator: &TokenKind,
        types: &Vec<Type>,
    ) -> Result<(), String> {
        if !types.contains(&ctype) {
            return Err(format!(
                "the {:?} operation is not allowed for '{}' type.",
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
        let mut program = parser.parse().unwrap();
        let mut env = TypeEnviroment::new();

        fn test_expression(
            tc: &TypeChecker,
            stmt: &Statement,
            expected_type: Type,
            env: &mut TypeEnviroment,
        ) {
            if let Statement::Expression(stmt) = stmt {
                let binary = &stmt.expression;
                assert_eq!(tc.infer(binary, env).unwrap(), expected_type);
            }
        }

        test_expression(&tc, &program.get_mut(0).unwrap(), Type::Int, &mut env);
        test_expression(&tc, &program.get_mut(1).unwrap(), Type::Float, &mut env);
        test_expression(&tc, &program.get_mut(2).unwrap(), Type::Str, &mut env);
        test_expression(&tc, &program.get_mut(3).unwrap(), Type::Int, &mut env);
        test_expression(&tc, &program.get_mut(4).unwrap(), Type::Float, &mut env);
        test_expression(&tc, &program.get_mut(5).unwrap(), Type::Int, &mut env);
        test_expression(&tc, &program.get_mut(6).unwrap(), Type::Float, &mut env);
        test_expression(&tc, &program.get_mut(7).unwrap(), Type::Int, &mut env);
        test_expression(&tc, &program.get_mut(8).unwrap(), Type::Float, &mut env);
    }
}
