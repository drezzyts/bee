use std::{cell::RefCell, rc::Rc};

use crate::{
    enviroment::{Enviroment, TypeEnviroment}, error::BeeError, expressions::*, token::TokenKind, typechecker::{Type, TypeChecker}, visitors::{ExpressionVisitable, ExpressionVisitor, StatementVisitable, StatementVisitor}
};

use crate::statements::*;

#[derive(Clone)]
pub struct Interpreter {
    pub globals: Rc<RefCell<Enviroment>>,
    pub enviroment: Rc<RefCell<Enviroment>>,
    pub source: String,
    pub type_env: TypeEnviroment
}

impl Interpreter {
    pub fn new(type_env: TypeEnviroment) -> Self {
        let print = LiteralValue::Callable {
            arity: 1,
            name: "print".to_string(),
            fun: Rc::new(|args: Vec<LiteralValue>| {
                println!("{}", args[0]);
                LiteralValue::Nil
            }),
        };

        let type_f = LiteralValue::Callable {
            arity: 1,
            name: "type".to_string(),
            fun: Rc::new(|args| {
                let value = args[0].clone();

                let typing = match value {
                    LiteralValue::String(_) => "str",
                    LiteralValue::Char(_) => "char",
                    LiteralValue::Integer(_) => "int",
                    LiteralValue::Float(_) => "float",
                    LiteralValue::True | LiteralValue::False => "bool",
                    LiteralValue::Callable { name: _, arity: _, fun: _ } => "function",
                    LiteralValue::Nil => "nil",
                    _ => "untyped" 
                };

                LiteralValue::String(typing.to_string())
            }),
        };

        let mut env = Enviroment::new();
        env.define("print".to_string(), true, print).unwrap();
        env.define("type".to_string(), true, type_f).unwrap();

        Self {
            type_env,
            enviroment: Rc::new(RefCell::new(env)),
            globals: Rc::new(RefCell::new(Enviroment::new())),
            source: "".to_string(),
        }
    }

    pub fn from_closure(env: Rc<RefCell<Enviroment>>, source: String, type_env: TypeEnviroment) -> Self {
        let enviroment = Rc::new(RefCell::new(Enviroment::new()));
        enviroment.borrow_mut().enclosing = Some(env.clone());

        Self {
            enviroment: enviroment,
            globals: Rc::new(RefCell::new(Enviroment::new())),
            source,
            type_env
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
        match self.enviroment.borrow_mut().get(expr.name.lexeme.clone()) {
            Err(message) => {
                let expression = Expression::Variable(expr.clone());
                let error = BeeError::report(
                    &Expression::position(expression),
                    message.as_str(),
                    "interpreter",
                    self.source.clone(),
                );
                Err(error)
            }
            Ok(value) => Ok(value),
        }
    }

    fn visit_assignment_expr(
        &mut self,
        expr: &AssignmentExpression,
    ) -> Result<LiteralValue, BeeError> {
        let value = self.evaluate(*expr.value.clone())?;

        if let Err(message) = self
            .enviroment
            .borrow_mut()
            .assign(expr.name.lexeme.clone(), value.clone())
        {
            let error = BeeError::report(
                &Expression::position(Expression::Assignment(expr.clone())),
                &message,
                "interpreter",
                self.source.clone(),
            );

            return Err(error);
        };

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpression) -> Result<LiteralValue, BeeError> {
        let left = self.evaluate(*expr.left.clone())?;

        if expr.operator.kind == TokenKind::Or {
            if self.is_truthy(left.clone()) {
                return Ok(left);
            }
        } else {
            if !self.is_truthy(left.clone()) {
                return Ok(left)
            }
        }

        return self.evaluate(*expr.right.clone());
    }

    fn visit_call_expr(&mut self, expr: &CallExpression) -> Result<LiteralValue, BeeError> {
        let callee = self.evaluate(*expr.callee.clone())?;
        let mut arguments: Vec<LiteralValue> = vec![];

        for argument in expr.args.clone() {
            arguments.push(self.evaluate(*argument.clone())?);
        }

        match callee {
            LiteralValue::Callable { arity, name, fun } => {
                if arguments.len() < arity {
                    return Err(BeeError::report(
                        &Expression::position(*expr.callee.clone()),
                        format!(
                            "{name} function expected {arity} arguments, but has founded only {}",
                            arguments.len()
                        )
                        .as_str(),
                        "interpreter",
                        self.source.clone(),
                    ));
                }

                let result = (fun)(arguments);
                Ok(result)
            }
            _ => Err(BeeError::report(
                &Expression::position(*expr.callee.clone()),
                "called using an invalid callee.",
                "interpreter",
                self.source.clone(),
            )),
        }
    }
    
    fn visit_cast_expr(&mut self, expr: &CastExpression) -> Result<LiteralValue, BeeError> {
        let to_cast = self.evaluate(*expr.casted.clone())?;
        let cast_type = expr.typing.lexeme.clone();

        let res = match cast_type.as_str() {
            "str" => to_cast.cast_str(),
            "int" => to_cast.cast_int(),
            "float" => to_cast.cast_float(),
            "bool" => to_cast.cast_bool(),
            _ => {
                if let Ok(value) = self.enviroment.borrow().get(cast_type) {
                    if let (LiteralValue::Struct { name, properties }, LiteralValue::Object(values)) = (value, to_cast.clone()) {
                        let mut i = 0;
                        let mut props: Vec<(String, LiteralValue)> = vec![];
                        
                        for value in values {
                            props.push((properties[i].clone(), value));
                            i += 1;
                        };

                        let instance = LiteralValue::Instance { typing: name, properties: props };
                        return Ok(instance);
                    }
                }   
                
                Ok(to_cast)
            }
        };

        Ok(res.unwrap())
    }
    
    fn visit_obj_expr(&mut self, expr: &ObjectExpression) -> Result<LiteralValue, BeeError> {
        let mut values: Vec<LiteralValue> = vec![];

        for expression in expr.values.clone() {
            let value = self.evaluate(expression)?;
            values.push(value);
        }

        let value = LiteralValue::Object(values);
        Ok(value)
    }
    
    fn visit_get_expr(&mut self, expr: &GetExpression) -> Result<LiteralValue, BeeError> {
        let left = self.evaluate(*expr.left.clone())?;
        
        if let LiteralValue::Instance { typing: _, properties } = left {
            match *expr.right.clone() {
                Expression::Get(expr) => self.visit_get_expr(&expr),
                Expression::Variable(expr) => {
                    let (_, value) = properties.iter().find(|(name, _)| name.clone() == expr.name.lexeme.clone()).unwrap();
                    return Ok(value.clone());
                },
                _ => unreachable!()
            }
        } else {
            unreachable!()
        }
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
            LiteralValue::Callable {
                arity: _,
                name,
                fun: _,
            } => name.clone(),
            LiteralValue::Struct { name, properties } => name.clone() + "{ " + properties.join(", ").as_str() +" }",
            LiteralValue::Object(values) => {
                let val: Vec<String> = values.iter().map(|p| -> String {
                    p.to_string()
                }).collect();

                let content = String::from("{") + val.join(", ").as_str() + "}";
                content
            },
            LiteralValue::Instance { typing, properties } => {
                let val: Vec<String> = properties.iter().map(|(name, value)| -> String {
                    format!("{name}: {}", value.to_string())
                }).collect();

                let content = String::from("{") + val.join(", ").as_str() + "}";

                format!("{}::{}", typing, content)
            }
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

        if let Err(message) =
            self.enviroment
                .borrow_mut()
                .define(stmt.name.lexeme.clone(), stmt.constant, value)
        {
            let error = BeeError::report(
                &Statement::Variable(stmt.clone()).position(),
                &message,
                "interpreter",
                self.source.clone(),
            );

            return Err(error);
        };

        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStatement) -> Result<(), BeeError> {
        let previous = self.enviroment.clone();
        let mut env = Enviroment::new();
        env.enclosing = Some(previous.clone());

        self.enviroment = Rc::new(RefCell::new(env));

        for statement in stmt.statements.clone() {
            self.execute(*statement)?;
        }

        self.enviroment = previous;

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStatement) -> Result<(), BeeError> {
        let result = self.evaluate(*stmt.condition.clone())?;

        if self.is_truthy(result) {
            self.execute(*stmt.consequent.clone())?;
        } else {
            if let Some(alternate) = stmt.alternate.clone() {
                self.execute(*alternate)?;
            }
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStatement) -> Result<(), BeeError> {
        loop {
            let condition = self.evaluate(*stmt.condition.clone())?;

            if self.is_truthy(condition) {
                self.execute(*stmt.body.clone())?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn visit_fun_stmt(&mut self, stmt: &FunctionStatement) -> Result<(), BeeError> {
        let params = stmt.params.clone();
        let name = stmt.name.lexeme.clone();
        let source = self.source.clone();
        let body = stmt.body.clone();
        let env = self.enviroment.clone();
        let type_env = self.type_env.clone();

        let fun = move |args: Vec<LiteralValue>| -> LiteralValue {
            let mut environment = Enviroment::new();
            environment.enclosing = Some(env.clone());

            for (i, (param, _)) in params.iter().enumerate() {
                if let Err(err) = environment.define(param.lexeme.clone(), true, args[i].clone()) {
                    panic!("{}", err);
                }
            }

            let mut intp =
                Interpreter::from_closure(Rc::new(RefCell::new(environment)), source.clone(), type_env.clone());

            let err = match *body.clone() {
                Statement::Block(block) => intp.visit_block_stmt(&block),
                _ => unreachable!(),
            };

            if let Err(err) = err {
                if err.location == "return" {
                    match err.object {
                        None => return LiteralValue::Nil,
                        Some(value) => return value,
                    };
                }

                panic!("{}", err.message);
            }

            LiteralValue::Nil
        };

        let value = LiteralValue::Callable {
            arity: stmt.params.len(),
            name,
            fun: Rc::new(fun),
        };

        match self
            .enviroment
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), true, value)
        {
            Ok(_) => Ok(()),
            Err(err) => Err(BeeError::report(
                &stmt.name.position,
                &err,
                "interpreter",
                self.source.clone(),
            )),
        }
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStatement) -> Result<(), BeeError> {
        let value = if let Some(value) = stmt.value.clone() {
            Some(self.evaluate(*value.clone())?)
        } else {
            None
        };

        Err(BeeError::return_v("return", &stmt.keyword.position, value))
    }
    
    fn visit_struct_stmt(&mut self, stmt: &StructStatement) -> Result<(), BeeError> {
        let name = stmt.name.lexeme.clone();
        let properties: Vec<String> = stmt.properties.iter().map(|p| -> String {
            p.name.lexeme.clone()
        }).collect();

        let v = LiteralValue::Struct {
            name: name.clone(),
            properties
        };

        self.enviroment.borrow_mut().define(name, true, v);
        Ok(())
    }
}
