use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{expressions::LiteralValue, typechecker::Type}
;

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub constant: bool,
    pub value: LiteralValue,
}

impl Variable {
    pub fn new(name: String, constant: bool, value: LiteralValue) -> Self {
        Self { name, constant, value }
    }
}

#[derive(Clone, Debug)]
pub struct Enviroment {
    pub values: HashMap<String, Variable>,
    pub enclosing: Option<Rc<RefCell<Enviroment>>>
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None 
        }
    }

    pub fn define(&mut self, name: String, constant: bool, value: LiteralValue) -> Result<(), String> {
        if constant && value == LiteralValue::Nil {
            return Err("attempt to define a constant variable with nil value.".to_string());
        }

        let variable = Variable::new(name.clone(), constant, value);
        self.values.insert(name, variable);
        Ok(())
    }

    pub fn assign(&mut self, name: String, value: LiteralValue) -> Result<LiteralValue, String> {
        if self.has(name.clone()) {
            let mut variable = self.get_variable_struct(name.clone()).unwrap();    
            variable.value = value.clone();

            if variable.constant {
                return Err(format!("attempt to reassign a value to a constant variable: '{name}'"));
            }

            self.values.insert(name, variable);
            return Ok(value);
        }

        if let Some(scope) = &self.enclosing {
            return scope.borrow_mut().assign(name.clone(), value);
        }

        Err(format!("attempt to reassign a value to a undefined variable: '{name}'"))
    }

    pub fn get(&self, name: String) -> Result<LiteralValue, String> { 
        if self.has(name.clone()) {
            let variable = self.values.get(name.as_str()).unwrap().clone();
            return Ok(variable.value.clone())
        } 
        
        let error = format!("referenced a variable that is not defined: '{name}'");

        if let Some(scope) = &self.enclosing {
            return scope.borrow().get(name);
        }

        Err(error)
    }

    pub fn get_variable_struct(&self, name: String) -> Option<Variable> {
        if self.has(name.clone()) {
            let variable = self.values.get(name.as_str()).unwrap().clone();
            return Some(variable);
        }

        None
    }

    pub fn has(&self, name: String) -> bool {
        if let Some(_) = self.values.get(name.as_str()) {
            true
        } else {
            false
        }
    }
}


#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeVariable {
    pub name: String,
    pub constant: bool,
    pub value: Type,
}

impl TypeVariable {
    pub fn new(name: String, constant: bool, value: Type) -> Self {
        Self { name, constant, value }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeEnviroment {
    pub values: HashMap<String, TypeVariable>,
    pub enclosing: Option<Rc<RefCell<TypeEnviroment>>>
}

impl TypeEnviroment {
  pub fn new() -> Self {
    Self { values: HashMap::new(), enclosing: None }
  }

  pub fn define(&mut self, name: String, constant: bool, value: Type) -> Type {
    let variable = TypeVariable::new(name.clone(), constant, value.clone());
    self.values.insert(name, variable);
    
    value
  }

  pub fn lookup(&mut self, name: String) -> Result<Type, String> {
    let variable = self.resolve(name)?;
    Ok(variable.value.clone())
  }

  pub fn resolve(&self, name: String) -> Result<TypeVariable, String> {
    if self.values.contains_key(name.as_str()) {
        Ok(self.values.get(name.as_str()).unwrap().clone())
    } else if let Some(scope) = self.enclosing.clone() {
        Ok(scope.borrow().resolve(name)?.clone())
    } else {
        Err(format!("variable {name} has referenced, but this is not defined."))
    }
  }
}
