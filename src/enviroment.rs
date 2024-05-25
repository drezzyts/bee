use std::collections::HashMap;

use crate::expressions::LiteralValue
;

#[derive(Clone)]
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
pub struct Enviroment {
    pub values: HashMap<String, Variable>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
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

        Err(format!("attempt to reassign a value to a undefined variable: '{name}'"))
    }

    pub fn get(&self, name: String) -> LiteralValue {
        if self.has(name.clone()) {
            let variable = self.values.get(name.as_str()).unwrap().clone();
            variable.value.clone()
        } else {
            LiteralValue::Nil
        }
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
