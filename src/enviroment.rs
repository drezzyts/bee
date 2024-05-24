use std::collections::HashMap;

use crate::{expressions::LiteralValue, position::{self, Position}, program::Program, token::Token};

pub struct Enviroment {
    pub values: HashMap<String, LiteralValue>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: LiteralValue) -> () {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: LiteralValue) -> () {
      if self.has(name.clone()) {
        self.values.insert(name, value);
      }
    }

    pub fn get(&self, name: String) -> LiteralValue {
        if self.has(name.clone()) {
            self.values.get(name.as_str()).unwrap().clone()
        } else {
            LiteralValue::Nil
        }
    }

    pub fn has(&self, name: String) -> bool {
        if let Some(_) = self.values.get(name.as_str()) {
          true
       } else {
          false
       }
    }
}
