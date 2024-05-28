#![allow(unused)]

use std::io::{self, Read, Write};
use std::fs::File;

use crate::enviroment::{self, Enviroment, TypeEnviroment};
use crate::error::BeeError;
use crate::interpreter::{self, Interpreter};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::position::Position;
use crate::token::Token;
use crate::typechecker::{self, TypeChecker};

pub struct Program {
    pub print_tokens: bool,
    pub interpreter: Interpreter,
    pub type_env: TypeEnviroment,
}

impl Program {
    pub fn new() -> Self {
        let type_env = TypeEnviroment::new();
        Self { print_tokens: false, interpreter: Interpreter::new(type_env.clone()), type_env: type_env }
    }

    fn run(&mut self, source: &String) -> Result<(), BeeError> {
        self.interpreter.type_env = self.type_env.clone();

        let mut lexer = Lexer::new(source);
        
        match lexer.read_tokens() {
            Ok(tokens) => {
                let mut parser = Parser::new(tokens.clone(), source.clone());
                let program = parser.parse()?;

                if self.print_tokens {
                    Program::print_tokens(tokens.clone());
                }

                let type_checker = TypeChecker::new(source.clone());
                
                for stmt in program.clone() {
                    type_checker.exec(&stmt, &mut self.type_env)?;
                }

                let result = self.interpreter.interpret(program, source.clone())?;

                Ok(())
            },
            Err(error) => Err(error)
        }
    }

    fn print_tokens(tokens: Vec<Token>) -> () {
        for token in tokens.clone() {
            println!("{:?}", token);
        }
    }

    pub fn run_repl(&mut self) -> Result<(), BeeError> {
        println!("bee repl v0.1 - commands:\n\t* quit --> :q\n\t* print tokens --> :tokens\n");
        let mut input = String::new();

        loop {
            print!(">> ");

            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            match input.trim() {
                ":q" => break,
                ":tokens" => {
                    self.print_tokens = !self.print_tokens;
                    println!("(debug) --> print tokens: {}", self.print_tokens);
                    input.clear();
                    continue;
                },
                _ => ()
            }

            if let Err(error) = self.run(&input) {
                println!("{}", error.message);
            }

            input.clear();
        }

        Ok(())
    }

    pub fn run_file(&mut self, filename: &String) -> Result<(), BeeError> {
        match File::open(filename) {
            Ok(mut file) => {
                let mut buf = String::new();
                file.read_to_string(&mut buf).unwrap();
                return self.run(&buf);
            }
            Err(_) => {
                let pos = Position { line: 0, cstart: 0, cend: 0 };
                let err = BeeError::error(&pos, "unexpected error ocurred while reading this file", "".to_string());
                return Err(err);
            }
        };
    }
    
    pub fn show_help(&self) {
      println!("Usage:\n\t* bee [filename]\n\t* bee --help\n\t* bee");
    }
}
