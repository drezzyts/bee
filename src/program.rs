#![allow(unused)]

use std::io::{self, Read, Write};
use std::fs::File;

use crate::enviroment::{self, Enviroment};
use crate::error::BeeError;
use crate::interpreter::{self, Interpreter};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::position::Position;
use crate::visitors::PrinterVisitor;

pub struct Program {
    pub print_tokens: bool,
    pub print_ast: bool,
    pub interpreter: Interpreter
}

impl Program {
    pub fn new() -> Self {
        Self { print_ast: false, print_tokens: false, interpreter: Interpreter::new() }
    }

    fn run(&mut self, source: &String) -> Result<(), BeeError> {
        let mut lexer = Lexer::new(source);
        
        match lexer.read_tokens() {
            Ok(tokens) => {
                let mut parser = Parser::new(tokens.clone(), source.clone());
                let mut printer = PrinterVisitor::new(&mut self.interpreter.enviroment);

                if self.print_tokens {
                    for token in tokens.clone() {
                        println!("{:?}", token);
                    }
                }

                let program = parser.parse()?;
                
                if self.print_ast {
                    for stmt in program.clone() {
                        println!("{}", printer.print(&stmt));
                    }
                }

                
                let result = self.interpreter.interpret(program, source.clone())?;

                Ok(())
            },
            Err(error) => Err(error)
        }
    }

    pub fn run_repl(&mut self) -> Result<(), BeeError> {
        println!("bee repl v0.1 - commands:\n\t* quit --> :q\n\t* print tokens --> :tokens\n\t* print ast --> :ast\n");
        let mut input = String::new();

        loop {
            print!(">> ");

            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            match input.trim() {
                ":q" => break,
                ":ast" => { 
                    self.print_ast = !self.print_ast;
                    println!("(debug) --> print abstract syntax tree: {}", self.print_ast);
                    input.clear();
                    continue
                }
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
