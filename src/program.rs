#![allow(unused)]

use std::io::{self, Read, Write};
use std::fs::File;

use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::position::Position;
use crate::visitors::PrinterVisitor;

pub struct Program {
    pub print_tokens: bool,
    pub print_ast: bool
}

impl Program {
    pub fn new() -> Self {
        Self { print_ast: false, print_tokens: false }
    }

    fn run(&self, source: &String) -> Result<(), String> {
        let mut lexer = Lexer::new(source);
        
        match lexer.read_tokens() {
            Ok(tokens) => {
                if self.print_tokens {
                    for token in tokens.clone() {
                        println!("{:?}", token);
                    }
                }

                let mut parser = Parser::new(tokens);
                let program = parser.parse()?;
                
                let mut printer = PrinterVisitor::new();
                if self.print_ast {
                    for stmt in program.clone() {
                        println!("{}", printer.print(&stmt));
                    }
                }

                let mut interpreter = Interpreter;
                let result = interpreter.interpret(program);

                Ok(())
            },
            Err(error) => Err(error)
        }
    }

    pub fn error(pos: Position, message: &str) -> String {
        Program::report(pos, "", message)
    }

    pub fn report(pos: Position, location: &str, message: &str) -> String {
        format!("(error) ~{location} {} --> {message}", pos.to_string())
    }

    pub fn run_repl(&mut self) -> Result<(), String> {
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
                println!("{error}");
            }

            input.clear();
        }

        Ok(())
    }

    pub fn run_file(&self, filename: &String) -> Result<(), String> {
        match File::open(filename) {
            Ok(mut file) => {
                let mut buf = String::new();
                file.read_to_string(&mut buf).unwrap();
                return self.run(&buf);
            }
            Err(_) => {
                let pos = Position { line: 0, cstart: 0, cend: 0 };
                let err = Program::error(pos, "unexpected error ocurred while reading this file");
                return Err(err);
            }
        };
    }
    
    pub fn show_help(&self) {
      println!("Usage:\n\t* bee [filename]\n\t* bee --help\n\t* bee");
    }
}
