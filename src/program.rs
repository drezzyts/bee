#![allow(unused)]

use std::io::{self, Read, Write};
use std::fs::File;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::position::Position;
use crate::visitors::PrinterVisitor;

pub struct Program;

impl Program {
    pub fn new() -> Self {
        Self
    }

    fn run(&self, source: &String) -> Result<(), String> {
        let mut lexer = Lexer::new(source);
        
        match lexer.read_tokens() {
            Ok(tokens) => {
                let mut parser = Parser::new(tokens.clone());
                let program = parser.parse()?;
                let mut printer = PrinterVisitor::new();
                println!("{}", printer.print(&program));
                
                for token in tokens {
                    println!("{:?}", token);
                }

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

    pub fn run_repl(&self) -> Result<(), String> {
        let mut input = String::new();

        loop {
            print!(">> ");

            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            if input.trim().eq(":q") {
                break;
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
