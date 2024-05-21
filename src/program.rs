#![allow(unused)]

use std::io::{self, Read, Write};
use std::fs::File;

use crate::position::Position;

pub struct Program {
    had_error: bool,
}

pub enum ProgramTaskResult {
    Failed,
    Success,
}

impl Program {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    fn run(&self, source: &String) -> ProgramTaskResult {
        ProgramTaskResult::Success
    }

    pub fn error(pos: Position, message: &str) -> String {
        Program::report(pos, "", message)
    }

    pub fn report(pos: Position, location: &str, message: &str) -> String {
        format!("(error) ~{location} {} --> {message}", pos.to_string())
    }

    pub fn run_repl(&self) -> Result<ProgramTaskResult, String> {
        let mut input = String::new();

        loop {
            print!(">> ");

            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            if input.trim().eq(":q") {
                break;
            }

            input.clear();
        }

        Ok(ProgramTaskResult::Success)
    }

    pub fn run_file(&self, filename: &String) -> Result<ProgramTaskResult, String> {
        match File::open(filename) {
            Ok(mut file) => {
                let mut buf = String::new();
                file.read_to_string(&mut buf).unwrap();
                println!("{buf}");
                return Ok(ProgramTaskResult::Success)
            }
            Err(_) => {
                let pos = Position { line: -1, cstart: -1, cend: -1 };
                let err = Program::error(pos, "unexpected error ocurred while reading this file");
                return Err(err);
            }
        };
    }
    
    pub fn show_help(&self) {
      println!("Usage:\n\t* bee [filename]\n\t* bee --help\n\t* bee");
    }
}
