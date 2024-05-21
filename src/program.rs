#![allow(unused)]

use std::io::{self, Read, Write};
use std::fs::File;
use std::process::Termination;
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

    pub fn run_repl(&self) -> ProgramTaskResult {
        let mut input = String::new();

        loop {
            print!(">> ");

            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            if input.trim().eq(":q") {
                break ProgramTaskResult::Success;
            }

            input.clear();
        }
    }

    pub fn run_file(&self, filename: &String) -> ProgramTaskResult {
        match File::open(filename) {
            Ok(mut file) => {
                let mut buf = String::new();
                file.read_to_string(&mut buf).unwrap();
                println!("{buf}");
                return ProgramTaskResult::Success
            }
            Err(_) => {
                println!("(error) --> unexpected error ocurred while reading this file");
                return ProgramTaskResult::Failed
            }
        };
    }
    
    pub fn show_help(&self) {
      println!("Usage:\n\t* bee [filename]\n\t* bee --help\n\t* bee");
    }
}
