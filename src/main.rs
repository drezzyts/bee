use std::env;
use program::Program;

mod program;
mod position;
mod lexer;
mod token;
mod expressions;
mod visitors;
mod parser;
mod interpreter;
mod statements;

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc: i32 = args.len().try_into().unwrap();
    let mut program = Program::new();
    let mut result: Result<(), String> = Ok(());

    match argc {
        1 => result = program.run_repl(), 
        2 => {
            let filename = args[1].clone();

            if filename.eq("--help") {
                program.show_help();
            } else {
                result = program.run_file(&filename);
            }
        },
        _ => program.show_help(),
    }

    match result {
        Ok(_) => println!("(log) --> exited with success."),
        Err(err) => {
            println!("{}", err);
            panic!("(error) --> exited with failed.")
        }
    }
}