use std::env;
use program::{Program, ProgramTaskResult};
mod program;

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc: i32 = args.len().try_into().unwrap();
    let program = Program::new();
    let mut result: ProgramTaskResult = ProgramTaskResult::Success;

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
        ProgramTaskResult::Failed => panic!("(error) --> exited with failed."),
        ProgramTaskResult::Success => println!("(log) --> exited with success.")
    }
}