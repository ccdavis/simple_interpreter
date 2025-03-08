// Try out some interesting ideas for a low-difficulty interpreter
pub mod compiler;
pub mod expression;
pub mod interpreter;
pub mod lex;
pub mod parser;
pub mod symbols;
pub mod token;
pub mod types;

use compiler::LanguageParser;

pub fn debug(level: usize) -> bool {
    true
}

use token::Token;

// Only for use during compilation, not interpreting.
// Running consumes the tokens
pub fn run(code: Vec<Token>) {
    let mut language_parser = LanguageParser::new(code);
    let ir_pool = language_parser.parse_program();

    match ir_pool {
        Err(errors) => {
            for e in errors {
                eprintln!("{}", e);
            }
        }
        Ok(ir) => {
            // Interpret the intermediate representation.
            let value = interpreter::run(&ir, expression::ExprRef(0));
            println!("Final value: {}", &value);
        }
    }
}

use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = match args.get(1) {
        Some(filename) => filename.clone(),
        None => {
            eprintln!("Supply a file name as the first argument to the interpreter.");
            std::process::exit(1);
        }
    };
    let code = match fs::read_to_string(filename) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading input file: '{}'", e);
            std::process::exit(1);
        }
    };
    let mut scanner = lex::Scanner::new(&code);
    let tokens = scanner.tokenize();
    run(tokens);
}
