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
    let syntax_pool = language_parser
        .parse_program()
        .expect("Error during parsing.");

    // Interpret the syntax pool
}

fn main() {
    println!("Hello, world!");
}
