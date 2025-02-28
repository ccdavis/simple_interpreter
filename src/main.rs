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
    let value = interpreter::run(&syntax_pool, expression::ExprRef(0));
    println!("Final value: {}", &value);
}

fn main() {
    let text = "let a:=1;
                        let b:=1;
                        if (a = 1) { b:=9} else { b:=0};
                        output b;
                        for (a < 25) {
                            b:= b + 3;
                            a := a + 1
                       };
                       output a;
                       output b
                        ";

    let mut scanner = lex::Scanner::new(text);
    let code = scanner.tokenize();
    run(code);
}
