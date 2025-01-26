// Try out some interesting ideas for a low-difficulty interpreter
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub struct TokenLocation {
    line: usize,
    column: usize,
    column_end: usize,
}

// Op is shared between tokens and expressions -- they exactly match token types
// and types of operations in expressions.
#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}
#[derive(Debug, Clone)]
pub enum LangType {
    Integer,
    Float,
    String,
    Boolean,
    Named(String),
}

#[derive(Debug, Clone)]
pub enum TokenValue {
    // user names for things
    Ident(String),

    // Data value literals
    Integer(i64),
    Float(f64),
    Str(String),

    // Any data operations
    Operator(Op),

    // Keywords
    Output,
    Input,
    If,
    Else,
    Let,
    Assign,
    For,

    // Symbols
    LeftParen,
    RightParen,
    EqualSign,
    SemiColon,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token(TokenValue, TokenLocation);

impl Token {
    pub fn value(&self) -> &TokenValue {
        &self.0
    }

    pub fn location(&self) -> &TokenLocation {
        &self.1
    }

    pub fn same_type(&self, t: &TokenValue) -> bool {
        // Only compare varients, not content of enums
        std::mem::discriminant(self.value()) == std::mem::discriminant(t)
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Op, ExprRef, ExprRef),
    Output(ExprRef),
    If(ExprRef, ExprRef, ExprRef),
    Let(ExprRef, ExprRef, LangType), // name, value,type
    Assign(ExprRef, ExprRef),
    Call(ExprRef), // anything labeled in the source with a name
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    For(ExprRef, ExprRef),
    Type(LangType),
    Unit,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprRef(u32);

pub struct ExpressionPool {
    // Indexed by ExprRef
    exprs: Vec<Expr>,
    // Use a token to hold the location of a language part for error reporting
    index_to_source: Vec<Token>,
}

impl ExpressionPool {
    pub fn size(&self) -> usize {
        self.exprs.len()
    }

    pub fn default() -> Self {
        Self {
            exprs: Vec::with_capacity(100_000_000),
            index_to_source: Vec::with_capacity(100_000_000),
        }
    }

    /// Dereference an AST node reference, obtaining the underlying `Expr`.
    fn get(&self, expr_ref: ExprRef) -> &Expr {
        &self.exprs[expr_ref.0 as usize]
    }

    /// Add an expression to the pool and get a reference to it.
    fn add(&mut self, expr: Expr) -> ExprRef {
        let idx = self.exprs.len();
        self.exprs.push(expr);
        ExprRef(idx.try_into().expect("too many exprs in the pool"))
    }
}

// helper functions
pub fn no_loc() -> TokenLocation {
    TokenLocation {
        column: 0,
        line: 0,
        column_end: 0,
    }
}

pub fn int_tok(n: i64) -> Token {
    Token(TokenValue::Integer(n), no_loc())
}

pub fn flt_tok(n: f64) -> Token {
    Token(TokenValue::Float(n), no_loc())
}

pub fn str_tok(s: &str) -> Token {
    Token(TokenValue::Str(s.to_string()), no_loc())
}

pub fn op_tok(o: Op) -> Token {
    Token(TokenValue::Operator(o), no_loc())
}

pub fn eof_tok() -> Token {
    Token(TokenValue::Eof, no_loc())
}

pub mod parser {
    use super::*;

    pub struct ParserState {
        tokens: Vec<Token>,
        current: usize,
        // TODO: errors and the file path of the source if any
    }

    impl ParserState {
        // We hope to move the tokens into the ParserState, not borrow them.
        pub fn new(tokens: Vec<Token>) -> Self {
            Self { tokens, current: 0 }
        }

        pub fn matches(&mut self, types: &[TokenValue]) -> bool {
            let found = types.iter().any(|t| self.check(t));
            if found {
                self.advance();
            }
            found
        }

        fn check(&self, token: &TokenValue) -> bool {
            self.is_finished() || self.peek().same_type(token)
        }

        fn is_finished(&self) -> bool {
            matches!(self.peek().value(), &TokenValue::Eof)
        }

        fn peek(&self) -> &Token {
            &self.tokens[self.current]
        }

        fn previous(&self) -> &Token {
            &self.tokens[self.current - 1]
        }

        fn advance(&mut self) -> &Token {
            if !self.is_finished() {
                self.current += 1
            }
            self.previous()
        }
    }

    // Only for use during compilation, not interpreting.
    #[derive(Clone, Debug)]
    pub struct SymbolTable(Vec<HashMap<String, ExprRef>>);

    impl SymbolTable {
        pub fn default() -> Self {
            SymbolTable(vec![HashMap::new()])
        }

        pub fn get(&self, frame: usize, name: &str) -> Option<&ExprRef> {
            self.0[frame].get(name)
        }

        pub fn set(&mut self, frame: usize, name: &str, value: ExprRef) {
            if self.0.len() >= frame {
                // This should never happen in a properly designed parser.
                panic!(
                    "Stack frame not created. Internal error adding {} on frame {}",
                    name, frame
                );
            }
            self.0[frame].insert(name.to_string(), value);
        }

        pub fn enter_frame(&mut self) -> usize {
            self.0.push(HashMap::new());
            self.0.len() - 1
        }

        pub fn exit_frame(&mut self) -> usize {
            self.0.pop();
            self.0.len() - 1
        }
    }

    pub struct LanguageParser {
        source: ParserState,
        target: ExpressionPool,
        symbols: SymbolTable,
        current_frame: usize,
    }

    impl LanguageParser {
        // TODO this is really basic
        fn print_error(&self, message: &str, t: &Token) {
            eprintln!(
                "{} at {}, {}",
                message,
                t.location().line,
                t.location().column
            );
        }

        pub fn new(code: Vec<Token>) -> Self {
            Self {
                source: ParserState::new(code),
                target: ExpressionPool::default(),
                symbols: SymbolTable::default(),
                current_frame: 0,
            }
        }

        // The grammar specific logic ---------------------
        pub fn parse_program(&mut self) -> Option<&ExpressionPool> {
            self.factor();
            Some(&self.target)
        }

        fn expression(&mut self) -> LangType {
            LangType::Integer
        }

        fn factor(&mut self) -> LangType {
            let look_ahead = self.source.peek().clone();
            let data_type = match look_ahead.value() {
                TokenValue::Float(f) => {
                    self.target.add(Expr::LiteralFloat(*f));

                    self.source.advance();
                    LangType::Float
                }
                TokenValue::Integer(i) => {
                    self.target.add(Expr::LiteralInt(*i));

                    self.source.advance();
                    LangType::Integer
                }
                TokenValue::Str(s) => {
                    self.target.add(Expr::LiteralString(s.clone()));

                    self.source.advance();
                    LangType::String
                }
                TokenValue::Ident(name) => {
                    let ste = self.symbols.get(self.current_frame, name).clone();
                    if let Some(value_storage) = ste {
                        self.target.add(Expr::Call(*value_storage));
                        self.source.advance();

                        // get type of the referenced value
                        let orig_expr: &Expr = self.target.get(*value_storage);
                        if let Expr::Let(_, _, data_type) = orig_expr {
                            data_type.clone()
                        } else {
                            panic!("Internal parser error. A call to '{}' has a reference to  something other than a 'let' statement.", &name );
                        }
                    } else {
                        let msg = format!("Undeclared identifier '{}'", name);
                        self.print_error(&msg, &look_ahead);
                        std::process::exit(1);
                    }
                }
                TokenValue::LeftParen => {
                    self.source.advance();
                    let expression_type = self.expression();
                    if self.source.matches(&[TokenValue::RightParen]) {
                        expression_type
                    } else {
                        self.print_error("Expected ')'", &look_ahead);
                        std::process::exit(1);
                    }
                }
                _ => {
                    let msg = format!(
                        "Unexpected token, not a legal part of a factor: {:?}",
                        &look_ahead.value()
                    );
                    self.print_error(&msg, &look_ahead);
                    std::process::exit(1);
                }
            };

            data_type
        }
    } // impl
}

// Running consumes the tokens
pub fn run(code: Vec<Token>) {
    let mut language_parser = parser::LanguageParser::new(code);
    let syntax_pool = language_parser
        .parse_program()
        .expect("Error during parsing.");

    // Interpret the syntax pool
}

mod test {

    use super::*;

    fn test_program1_tokens() -> Vec<super::Token> {
        vec![
            int_tok(5),
            op_tok(Op::Add),
            int_tok(9),
            op_tok(Op::Mul),
            int_tok(55),
            eof_tok(),
        ]
    }

    #[test]
    fn test_parser() {
        let code = test_program1_tokens();
        let mut language_parser = parser::LanguageParser::new(code);
        let expr_pool = language_parser
            .parse_program()
            .expect("Error during parsing.");
        assert!(expr_pool.size() > 0);
    }
}

fn main() {
    println!("Hello, world!");
}
