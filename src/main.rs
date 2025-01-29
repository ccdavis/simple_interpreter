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

impl Op {
    // Convenience for deciding on classes of operators, mainly for determining precedence.
    pub fn is_mul_op(&self) -> bool {
        matches!(*self, Self::Mul) || matches!(*self, Self::Div)
    }

    pub fn is_add_op(&self) -> bool {
        matches!(*self, Self::Add) || matches!(*self, Self::Sub)
    }
}

#[derive(Debug, Clone)]
pub enum CompareOp {
    Gt,
    Lt,
    Ne,
    Eq,
    Lte,
    Gte,
}

#[derive(Debug, Clone)]
pub enum LangType {
    Integer,
    Float,
    String,
    Boolean,
    Named {
        name: String,
        parent: Option<Box<LangType>>,
    },
    Unit,
}

impl LangType {
    pub fn scalar(&self) -> bool {
        match self {
            LangType::Integer | LangType::Float => true,
            _ => false,
        }
    }

    pub fn comparable(lhs_t: LangType, rhs_t: LangType) -> bool {
        if lhs_t.scalar() && rhs_t.scalar() {
            return true;
        }

        match (lhs_t, rhs_t) {
            (LangType::Boolean, LangType::Boolean) => true,
            (LangType::String, LangType::String) => true,
            _ => false,
        }
    }
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
    CompareOperator(CompareOp),

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
    Comma,
    LeftBrace,
    RightBrace,
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

    pub fn is_mul_op(&self) -> bool {
        match self.value() {
            TokenValue::Operator(op) => op.is_mul_op(),
            _ => false,
        }
    }

    pub fn is_add_op(&self) -> bool {
        match self.value() {
            TokenValue::Operator(op) => op.is_add_op(),
            _ => false,
        }
    }

    pub fn is_compare_op(&self) -> bool {
        match self.value() {
            TokenValue::CompareOperator(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Op, ExprRef, ExprRef),
    Compare(CompareOp, ExprRef, ExprRef),
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

    pub fn last_exprref(&self) -> ExprRef {
        ExprRef((self.size() - 1).try_into().expect("Can't get ExprRef"))
    }

    pub fn last_two_exprref(&self) -> (ExprRef, ExprRef) {
        (
            ExprRef((self.size() - 2).try_into().expect("Can't get ExprRef")),
            ExprRef((self.size() - 1).try_into().expect("Can't get ExprRef")),
        )
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

        fn next_token(&self) -> Token {
            self.source.peek().clone()
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

        // I'm going to convert these statements into expression-statements.

        fn statement_list(&mut self) -> LangType {
            let stmt_type = self.statement();
            let mut look_ahead = self.next_token();
            while !matches!(look_ahead.value(), TokenValue::RightBrace)
                && !matches!(look_ahead.value(), TokenValue::Else)
            {
                self.statement();
                look_ahead = self.next_token();
            }

            LangType::Unit
        }

        fn statement(&mut self) -> LangType {
            let look_ahead = self.next_token();
            match look_ahead.value() {
                TokenValue::If => {
                    self.source.advance();
                    self.source.matches(&[TokenValue::LeftParen]);
                    let et = self.expression();
                    if !matches!(et, LangType::Boolean) {
                        self.print_error(
                            "Must use a boolean type of expression in an if-statement.",
                            &look_ahead,
                        );
                        std::process::exit(1);
                    }
                    self.source.matches(&[TokenValue::RightParen]);
                    self.source.matches(&[TokenValue::LeftBrace]);
                    self.statement_list();
                    self.source.matches(&[TokenValue::RightBrace]);
                    if matches!(self.next_token().value(), TokenValue::Else) {
                        self.source.advance();
                        self.source.matches(&[TokenValue::LeftBrace]);
                        self.statement_list();
                        self.source.matches(&[TokenValue::RightBrace]);
                    }
                }
                _ => panic!("Not implemented!"),
            }
            self.source.matches(&[TokenValue::SemiColon]);

            LangType::Unit
        }

        fn expression_list(&mut self) -> LangType {
            let mut expr_type = self.expression();
            while let TokenValue::Comma = self.next_token().value() {
                self.source.matches(&[TokenValue::Comma]);
                expr_type = self.expression();
            }
            expr_type
        }

        fn expression(&mut self) -> LangType {
            let expression_type = self.simple_expression();
            let look_ahead = self.next_token();
            if let TokenValue::CompareOperator(bool_op) = look_ahead.value().clone() {
                self.source.advance();
                let rhs_expression_type = self.simple_expression();

                // Some basic type checking
                if !LangType::comparable(expression_type, rhs_expression_type) {
                    let message = format!("Can't compare arguments to '{:?}'", look_ahead.value());
                    self.print_error(&message, &look_ahead);
                    std::process::exit(1);
                }

                let (lhs, rhs) = self.target.last_two_exprref();
                self.target.add(Expr::Compare(bool_op, lhs, rhs));
                LangType::Boolean
            } else {
                expression_type
            }
        }

        fn simple_expression(&mut self) -> LangType {
            // A leading '-' is possible. We set the leading operator to '+' by default.
            let mut leading_op = Op::Add;
            let look_ahead = self.next_token();
            if look_ahead.is_add_op() {
                if let TokenValue::Operator(ref op) = look_ahead.value() {
                    if matches!(op, Op::Sub) {
                        leading_op = Op::Sub;
                    }
                }
                self.source.advance();
            }

            let lhs_type = self.term();
            if matches!(leading_op, Op::Sub) {
                // Insert a -1
                self.target.add(Expr::LiteralInt(-1));
                let (value, negator) = self.target.last_two_exprref();
                self.target.add(Expr::Binary(Op::Mul, negator, value));
            }
            let rhs_type = self.simple_part();
            if matches!(rhs_type, LangType::Float) || matches!(lhs_type, LangType::Float) {
                LangType::Float
            } else {
                LangType::Integer
            }
        }

        fn term(&mut self) -> LangType {
            let lhs_type = self.factor();
            // Multiply by the term part
            let rhs_type = self.term_part();
            if matches!(lhs_type, LangType::Float) || matches!(rhs_type, LangType::Float) {
                LangType::Float
            } else {
                LangType::Integer
            }
        }

        fn term_part(&mut self) -> LangType {
            let look_ahead = self.next_token();
            if look_ahead.is_mul_op() {
                let op = if let TokenValue::Operator(mul_op) = look_ahead.value() {
                    mul_op
                } else {
                    panic!("Internal error.");
                };
                self.source.advance();

                // Parse the right-hand side and get the type of the argument to 'op'
                let rhs_type = self.factor();

                let (lhs, rhs) = self.target.last_two_exprref();
                self.target.add(Expr::Binary(op.clone(), lhs, rhs));
                let next_part_type = self.term_part();

                if matches!(rhs_type, LangType::Float) || matches!(next_part_type, LangType::Float)
                {
                    LangType::Float
                } else {
                    LangType::Integer
                }
            } else {
                LangType::Integer
            }
        }

        fn simple_part(&mut self) -> LangType {
            let look_ahead = self.next_token();
            if look_ahead.is_add_op() {
                if let TokenValue::Operator(ref op) = look_ahead.value() {
                    self.source.advance();
                    let term_type = self.term();
                    let (lhs, rhs) = self.target.last_two_exprref();
                    self.target.add(Expr::Binary(op.clone(), lhs, rhs));

                    let next_part_type = self.simple_part();
                    if matches!(next_part_type, LangType::Float)
                        || matches!(term_type, LangType::Float)
                    {
                        LangType::Float
                    } else {
                        LangType::Integer
                    }
                } else {
                    panic!(
                        "Internal error. No 'add' operator should not match the Operator variant."
                    );
                }
            } else {
                LangType::Integer
            }
        }

        // AKA "Primary expressions"
        fn factor(&mut self) -> LangType {
            let look_ahead = self.next_token();
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
