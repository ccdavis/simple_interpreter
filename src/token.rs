#[derive(Debug, Clone)]
pub struct TokenLocation {
    pub line: usize,
    pub column: usize,
    pub column_end: usize,
}

// Op is shared between tokens and expressions -- they exactly match token types
// and types of operations in expressions.
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum CompareOp {
    Gt,
    Lt,
    Ne,
    Eq,
    Lte,
    Gte,
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
pub struct Token(pub TokenValue, pub TokenLocation);

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

// helper functions
pub fn no_loc() -> TokenLocation {
    TokenLocation {
        column: 0,
        line: 0,
        column_end: 0,
    }
}

pub fn if_stmt_tok() -> Token {
    Token(TokenValue::If, no_loc())
}

pub fn else_tok() -> Token {
    Token(TokenValue::Else, no_loc())
}

pub fn output_tok() -> Token {
    Token(TokenValue::Output, no_loc())
}

pub fn for_stmt_tok() -> Token {
    Token(TokenValue::For, no_loc())
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

pub fn compare_op_tok(o: CompareOp) -> Token {
    Token(TokenValue::CompareOperator(o), no_loc())
}

pub fn stmt_terminator_tok() -> Token {
    Token(TokenValue::SemiColon, no_loc())
}

pub fn let_stmt_tok() -> Token {
    Token(TokenValue::Let, no_loc())
}

pub fn equals_tok() -> Token {
    Token(TokenValue::EqualSign, no_loc())
}

pub fn assign_tok() -> Token {
    Token(TokenValue::Assign, no_loc())
}

pub fn ident_tok(n: &str) -> Token {
    Token(TokenValue::Ident(n.to_string()), no_loc())
}

pub fn left_brace_tok() -> Token {
    Token(TokenValue::LeftBrace, no_loc())
}

pub fn right_brace_tok() -> Token {
    Token(TokenValue::RightBrace, no_loc())
}

pub fn left_paren_tok() -> Token {
    Token(TokenValue::LeftParen, no_loc())
}

pub fn right_paren_tok() -> Token {
    Token(TokenValue::RightParen, no_loc())
}

pub fn eof_tok() -> Token {
    Token(TokenValue::Eof, no_loc())
}
