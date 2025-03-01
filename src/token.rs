use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub struct TokenLocation {
    pub line: usize,
    pub column: usize,
    pub column_end: usize,
}

#[derive(Debug, Clone)]
pub struct Token(pub TokenValue, pub TokenLocation);

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    // user names for things
    Ident(String),

    // Data value literals
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,

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
    SemiColon,
    Colon,
    At,
    Dot,
    DotDot,
    Comma,
    LeftBrace,
    RightBrace,
    Comment(String),

    Eof,
}

static RESERVED_WORDS: &'static [TokenValue] = &[
    TokenValue::Output,
    TokenValue::Input,
    TokenValue::If,
    TokenValue::Let,
    TokenValue::Else,
    TokenValue::For,
];

pub fn reserved_words_lookup() -> HashMap<String, TokenValue> {
    let mut lookup = HashMap::new();
    for tv in RESERVED_WORDS {
        lookup.insert(tv.to_string(), tv.clone());
    }
    lookup
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

#[derive(Debug, Clone, PartialEq)]
pub enum CompareOp {
    Gt,
    Lt,
    Ne,
    Eq,
    Lte,
    Gte,
}

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ident(n) => write!(f, "{}", &n),
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(flt) => write!(f, "{}", flt),
            Self::Str(s) => write!(f, "\"{}\"", &s),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Unit => write!(f, "()"),
            Self::Operator(op) => write!(f, "{}", &op),
            Self::CompareOperator(op) => write!(f, "{}", &op),
            Self::Output => write!(f, "output"),
            Self::Input => write!(f, "input"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Let => write!(f, "let"),
            Self::Assign => write!(f, ":="),
            Self::For => write!(f, "for"),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::SemiColon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::At => write!(f, "@"),
            Self::Dot => write!(f, "."),
            Self::DotDot => write!(f, ".."),
            Self::Colon => write!(f, ":"),
            Self::Comment(c) => write!(f, "# {}", &c),
            Self::Eof => write!(f, "EOF"),
        }
    }
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

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Div => write!(f, "/"),
            Self::Mul => write!(f, "*"),
        }
    }
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Eq => write!(f, "="),
            Self::Ne => write!(f, "<>"),
            Self::Lte => write!(f, "<="),
            Self::Gte => write!(f, ">="),
        }
    }
}

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
    Token(TokenValue::CompareOperator(CompareOp::Eq), no_loc())
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
