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
