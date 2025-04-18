use super::debug;
use super::token::{CompareOp, LogicalOp, Op, Token};
use super::types::LangType;

const EXPR_DEBUG: bool = false;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(Op, ExprRef, ExprRef),
    Compare(CompareOp, ExprRef, ExprRef),
    Logical(LogicalOp, ExprRef, ExprRef),
    Output(ExprRef, LangType),
    StmtList(ExprRef, ExprRef), // start addr and end addr useful for looping
    // To interpret or compile 'If' remember to increment the then_addr by 1and jump there
    // to keep evaluating and return from else_addr.
    If(ExprRef, ExprRef, Option<ExprRef>),
    Let(ExprRef, LangType), // value,type
    Assign(ExprRef, ExprRef),
    Call(ExprRef), // anything labeled in the source with a name
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    LiteralBool(bool),
    For(ExprRef, ExprRef),
    Type(LangType),
    Unit,
}

impl Expr {
    pub fn same_type(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExprRef(pub u32);
#[derive(Clone, Debug)]
pub struct ExpressionPool {
    // Indexed by ExprRef
    pub exprs: Vec<Expr>,
    pub types: Vec<LangType>,
    // Use a token to hold the location of a language part for error reporting
    index_to_source: Vec<Token>,
}

impl ExpressionPool {
    pub fn size(&self) -> usize {
        self.exprs.len()
    }

    pub fn new() -> Self {
        Self {
            exprs: Vec::with_capacity(100_000),
            types: Vec::with_capacity(100_000),
            index_to_source: Vec::with_capacity(100_000),
        }
    }

    pub fn debug_dump(&self) {
        for address in 0..self.size() {
            let exp = self.exprs[address].clone();
            let exp_type = self.types[address].clone();
            println!("{}:\t{:?}\t\t{:?}", address, exp, exp_type);
        }
    }

    /// Dereference an AST node reference, obtaining the underlying `Expr`.
    pub fn get(&self, expr_ref: ExprRef) -> &Expr {
        &self.exprs[expr_ref.0 as usize]
    }

    // Get a clone of an expression at address e for convenience
    pub fn expr(&self, e: usize) -> Expr {
        self.exprs[e].clone()
    }

    /// Add an expression to the pool and get a reference to it.
    pub fn add(&mut self, expr: Expr) -> ExprRef {
        let idx = self.exprs.len();
        self.exprs.push(expr.clone());
        self.types.push(LangType::Unresolved);
        let addr = ExprRef(idx.try_into().expect("too many exprs in the pool"));
        if EXPR_DEBUG {
            println!("Add {}  {:?}", addr.0, &expr)
        }
        addr
    }

    pub fn add_with_type(&mut self, expr: Expr, expr_type: &LangType) -> ExprRef {
        let idx = self.exprs.len();
        self.exprs.push(expr.clone());
        self.types.push(expr_type.clone());
        let addr = ExprRef(idx.try_into().expect("too many exprs in the pool"));
        if EXPR_DEBUG {
            println!("Add {}  {:?}", addr.0, &expr);
        }
        addr
    }

    pub fn update(&mut self, expr_ref: ExprRef, expr: Expr) {
        if EXPR_DEBUG {
            println!("Update {}  {:?}", expr_ref.0, &expr)
        }
        self.exprs[expr_ref.0 as usize] = expr
    }

    pub fn get_type(&self, expr_ref: ExprRef) -> LangType {
        self.types[expr_ref.0 as usize].clone()
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
