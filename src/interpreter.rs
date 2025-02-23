use super::expression::{Expr, ExprRef, ExpressionPool};
use super::token::{CompareOp, Op};

use std::fmt;

#[derive(Clone)]
pub enum Value {
    Int(i64),
    Flt(f64),
    Str(String),
    Bool(bool),
    None,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Flt(n) => write!(f, "{}", n),
            Self::Str(s) => write!(f, "{}", &s),
            Self::None => write!(f, "None"),
            Self::Bool(b) => match *b {
                true => write!(f, "True"),
                false => write!(f, "False"),
            }, // bool
        } // match types
    } // fmt()
} // display

impl Value {
    pub fn add(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_add(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i + (*j as f64)),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 + *j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i + *j),
            (Value::Str(ref s), Value::Str(ref t)) => Value::Str(s.to_owned() + t),
            _ => panic!("Invalid operands for '+'"),
        }
    }

    pub fn sub(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_sub(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i - *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 - *j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i - *j),
            _ => panic!("Invalid operands for '+'"),
        }
    }

    pub fn div(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_div(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i / *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 / *j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i / *j),
            _ => panic!("Invalid operands for '+'"),
        }
    }

    pub fn mul(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_mul(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i * *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 * j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i * j),
            _ => panic!("Invalid operands for '+'"),
        }
    }

    pub fn less_than(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i < *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i < *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j > *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i < *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    pub fn equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i == *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i == *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j == *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i == *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    pub fn not_equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i != *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i != *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j != *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i != *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    pub fn greater_than(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i > *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i > *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j < *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i > *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    pub fn greater_than_equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i >= *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i >= *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j <= *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i >= *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    pub fn less_than_equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i <= *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i <= *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j >= *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i <= *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }
}

pub fn skip_statements(pool: &ExpressionPool, expr_ref: &ExprRef) -> usize {
    match &pool.exprs[expr_ref.0 as usize] {
        Expr::StmtList(_head, ref tail) => tail.0 as usize,
        _ => panic!("Internal error: Can't skip any expression other than a statement-list."),
    }
}

pub fn run(pool: &ExpressionPool, root: ExprRef) -> Value {
    let mut state: Vec<Value> = vec![Value::None; pool.size()];
    let mut stack = vec![(0 as usize, 0 as usize); 100];
    let mut sp = 0 as usize;

    //for (i, expr) in pool.exprs.iter().enumerate() {
    let mut i: usize = 0;
    loop {
        let expr = &pool.exprs[i];
        println!("{}:  Execute {:?}", i, &expr);
        let result = match expr {
            Expr::Output(value_addr, _) => {
                let value =  &state[value_addr.0 as usize];
                println!("{}", value);
                // This is mostly for diagnostics; normally the result of 'output' would be ignored.
                value.clone()
            },
            // This won't be matched first; 'state' will have been populated
            // from evaluating some literals first by the time operations are reached.
            Expr::Binary(op, lhs, rhs) => {
                let lhs = &state[lhs.0 as usize];
                let rhs = &state[rhs.0 as usize];
                match op {
                    Op::Add => lhs.add(rhs),
                    Op::Sub => lhs.sub(rhs),
                    Op::Div => lhs.div(rhs),
                    Op::Mul => lhs.mul(rhs),
                }
            }
            Expr::Compare(op, lhs, rhs) => {
                let lhs = &state[lhs.0 as usize];
                let rhs = &state[rhs.0 as usize];
                println!("Compare {} {:?} {}", lhs, op, rhs);
                match op {
                    CompareOp::Eq => lhs.equal(rhs),
                    CompareOp::Ne => lhs.not_equal(rhs),
                    CompareOp::Lt => lhs.less_than(rhs),
                    CompareOp::Gt => lhs.greater_than(rhs),
                    CompareOp::Lte => lhs.less_than_equal(rhs),
                    CompareOp::Gte => lhs.greater_than_equal(rhs),
                }
            }
            Expr::LiteralInt(i) => Value::Int(*i),
            Expr::LiteralFloat(f) => Value::Flt(*f),
            Expr::LiteralString(s) => Value::Str(s.clone()),
            Expr::LiteralBool(b) => Value::Bool(*b),
            Expr::Let(assigned_value, _assigned_type) => {
                let initial_value = &state[assigned_value.0 as usize];
                // TODO: You could put a runtime type-check here
                initial_value.clone()
            }
            Expr::Call(storage_addr) => state[storage_addr.0 as usize].clone(),
            Expr::Assign(lhs, rhs) => {
                state[lhs.0 as usize] = state[rhs.0 as usize].clone();
                Value::None
            }
            Expr::StmtList(start, finish) => {
                // Any time we get into a statement list it means something has been pushed on the control-flow stack
                // and so when  completing the list we have to pop the stack, and change control to the value
                // stored on the top. That is done elsewhere.
                if sp > 0 {
                    println!("Enter statement liststarting at {}, {}", i + 1, finish.0);
                    // 'start' is where the first statement value will be placed, likewise with 'finish' for the last.
                    // The code to execute however begins immediately after the location of the statement list
                    // The 'finish' address is used to check the counter 'i and know when the statement list is done.
                    stack[sp - 1].1 = finish.0 as usize;
                }
                i += 1;
                continue;
            }
            Expr::For(cond_addr, loop_addr) => {
                match &state[cond_addr.0 as usize] {
                    Value::Bool(b) => {
                        if *b {
                            println!("Enter for loop");
                            sp += 1;
                            // When in a statement-list (sp>0) we restore the program pointer
                            // to  stack[sp-1].0. In a loop we need to jump back to the conditional
                            // expression to re-evaluate to decide if we re-enter the statements or not.
                            // The second number in the tuple will be set to the address  of the last
                            // statement in the list as we evaluate the statement list.
                            stack[sp - 1] = (cond_addr.0 as usize, 0);
                            // enter the loop
                            i += 1; // The statement-list expr is here
                            continue;
                        } else {
                            // Jump past the loop's statements
                            i = skip_statements(pool, loop_addr);
                            println!("Finished withfor loop, skip to {}", i);
                            continue;
                        }
                    }
                    _ => panic!("Single for-expression must be boolean."),
                }
            }
            Expr::If(cond, conditional_branch, skip_branch) => {
                match &state[cond.0 as usize] {
                    Value::Bool(c) => {
                        let skip_to = if let Some(skip_addr) = skip_branch {
                            skip_addr
                        } else {
                            conditional_branch
                        };
                        sp += 1;
                        stack[sp - 1] = (skip_to.0 as usize, 0);
                        if *c {
                            // Move into the 'then' branch
                            i += 1;
                            continue;
                        } else {
                            // Skip over the 'then' branch
                            i = conditional_branch.0 as usize;
                            continue;
                        }
                    }
                    _ => panic!("Internal error: 'if' conditional must be a boolean expression."),
                }
            }
            _ => panic!("not implemented"),
        };
        state[i] = result;
        println!("{}: value: {}", i, &state[i]);

        // Did we reach the end of the current scope? The .1 part of the stack frame tuple has the 'finish' address.
        // The .0 part has the caller's address.
        if sp > 0 && i == stack[sp - 1].1 {
            // Restore program pointer
            i = stack[sp - 1].0;
            println!("Restore program pointer to '{}'", i);
            sp -= 1;

            // If the last part of the main program is a statement list, the new program pointer
            // will point past the end of the program, so we're done.
        } else {
            i = i + 1;
            println!("Increment program pointer to '{}'", i);
        }
        if i == pool.size() {
            println!("Program complete.");
            break;
        }
    }
    state[i -1].clone()
}

#[cfg(test)]
mod test {
    use super::super::expression::Expr;
    use super::super::expression::ExprRef;
    use super::super::expression::ExpressionPool;
    use super::super::interpreter::*;
    use super::super::token::*;
    use super::super::types::LangType;
    use super::super::LanguageParser;

    #[test]
    fn test_interpret_simple_stmt() {
        let mut pool = ExpressionPool::default();

        let stmt_list = Expr::StmtList(ExprRef(2), ExprRef(2));
        let int_literal = Expr::LiteralInt(99);
        let output_stmt = Expr::Output(ExprRef(1), LangType::Integer);

        pool.add(stmt_list);
        pool.add(int_literal);
        pool.add(output_stmt);
        assert_eq!(3, pool.size());
        println!("Run program...");
        let result = run(&pool, ExprRef(0));
        assert_eq!("99",&result.to_string()); 
    }

    #[test]
    fn test_let_and_call_stmt() {
        let mut pool = ExpressionPool::default();
        pool.add(Expr::StmtList(ExprRef(2), ExprRef(4)));
        pool.add(Expr::LiteralInt(5));
        pool.add(Expr::Let(ExprRef(1), LangType::Integer));
        pool.add(Expr::Call(ExprRef(2)));
        pool.add(Expr::Output(ExprRef(3), LangType::Integer));
        let result = run(&pool, ExprRef(0));
        assert_eq!("5", &result.to_string());        
    }

    #[test]
    fn test_for_loop() {
        let program = vec![
            let_stmt_tok(),
            ident_tok("a"),
            equals_tok(),
            int_tok(0),
            stmt_terminator_tok(),
            for_stmt_tok(),
            left_paren_tok(),
            ident_tok("a"),
            compare_op_tok(CompareOp::Lt),
            int_tok(8),
            right_paren_tok(),
            left_brace_tok(),
            ident_tok("a"),
            assign_tok(), // :=
            ident_tok("a"),
            op_tok(Op::Add),
            int_tok(1),
            stmt_terminator_tok(),
            output_tok(),
            ident_tok("a"),
            right_brace_tok(),
            eof_tok(),
        ];
        println!("----------------------- for loop test --------------------------------");
        let parsed_code = try_parsing(program);
        let ir = match parsed_code {
            Err(e) => {
                panic!("Failed to parse test program: {}", &e);
            }
            Ok(code) => code,
        };
        let result = run(&ir, ExprRef(0));
        assert_eq!("8",&format!("{}", result));

        
    }

    #[test]
    fn test_if_stmt() {
        let program = vec![
            let_stmt_tok(),
            ident_tok("a"),
            equals_tok(),
            int_tok(0),
            stmt_terminator_tok(),
            if_stmt_tok(),
            left_paren_tok(),
            int_tok(5),
            compare_op_tok(CompareOp::Eq),
            int_tok(8),
            right_paren_tok(),
            left_brace_tok(),
            ident_tok("a"),
            assign_tok(), // :=
            int_tok(-1),
            right_brace_tok(),
            else_tok(),
            left_brace_tok(),
            ident_tok("a"),
            assign_tok(),
            int_tok(1),
            right_brace_tok(),
            eof_tok(),
        ];
        let parsed_code = try_parsing(program);
        let ir = match parsed_code {
            Err(e) => {
                panic!("Failed to parse test program: {}", &e);
            }
            Ok(code) => code,
        };
        let result = run(&ir, ExprRef(0));
        assert!(matches!(result, Value::None));
    }

    #[test]
    fn test_end_to_end_program() {
        let code = program1_tokens();
        let mut language_parser = LanguageParser::new(code);
        let expr_pool = language_parser
            .parse_program()
            .expect("Error during parsing.");
        assert!(expr_pool.size() > 0);
        let result = run(&expr_pool, ExprRef(0));
        assert_eq!("0", &result.to_string());        
    }

    fn program1_tokens() -> Vec<Token> {
        vec![
            output_tok(),
            int_tok(5),
            op_tok(Op::Add),
            int_tok(9),
            op_tok(Op::Mul),
            int_tok(55),
            stmt_terminator_tok(),
            output_tok(),
            int_tok(0),
            op_tok(Op::Mul),
            int_tok(5),
            eof_tok(),
        ]
    }

    fn try_parsing(code: Vec<Token>) -> Result<ExpressionPool, String> {
        let mut language_parser = LanguageParser::new(code);
        assert_eq!(1, language_parser.symbol_table_frame());
        let expr_pool = language_parser.parse_program();

        // TODO Put in real error handling
        if let Some(output) = expr_pool {
            Ok(output)
        } else {
            Err(format!("No results from parsing. Check STDERR."))
        }
    }
}
