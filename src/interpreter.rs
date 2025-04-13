use super::expression::{Expr, ExprRef, ExpressionPool};
use super::token::{CompareOp, LogicalOp, Op};

use std::fmt;

const DEBUG: bool = false;

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
    #[inline(always)]
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

    #[inline(always)]
    pub fn sub(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_sub(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i - *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 - *j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i - *j),
            _ => panic!("Invalid operands for '+'"),
        }
    }
    #[inline(always)]
    pub fn div(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_div(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i / *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 / *j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i / *j),
            _ => panic!("Invalid operands for '+'"),
        }
    }

    #[inline(always)]
    pub fn mul(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_mul(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(*i * *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 * j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(*i * j),
            _ => panic!("Invalid operands for '+'"),
        }
    }
    #[inline(always)]
    pub fn less_than(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i < *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i < *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j > *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i < *j),
            _ => panic!("Invalid operands for '<': {}, {}", &self, &rhs),
        }
    }

    #[inline(always)]
    pub fn equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i == *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i == *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j == *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i == *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    #[inline(always)]
    pub fn not_equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i != *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i != *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j != *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i != *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    #[inline(always)]
    pub fn greater_than(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i > *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i > *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j < *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i > *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    #[inline(always)]
    pub fn greater_than_equal(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Bool(*i >= *j),
            (Value::Flt(i), Value::Int(j)) => Value::Bool(*i >= *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Bool(*j <= *i as f64),
            (Value::Flt(i), Value::Flt(j)) => Value::Bool(*i >= *j),
            _ => panic!("Invalid operands for '<'"),
        }
    }

    #[inline(always)]
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

#[inline(always)]
pub fn skip_statements(pool: &ExpressionPool, expr_ref: &ExprRef) -> usize {
    if expr_ref.0 == 0 {
        panic!("Internal error. statement list address can't be 0.");
    }
    match &pool.exprs[expr_ref.0 as usize] {
        Expr::StmtList(_head, ref tail) => tail.0 as usize,
        _ => panic!("Internal error: Can't skip any expression other than a statement-list."),
    }
}

pub fn run(pool: &ExpressionPool, root: ExprRef) -> Value {
    let mut instruction_counter: u128 = 0;
    let mut state: Vec<Value> = vec![Value::None; pool.size()];
    let mut stack = vec![(0, 0); 100];
    let mut sp = 0;
    if DEBUG {
        pool.debug_dump();
    }

    //for (i, expr) in pool.exprs.iter().enumerate() {
    let mut i: usize = 0;
    loop {
        // If jumping past the end of a statement list and that list
        // happens to be the last thing in the program.
        if i >= pool.size() {
            break;
        }
        instruction_counter += 1;
        let expr = &pool.exprs[i];
        if DEBUG {
            println!("Execute {}: {:?}", i, &expr);
            println!(" ********************************************");
            for (ind, s) in state.iter().enumerate() {
                if ind <= i {
                    println!("{}\t{}", ind, s);
                }
            }
        }
        let result = match expr {
            Expr::Unit => Value::None,
            Expr::Output(value_addr, _) => {
                let value = &state[value_addr.0 as usize];
                println!("{}", value);
                // This is mostly for diagnostics; normally the result of 'output' would be ignored.
                value.clone()
            }
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
                match op {
                    CompareOp::Eq => lhs.equal(rhs),
                    CompareOp::Ne => lhs.not_equal(rhs),
                    CompareOp::Lt => lhs.less_than(rhs),
                    CompareOp::Gt => lhs.greater_than(rhs),
                    CompareOp::Lte => lhs.less_than_equal(rhs),
                    CompareOp::Gte => lhs.greater_than_equal(rhs),
                }
            }
            Expr::Logical(logical_op, lhs, rhs) => {
                match logical_op {
                    LogicalOp::Or => {
                        let lhs_value = &state[lhs.0 as usize];
                        let rhs_value = &state[rhs.0 as usize];
                        if let (Value::Bool(l), Value::Bool(r)) = (lhs_value, rhs_value) {
                            Value::Bool(*l || *r)
                        } else {
                            // TODO turn into internal error (compiler should prevent this situation.)
                            // Runtime error
                            panic!("Logical operators require boolean operands.");
                        }
                    }
                    LogicalOp::And => {
                        if let Value::Bool(l) = &state[lhs.0 as usize] {
                            if *l {
                                // Still need to check if rhs is also true.
                                // move past the 'and'  op expr address and restart evaluation.
                                i += 1;
                                continue;
                            } else {
                                i = rhs.0 as usize;
                                Value::Bool(false)
                            }
                        } else {
                            // Runtime error
                            panic!("The 'and' operator requires boolean operands.");
                        }
                    }
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
                    if DEBUG {
                        println!("Enter statement liststarting at {}, {}", i + 1, finish.0);
                    }
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
                            if DEBUG {
                                println!("Enter for loop");
                            }
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
                            i += 1;
                            if DEBUG {
                                println!("Finished withfor loop, skip to {}", i);
                            }
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
        if DEBUG {
            println!("Value @{}: {}", i, &state[i]);
        }

        // Did we reach the end of the current scope? The .1 part of the stack frame tuple has the 'finish' address.
        // The .0 part has the caller's address.
        if sp > 0 && i == stack[sp - 1].1 {
            // Restore program pointer
            i = stack[sp - 1].0;
            if DEBUG {
                println!("Restore program pointer to '{}'", i);
            }
            sp -= 1;

            // If the last part of the main program is a statement list, the new program pointer
            // will point past the end of the program, so we're done.
        } else {
            i += 1;
            if DEBUG {
                println!("Increment program pointer to '{}'", i);
            }
        }
        if i == pool.size() {
            if DEBUG {
                println!("Program complete.");
            }
            break;
        }
    }
    println!("Total instructions: {}", instruction_counter);
    state[i - 1].clone()
}

#[cfg(test)]
mod test {
    use super::super::expression::Expr;
    use super::super::expression::ExprRef;
    use super::super::expression::ExpressionPool;
    use super::super::interpreter::*;
    use super::super::lex::Scanner;
    use super::super::token::*;
    use super::super::types::LangType;
    use super::super::LanguageParser;

    #[test]
    fn test_interpret_simple_stmt() {
        let mut pool = ExpressionPool::new();

        let stmt_list = Expr::StmtList(ExprRef(2), ExprRef(2));
        let int_literal = Expr::LiteralInt(99);
        let output_stmt = Expr::Output(ExprRef(1), LangType::Integer);

        pool.add(stmt_list);
        pool.add(int_literal);
        pool.add(output_stmt);
        assert_eq!(3, pool.size());
        println!("Run program...");
        let result = run(&pool, ExprRef(0));
        assert_eq!("99", &result.to_string());
    }

    #[test]
    fn test_let_and_call_stmt() {
        let mut pool = ExpressionPool::new();
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
            assign_tok(),
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

        if let Err(ref e) = parsed_code {
            eprintln!("Error was {:?}", e);
        }

        if let Ok(ir) = parsed_code {
            let result = run(&ir, ExprRef(0));
            assert_eq!("8", &format!("{}", result));
        }
    }

    #[test]
    fn test_nested_for_loop() {
        let code = "let x := 1; let y := 1; let outer := 0; let inner := 0;
            for (x < 5) {
                outer := outer + 1;
                for (y < 10) {
                    inner :=inner + 1;
                    y := y + 1;
                };
                x := x + 1
            };
            let final := inner + outer;            
            output final
            ";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.tokenize();
        let mut language_parser = LanguageParser::new(tokens);
        let ir_pool = language_parser.parse_program();
        match ir_pool {
            Err(errors) => {
                for e in errors {
                    eprintln!("{}", e);
                }
            }
            Ok(ir) => {
                // Interpret the intermediate representation.
                let value = super::super::interpreter::run(&ir, ExprRef(0));
                println!("Final value: {}", &value);
                assert!(matches!(value, Value::Int(13)));
            }
        }
    }

    #[test]
    pub fn test_more_logical_and() {
        let code = "let x := 1; let y := 1; let outer := 0; let inner := 0;
            for (x < 5) {
                outer := outer + 1;
                for (y < 10) {
                    inner :=inner + 1;
                    y := y + 1;
                };
                x := x + 1
            };
            let final := inner + outer;            
            output final
            ";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.tokenize();
        let mut language_parser = LanguageParser::new(tokens);
        let ir_pool = language_parser.parse_program();
        match ir_pool {
            Err(errors) => {
                for e in errors {
                    eprintln!("{}", e);
                }
            }
            Ok(ir) => {
                // Interpret the intermediate representation.
                let value = super::super::interpreter::run(&ir, ExprRef(0));
                println!("Final value: {}", &value);
                assert!(matches!(value, Value::Int(13)));
            }
        }
        let code = "let x := 1; let y := 1; 
        let first_and := false;
        if (x >  0 and y > 0) {
            first_and := true;
        };
        let second_and := false;
        if (x > 1 and y>1) {
            second_and :=false;
        };

        for (first_and and x < 25 ) {
            x := x + 1;
        };
        output x

        
        ";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.tokenize();
        let mut language_parser = LanguageParser::new(tokens);
        let ir_pool = language_parser.parse_program();
        match ir_pool {
            Err(ref errors) => {
                for e in errors {
                    eprintln!("{}", e);
                }
            }
            Ok(ref ir) => {
                // Interpret the intermediate representation.
                let value = super::super::interpreter::run(ir, ExprRef(0));
                println!("Final value: {}", &value);
                assert!(matches!(value, Value::Int(25)));
            }
        }
        assert!(ir_pool.is_ok());
    }

    #[test]
    fn test_logical_and_op() {
        println!("test logical AND op ---------------------------------------");
        let program = vec![
            output_tok(),
            int_tok(5),
            compare_op_tok(CompareOp::Eq),
            int_tok(3),
            logical_tok(LogicalOp::And),
            int_tok(3),
            compare_op_tok(CompareOp::Eq),
            int_tok(3),
            eof_tok(),
        ];
        let parsed_code = try_parsing(program);
        assert!(parsed_code.is_ok());
        if let Err(ref e) = parsed_code {
            eprintln!("Error parsing {}", e);
        }
        if let Ok(ref ir) = parsed_code {
            let result = run(ir, ExprRef(0));
            assert!(matches!(result, Value::Bool(false)));
        }

        let program_true = vec![
            output_tok(),
            int_tok(5),
            compare_op_tok(CompareOp::Eq),
            int_tok(5),
            logical_tok(LogicalOp::And),
            int_tok(3),
            compare_op_tok(CompareOp::Eq),
            int_tok(3),
            eof_tok(),
        ];
        let parsed_code = try_parsing(program_true);
        assert!(parsed_code.is_ok());
        if let Err(ref e) = parsed_code {
            eprintln!("Error parsing {}", e);
        }
        if let Ok(ir) = parsed_code {
            ir.debug_dump();
            let result = run(&ir, ExprRef(0));
            println!("result of program_true: {}", result);
            assert!(matches!(result, Value::Bool(true)));
        }
    }

    #[test]
    fn test_if_stmt() {
        let program = vec![
            let_stmt_tok(),
            ident_tok("a"),
            assign_tok(),
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
        assert!(parsed_code.is_ok());

        if let Err(ref e) = parsed_code {
            eprintln!("Error was {:?}", e);
        }
        if let Ok(ref ir) = parsed_code {
            let result = run(ir, ExprRef(0));
            assert!(matches!(result, Value::None));
        }
    }

    #[test]
    fn test_end_to_end_program() {
        println!("In end to end program test");
        let code = program1_tokens();
        let mut language_parser = LanguageParser::new(code);
        let expr_pool = language_parser
            .parse_program()
            .expect("Error during parsing.");
        assert!(expr_pool.size() > 0);
        let result = run(&expr_pool, ExprRef(0));
        assert_eq!("0", &result.to_string());
    }

    #[test]
    fn test_parse_and_interpret() {
        let program = "let a := 2;\n let result := 12;\n a := a + 1; if (a = 2){ result := 99} else {result := 0}; output result";
        let mut s = Scanner::new(program);
        let code = s.tokenize();
        let mut language_parser = LanguageParser::new(code);
        let expr_pool_result = language_parser.parse_program();

        if let Err(ref e) = expr_pool_result {
            eprintln!("Error is {:?}", e);
        }

        assert!(expr_pool_result.is_ok());
        if let Ok(expr_pool) = expr_pool_result {
            assert!(expr_pool.size() > 0);
            let result = run(&expr_pool, ExprRef(0));
            assert_eq!("0", &result.to_string());
        }
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
        match expr_pool {
            Ok(output) => Ok(output),
            Err(e) => Err(e
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")),
        }
    }
}
