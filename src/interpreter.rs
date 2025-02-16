use super::expression::{Expr, ExprRef, ExpressionPool};
use super::token::{CompareOp, Op};
use super::types::LangType;
use std::fmt;

#[derive(Clone)]
enum Value {
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
            (Value::Flt(i), Value::Int(j)) => Value::Flt(i + (*j as f64)),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 + *j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(i + j),
            (Value::Str(ref s), Value::Str(ref t)) => Value::Str(s.to_owned() + t),
            _ => panic!("Invalid operators for '+'"),
        }
    }

    pub fn sub(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_sub(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(i - *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 - j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(i - j),
            _ => panic!("Invalid operators for '+'"),
        }
    }

    pub fn div(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_div(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(i / *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 / j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(i / j),
            _ => panic!("Invalid operators for '+'"),
        }
    }

    pub fn mul(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Int(i), Value::Int(j)) => Value::Int(i.wrapping_mul(*j)),
            (Value::Flt(i), Value::Int(j)) => Value::Flt(i * *j as f64),
            (Value::Int(i), Value::Flt(j)) => Value::Flt(*i as f64 * j),
            (Value::Flt(i), Value::Flt(j)) => Value::Flt(i * j),
            _ => panic!("Invalid operators for '+'"),
        }
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
        println!("Execute: {:?}", &expr);
        let result = match expr {
            Expr::Output(value_addr, _) => {
                println!("{}", &state[value_addr.0 as usize]);
                Value::None
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
            Expr::LiteralInt(i) => Value::Int(*i),
            Expr::LiteralFloat(f) => Value::Flt(*f),
            Expr::LiteralString(s) => Value::Str(s.clone()),
            Expr::LiteralBool(b) => Value::Bool(*b),
            Expr::Let(assigned_value, _assigned_type) => {
                let initial_value = &state[assigned_value.0 as usize];
                // TODO: You could put a runtime type-check here
                initial_value.clone()
            }
            Expr::Assign(lhs, rhs) => {
                // TODO you could put a runtime type-check here
                state[lhs.0 as usize] = state[rhs.0 as usize].clone();
                Value::None
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
                    stack[sp - 1].1 = finish.0 as usize;
                    i = start.0 as usize;
                } else {
                    i += 1;
                }
                continue;
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
            sp -= 1;
        } else {
            i = i + 1;
            if i == pool.size() {
                break;
            }
        }
    }
    state[root.0 as usize].clone()
}

#[cfg(test)]
mod test {
    use super::*;

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
        assert!(matches!(result, Value::None));
    }
}
