use super::expression::{Expr, ExprRef, ExpressionPool};
use super::token::{CompareOp, Op};
use super::types::LangType;

#[derive(Clone)]
enum Value {
    Int(i64),
    Flt(f64),
    Str(String),
    Bool(bool),
    None,
}

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
    for (i, expr) in pool.exprs.iter().enumerate() {
        let result = match expr {
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
            _ => panic!("not implemented"),
        };
        state[i] = result;
    }
    state[root.0 as usize].clone()
}
