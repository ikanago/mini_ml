pub mod eval;
use crate::parser::syntax::Expr;
use std::collections::{HashMap, VecDeque};

type Env = HashMap<String, ExprVal>;

#[derive(Clone, Debug, PartialEq)]
pub enum ExprVal {
    I64(i64),
    Bool(bool),
    Array(VecDeque<ExprVal>),
    // argument, body, captured environment
    Closure(String, Box<Expr>, Env),
    // closure name, argument, body, captured environment
    ClosureRec(String, String, Box<Expr>, Env),
}

#[derive(Debug)]
pub enum EvalError {
    NotBound(String),
    PatternsNotExhaustive,
    UnexpectedType(String),
    UnsupportedOperandType(String),
}
