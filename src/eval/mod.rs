pub mod eval;
use crate::parser::syntax::Expr;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ExprVal {
    U64(u64),
    Bool(bool),
    ProcV(String, Box<Expr>, HashMap<String, ExprVal>),
}

#[derive(Debug)]
pub enum EvalError {
    NotBound(String),
    UnexpectedType(String),
    UnsupportedOperandType(String),
}
