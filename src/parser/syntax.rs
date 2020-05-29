use std::collections::VecDeque;

#[derive(Clone, Debug, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Lt,
    Gt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var(String),
    U64(u64),
    Bool(bool),
    Array(VecDeque<Expr>),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    Fun(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Nil,
    Cons(String, String),
    Var(String),
}
