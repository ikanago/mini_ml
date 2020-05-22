#[derive(Clone, Debug, PartialEq)]
pub enum BinOpKind {
    Plus,
    Mult,
    Lt,
    Gt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var(String),
    U64(u64),
    Bool(bool),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

enum Type {
    U64,
    Bool,
    Var,
    Fun,
    List,
}
