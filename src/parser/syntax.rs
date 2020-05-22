#[derive(Clone, Debug, PartialEq)]
pub enum BinOpKind {
    Plus,
    Mult,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var(String),
    U64(u64),
    Bool(bool),
    Binop(BinOpKind, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

enum Type {
    U64,
    Bool,
    Var,
    Fun,
    List,
}
