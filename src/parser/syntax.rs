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
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Fun(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
}

// enum Type {
//     U64,
//     Bool,
//     Var,
//     Fun,
//     List,
// }
