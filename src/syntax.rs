enum BinopKind {
    Plus,
    Mult,
}

enum Expr {
    Var(String),
    I64(i64),
    Bool(bool),
    Binop(BinopKind, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

enum Type {
    I64,
    Bool,
    Var,
    Fun,
    List,
}
