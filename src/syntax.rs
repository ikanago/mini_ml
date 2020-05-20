enum BinopKind {
    Plus,
    Mult,
}

enum Expr {
    Var(String),
    U64(u64),
    Bool(bool),
    Binop(BinopKind, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

enum Type {
    U64,
    Bool,
    Var,
    Fun,
    List,
}
