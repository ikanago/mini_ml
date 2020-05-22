pub mod eval;

#[derive(Debug)]
pub enum TypeError {
    NotBound(String),
    UnexpectedType(String),
    UnsupportedOperandType(String),
}
