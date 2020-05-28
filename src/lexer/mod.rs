pub mod lexer;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Number(u64),
    Identifier(String),
    True,
    False,
    Plus,
    Minus,
    Asterisk,
    Lt,
    Gt,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Let,
    In,
    Equal,
    If,
    Then,
    Else,
    Match,
    With,
    Cons,
    Fun,
    RArrow,
    Rec,
    Semicolon,
    DoubleSemicolon,
    WhiteSpace,
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedToken(String),
    UnexpectedEof,
    NormalEof,
}
