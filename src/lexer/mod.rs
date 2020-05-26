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
    Let,
    In,
    Equal,
    If,
    Then,
    Else,
    Fun,
    RArrow,
    Rec,
    SemiColon,
}

#[derive(Debug)]
pub enum LexError {}
