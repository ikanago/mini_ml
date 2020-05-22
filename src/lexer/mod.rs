pub mod lexer;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Number(u64),
    Identifier(String),
    True,
    False,
    Plus,
    Asterisk,
    If,
    Then,
    Else,
    SemiColon,
}

#[derive(Debug)]
pub enum LexError {}
