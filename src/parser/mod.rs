pub mod parser;
pub mod syntax;
pub mod typing;

#[derive(Debug)]
pub enum ParseError {
    Eof,
    NonTerminalSymbol,
    UnexpectedToken,
    UnclosedParen,
}
