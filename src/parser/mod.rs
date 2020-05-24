pub mod parser;
pub mod syntax;

#[derive(Debug)]
pub enum ParseError {
    Eof,
    NonTerminalSymbol,
    UnexpectedToken,
    UnclosedParen,
}
