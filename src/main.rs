use mini_ml::lexer;
use mini_ml::parser;

fn main() {
    let mut lexer = lexer::Lexer::new("if true then if false then 1 else 2 else 4;;");
    let tokens = lexer.lex().unwrap();
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse();
    println!("{:?}", ast);
}