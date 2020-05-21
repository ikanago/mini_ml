use mini_ml::lexer::lexer::Lexer;
use mini_ml::parser::parser::Parser;

fn main() {
    let mut lexer = Lexer::new("if true then if false then 1 else 2 else 4;;");
    let tokens = lexer.lex().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{:?}", ast);
}