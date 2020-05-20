use mini_ml::lexer;

fn main() {
    let mut lexer = lexer::Lexer::new("123+456*789");
    let tokens = lexer.lex();
    println!("{:?}", tokens);
}