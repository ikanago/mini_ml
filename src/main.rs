use mini_ml::lexer;

fn main() {
    let mut lexer = lexer::Lexer::new("if true then 3 else 4;;");
    let tokens = lexer.lex();
    println!("{:?}", tokens);
}