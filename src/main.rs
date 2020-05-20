use mini_ml::lexer;

fn main() {
    let mut lexer = lexer::Lexer::new("if+123+456*789+aa3_7f");
    let tokens = lexer.lex();
    println!("{:?}", tokens);
}