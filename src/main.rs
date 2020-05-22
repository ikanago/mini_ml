use mini_ml::eval::eval::Eval;
use mini_ml::lexer::lexer::Lexer;
use mini_ml::parser::parser::Parser;

fn main() {
    let mut lexer = Lexer::new("let a = 3 in let b = 2 in a + b;;");
    let tokens = lexer.lex().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut evaluator = Eval::new();
    let result = evaluator.eval(&ast);
    println!("{:?}", result);
}
