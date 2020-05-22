use mini_ml::eval::eval::Eval;
use mini_ml::lexer::lexer::Lexer;
use mini_ml::parser::parser::Parser;
use mini_ml::parser::syntax::Expr;
use std::collections::HashMap;

fn main() {
    let mut lexer = Lexer::new("if a < b then if a > b then 1 else (a + b) * 4 else 4;;");
    let tokens = lexer.lex().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut environment: HashMap<String, Expr> = HashMap::new();
    environment.insert("a".to_string(), Expr::U64(2));
    environment.insert("b".to_string(), Expr::U64(3));
    let evaluator = Eval::new(environment);
    let result = evaluator.eval(&ast);
    println!("{:?}", result);
}
