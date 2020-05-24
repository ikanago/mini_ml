#[macro_export]
macro_rules! interpret {
    ($code:expr) => {{
        let mut lexer = Lexer::new($code);
        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(err) => panic!(err),
        };
        let mut parser = Parser::new(tokens);
        let vec_ast = match parser.parse() {
            Ok(vec_ast) => vec_ast,
            Err(err) => panic!(err),
        };
        let result = eval(&vec_ast);
        result
    }};
}
