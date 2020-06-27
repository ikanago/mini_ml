pub mod eval;
pub mod lexer;
pub mod parser;

use crate::eval::eval::eval;
use crate::eval::ExprVal;
use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::parser::typing::Typer;

/// Interpret source code.
/// If `dump_token` is true, dump lexed tokens, so do `dump_ast`.
pub fn interpret(source_code: &str, dump_token: bool, dump_ast: bool) -> Vec<ExprVal> {
    let mut lexer = Lexer::new(source_code);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{:?}", err);
            panic!()
        }
    };
    if dump_token {
        eprintln!("{:?}", tokens);
    }

    let mut parser = Parser::new(tokens);
    let vec_ast = match parser.parse() {
        Ok(vec_ast) => vec_ast,
        Err(err) => {
            eprintln!("{:?}", err);
            panic!()
        }
    };
    if dump_ast {
        eprintln!("{:?}", vec_ast);
    }

    let mut typer = Typer::new(&vec_ast);
    typer.infer_type().unwrap();

    match eval(&vec_ast) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("{:?}", err);
            panic!()
        }
    }
}
