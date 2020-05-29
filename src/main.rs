#[macro_use]
extern crate clap;

use mini_ml::interpret;
use mini_ml::parser::syntax::Type;
use mini_ml::parser::typing::typing_expression;
use std::fs::File;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let matches = clap_app!(mini_ml =>
        (version: crate_version!())
        (author: crate_authors!())
        (about: crate_description!())
        (@arg cmd: -c +takes_value "Pass program as string.")
        (@arg file: "Program file.")
        (@arg dump_token: --("dump-token") "Dump tokens into stderr.")
        (@arg dump_ast: --("dump-ast") "Dump AST into stderr.")
    )
    .get_matches();

    let dump_token = matches.is_present("dump_token");
    let dump_ast = matches.is_present("dump_ast");

    if let Some(source_code) = matches.value_of("cmd") {
        use mini_ml::lexer::lexer::Lexer;
        use mini_ml::parser::parser::Parser;
        use std::collections::HashMap;
        let mut env: HashMap<String, Type> = HashMap::new();
        env.insert("a".to_string(), Type::TyI64);
        env.insert("b".to_string(), Type::TyI64);
        let mut lexer = Lexer::new(source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let asts = parser.parse().unwrap();
        let infered_type = typing_expression(&asts[0], &mut env);
        // let result = interpret(source_code, dump_token, dump_ast);
        println!("{:?}", infered_type);
    } else if let Some(file) = matches.value_of("file") {
        let mut source_file = File::open(file)?;
        let mut source_code = String::new();
        source_file.read_to_string(&mut source_code)?;

        let result = interpret(&source_code, dump_token, dump_ast);
        println!("{:?}", result);
    }
    Ok(())
}
