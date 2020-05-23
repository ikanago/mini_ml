#[macro_use]
extern crate clap;

use mini_ml::eval::eval::Eval;
use mini_ml::interpret;
use mini_ml::lexer::lexer::Lexer;
use mini_ml::parser::parser::Parser;
use std::fs::File;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let matches = clap_app!(mini_ml =>
        (version: crate_version!())
        (author: crate_authors!())
        (about: crate_description!())
        (@arg cmd: -c +takes_value "Pass program as string.")
        (@arg file: "Program file.")
    )
    .get_matches();

    if let Some(source_code) = matches.value_of("cmd") {
        let result = interpret!(source_code);
        println!("{:?}", result);
    } else if let Some(file) = matches.value_of("file") {
        let mut source_file = File::open(file)?;
        let mut source_code = String::new();
        source_file.read_to_string(&mut source_code)?;

        let result = interpret!(&source_code);
        println!("{:?}", result);
    }
    Ok(())
}
