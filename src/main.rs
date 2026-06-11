use std::{env, fs::read_to_string};

use tomo::{error::Error, lexer::{Lexer, Token}, parser::{Parser, TopLevelAst}};


fn main() {
    let source_file = env::args()
        .nth(1)
        .expect("Provide a source file ex: tomoc hello.tomo");
    let source = read_to_string(source_file).expect("Failed to read source file");
    let tokens = Lexer::new(&source);
    println!("{}", tokens.clone().map(|t| t.to_string()).collect::<Vec<String>>().join(" "));
    let mut parser = Parser::from_iter(tokens);
    TopLevelAst::parse(&mut parser).expect("failed to parse source");
}