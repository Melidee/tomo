use std::{env, fs::read_to_string};

use crate::{lexer::{Lexer, Token}, parser::parse};

mod lexer;
mod parser;
mod qbe;
mod error;

fn main() {
    let source_file = env::args()
        .nth(1)
        .expect("Provide a source file ex: tomoc hello.tomo");
    let source = read_to_string(source_file).expect("Failed to read source file");
    let tokens = Lexer::new(&source).collect::<Vec<Token>>();
    println!("{}", tokens.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" "));
    let ast = parse(tokens);
    println!("{:?}", ast);
}