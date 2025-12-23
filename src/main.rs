use std::{env, fs::read_to_string};

use crate::{
    lexer::{Lexer, Token},
    parser::parse,
};

mod lexer;
mod parser;
mod qbe;
mod codegen;

fn main() {
    let source_file = env::args()
        .nth(1)
        .expect("Provide a source file ex: tomoc hello.tomo");
    let source = read_to_string(source_file).expect("Failed to read source file");
    let tokens = Lexer::new(&source).collect::<Vec<Token>>();
    let ast = parse(tokens);
    println!("{:?}", ast);
}
