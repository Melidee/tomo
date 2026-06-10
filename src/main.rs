use std::{env, fs::read_to_string};

use crate::{error::Error, lexer::{Lexer, Token}, parser::Parser};

mod lexer;
mod parser;
mod qbe;
mod error;

fn main() {
    let mut parser = Parser::from_iter(vec![Token::OpenParen, Token::CloseParen].into_iter());
    
    assert_eq!(parser.expect_symbol(Token::OpenParen).unwrap(), ());
    assert_eq!(parser.expect_symbol(Token::Comma).unwrap_err(), Error::UnexpectedToken(Token::Comma.to_string(), Token::CloseParen.to_string()));
    return;

    
    let source_file = env::args()
        .nth(1)
        .expect("Provide a source file ex: tomoc hello.tomo");
    let source = read_to_string(source_file).expect("Failed to read source file");
    let tokens = Lexer::new(&source).collect::<Vec<Token>>();
    println!("{}", tokens.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" "));
}