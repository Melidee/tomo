use crate::lexer::{Lexer, Token};

fn parse<'a>(tokens: Vec<Token>) -> Vec<Ast<'a>> {
    todo!()
}

#[derive(Debug, PartialEq)]
enum Ast<'a> {
    Use(&'a str),
    FuncDef {
        name: &'a str,
        args: Vec<Arg<'a>>,
        return_type: &'a str,
        block: Vec<Ast<'a>>,
    },
    Return(Expression<'a>),
    Assignment {
        declaration: bool,
        identifier: &'a str,
        type_: &'a str,
        expr: Expression<'a>,
    },
    Expression(Expression<'a>),
}

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    FuncCall {
        identifier: &'a str,
        args: Vec<Expression<'a>>,
    },
    StringLiteral(&'a str),
    NumberLiteral(&'a str),
}

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    Return(Expression<'a>),
    Assignment(Assignment<'a>),
}

#[derive(Debug, PartialEq)]
struct Assignment<'a> {
    declaration: bool,
    identifier: &'a str,
}

#[derive(Debug, PartialEq)]
struct Arg<'a> {
    identifier: &'a str,
    type_: &'a str,
}

#[cfg(test)]
mod Tests {
    use super::*;

    #[test]
    fn main_ast() {
        let source = vec![
            Token::Use,
            Token::Identifier("std.io"),
            Token::Semicolon,
            Token::Identifier("i32"),
            Token::Identifier("main"),
            Token::OpenParen,
            Token::CloseParen,
            Token::OpenSquirrely,
            Token::Identifier("io.println"),
            Token::OpenParen,
            Token::StringLiteral("Hello, world!"),
            Token::CloseParen,
            Token::Semicolon,
            Token::Return,
            Token::Number("0"),
            Token::Semicolon,
            Token::CloseSquirrely,
        ];
        let expected = vec![
            Ast::Use("std.io"),
            Ast::FuncDef {
                name: "main",
                args: vec![],
                return_type: "i32",
                block: vec![
                    Ast::Expression(Expression::FuncCall {
                        identifier: "io.println",
                        args: vec![Expression::StringLiteral("Hello, World!")],
                    }),
                    Ast::Return(Expression::NumberLiteral("0")),
                ],
            },
        ];
        let result = parse(source);
        assert_eq!(expected, result)
    }
}
