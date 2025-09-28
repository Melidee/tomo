use crate::lexer::{Lexer, Token};

pub fn parse<'a>(source_tokens: Vec<Token<'a>>) -> Vec<Ast<'a>> {
    let mut asts = vec![];
    let mut tokens = source_tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match token {
            Token::Use => {
                if let Some(Token::Identifier(id)) = tokens.next()
                    && Some(Token::Semicolon) == tokens.next()
                {
                    asts.push(Ast::Use(id));
                } else {
                    panic!("identifier not found after use")
                }
            }
            Token::Return => {
                let mut expr = vec![];
                while let Some(token) = tokens.next()
                    && token != Token::Semicolon
                {
                    expr.push(token);
                }
                let expression = Expression::parse(expr);
                asts.push(Ast::Return(expression));
            }
            Token::Fn => {
                let identifier = if let Some(Token::Identifier(id)) = tokens.next() {
                    id
                } else {
                    panic!("Expected identifier")
                };
                if tokens.next() != Some(Token::OpenParen) {
                    panic!("Expected args")
                }
                let mut args_tokens = vec![];
                while let Some(t) = tokens.next()
                    && t != Token::CloseParen
                {
                    args_tokens.push(t);
                }
                let args = Arg::parse_args(&args_tokens);
                if let Some(t) = tokens.next()
                    && t != Token::Colon
                {
                    panic!("Expected type")
                }
                let mut type_tokens = vec![];
                while let Some(t) = tokens.next()
                    && t != Token::OpenSquirrely
                {
                    type_tokens.push(t);
                }
                let return_type = Type::parse(&type_tokens);
                let block_tokens = collect_block(&mut tokens);
                let block = parse(block_tokens);
                asts.push(Ast::FuncDef {
                    identifier,
                    args,
                    return_type,
                    block,
                });
            }
            Token::Identifier(id) if tokens.peek() == Some(&Token::OpenParen) => {
                let mut args_tokens = vec![Token::Identifier(id)];
                while let Some(token) = tokens.next()
                    && token != Token::CloseParen
                {
                    args_tokens.push(token);
                }
                args_tokens.push(Token::CloseParen);
                let func_call = Expression::parse(args_tokens);
                asts.push(Ast::Expression(func_call));
            }
            _ => {}
        }
    }
    return asts;
}

fn collect_block<'a>(iter: &mut impl Iterator<Item = Token<'a>>) -> Vec<Token<'a>> {
    let mut brackets = 0;
    let mut tokens = vec![];
    while let Some(token) = iter.next() {
        match token {
            Token::OpenBracket => brackets += 1,
            Token::CloseBracket => {
                if brackets == 0 {
                    break;
                } else {
                    brackets -= 1
                }
            }
            _ => tokens.push(token),
        }
    }
    return tokens;
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Use(&'a str),
    FuncDef {
        identifier: &'a str,
        args: Vec<Arg<'a>>,
        return_type: Type<'a>,
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
    Identifier(&'a str),
    FuncCall {
        identifier: &'a str,
        args: Vec<Expression<'a>>,
    },
    StringLiteral(&'a str),
    NumberLiteral(&'a str),
}

impl<'a> Expression<'a> {
    fn parse(tokens: Vec<Token<'a>>) -> Self {
        match tokens[0] {
            Token::Identifier(id) if tokens.get(1) == Some(&Token::OpenParen) => {
                let mut args_tokens: Vec<Vec<Token<'_>>> = vec![];
                for token in tokens.into_iter().skip(2) {
                    if token == Token::CloseParen {
                        break;
                    } else if token == Token::Comma {
                        args_tokens.push(vec![]);
                    } else {
                        if args_tokens.is_empty() {
                            args_tokens.push(vec![]);
                        }
                        let idx = args_tokens.len() - 1;
                        args_tokens[idx].push(token);
                    }
                }
                let args = args_tokens
                    .into_iter()
                    .map(|arg_tokens| Expression::parse(arg_tokens))
                    .collect();
                
                Self::FuncCall {
                    identifier: id,
                    args,
                }
            }
            Token::Identifier(id) => Expression::Identifier(id),
            Token::StringLiteral(lit) => Expression::StringLiteral(lit),
            Token::Number(num) => Expression::NumberLiteral(num),
            _ => panic!("not an expr {:?}", tokens),
        }
    }
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
    type_: Type<'a>,
}

impl<'a> Arg<'a> {
    fn parse_args(tokens: &[Token<'a>]) -> Vec<Self> {
        if tokens.len() == 0 {
            return vec![];
        }

        let mut args = vec![];
        for pair in tokens.split(|t| t == &Token::Comma) {
            args.push(Self::parse_pair(pair));
        }
        args
    }

    fn parse_pair(tokens: &[Token<'a>]) -> Self {
        let mut parts = tokens.split(|t| *t == Token::Colon);
        let identifier = if let Some([Token::Identifier(id)]) = parts.next() {
            id
        } else {
            panic!()
        };

        let type_ = Type::parse(parts.next().expect("expected type"));
        if parts.next().is_some() {
            panic!("failed to parse arg")
        }
        Self { identifier, type_ }
    }
}

#[derive(Debug, PartialEq)]
enum Type<'a> {
    Identifier(&'a str),
    Pointer(&'a str),
    Array(&'a str),
}

impl<'a> Type<'a> {
    fn parse(tokens: &[Token<'a>]) -> Self {
        match tokens {
            &[Token::Identifier(id)] => Self::Identifier(id),
            &[Token::Star, Token::Identifier(id)] => Self::Pointer(id),
            &[
                Token::OpenBracket,
                Token::CloseBracket,
                Token::Identifier(id),
            ] => Self::Array(id),
            _ => panic!("unknown type"),
        }
    }
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
            Token::Fn,
            Token::Identifier("main"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("i32"),
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
                identifier: "main",
                args: vec![],
                return_type: Type::Identifier("i32"),
                block: vec![
                    Ast::Expression(Expression::FuncCall {
                        identifier: "io.println",
                        args: vec![Expression::StringLiteral("Hello, world!")],
                    }),
                    Ast::Return(Expression::NumberLiteral("0")),
                ],
            },
        ];
        let result = parse(source);
        assert_eq!(expected, result)
    }
}
