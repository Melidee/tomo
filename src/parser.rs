use crate::lexer::Token;

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
    ConstDef {
        identifier: &'a str,
        type_: Type<'a>,
        expr: Expression<'a>,
    },
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
pub enum Expression<'a> {
    Identifier(&'a str, Vec<&'a str>),
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
            Token::Identifier(id) => Self::parse_identifier(id),
            Token::StringLiteral(lit) => Expression::StringLiteral(lit),
            Token::Number(num) => Expression::NumberLiteral(num),
            _ => panic!("not an expr {:?}", tokens),
        }
    }

    fn parse_identifier(id: &'a str) -> Expression<'a> {
        let mut parts = id.split(".");
        let first = parts.next().unwrap(); // we can safely unwrap here because split always returns at least one element
        let rest = parts.collect();
        Expression::Identifier(first, rest)
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Return(Expression<'a>),
    Assignment(Assignment<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Assignment<'a> {
    declaration: bool,
    identifier: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct Arg<'a> {
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
pub enum Type<'a> {
    Identifier(&'a str),
    Pointer(&'a str),
    Array(&'a str),
}

impl<'a> Type<'a> {
    fn parse(tokens: &[Token<'a>]) -> Self {
        match tokens {
            [Token::Identifier(id)] => Self::Identifier(id),
            [Token::Star, Token::Identifier(id)] => Self::Pointer(id),
            [
                Token::OpenBracket,
                Token::CloseBracket,
                Token::Identifier(id),
            ] => Self::Array(id),
            _ => panic!("unknown type"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===== Use Statements =====
    #[test]
    fn test_simple_use() {
        let source = vec![Token::Use, Token::Identifier("std"), Token::Semicolon];
        let expected = vec![Ast::Use("std")];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_use_with_path() {
        let source = vec![Token::Use, Token::Identifier("std.io"), Token::Semicolon];
        let expected = vec![Ast::Use("std.io")];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_multiple_use_statements() {
        let source = vec![
            Token::Use,
            Token::Identifier("std.io"),
            Token::Semicolon,
            Token::Use,
            Token::Identifier("std.collections"),
            Token::Semicolon,
        ];
        let expected = vec![Ast::Use("std.io"), Ast::Use("std.collections")];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    #[should_panic(expected = "identifier not found after use")]
    fn test_use_without_identifier() {
        let source = vec![Token::Use, Token::Semicolon];
        parse(source);
    }

    #[test]
    #[should_panic(expected = "identifier not found after use")]
    fn test_use_without_semicolon() {
        let source = vec![Token::Use, Token::Identifier("std")];
        parse(source);
    }

    // ===== Function Definitions =====
    #[test]
    fn test_function_no_args_no_body() {
        let source = vec![
            Token::Fn,
            Token::Identifier("foo"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("void"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "foo",
            args: vec![],
            return_type: Type::Identifier("void"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_single_arg() {
        let source = vec![
            Token::Fn,
            Token::Identifier("inc"),
            Token::OpenParen,
            Token::Identifier("x"),
            Token::Colon,
            Token::Identifier("i32"),
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("i32"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "inc",
            args: vec![Arg {
                identifier: "x",
                type_: Type::Identifier("i32"),
            }],
            return_type: Type::Identifier("i32"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_multiple_args() {
        let source = vec![
            Token::Fn,
            Token::Identifier("add"),
            Token::OpenParen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Identifier("i32"),
            Token::Comma,
            Token::Identifier("b"),
            Token::Colon,
            Token::Identifier("i32"),
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("i32"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "add",
            args: vec![
                Arg {
                    identifier: "a",
                    type_: Type::Identifier("i32"),
                },
                Arg {
                    identifier: "b",
                    type_: Type::Identifier("i32"),
                },
            ],
            return_type: Type::Identifier("i32"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_pointer_arg() {
        let source = vec![
            Token::Fn,
            Token::Identifier("deref"),
            Token::OpenParen,
            Token::Identifier("ptr"),
            Token::Colon,
            Token::Star,
            Token::Identifier("i32"),
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("i32"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "deref",
            args: vec![Arg {
                identifier: "ptr",
                type_: Type::Pointer("i32"),
            }],
            return_type: Type::Identifier("i32"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_array_arg() {
        let source = vec![
            Token::Fn,
            Token::Identifier("process"),
            Token::OpenParen,
            Token::Identifier("arr"),
            Token::Colon,
            Token::OpenBracket,
            Token::CloseBracket,
            Token::Identifier("i32"),
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("void"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "process",
            args: vec![Arg {
                identifier: "arr",
                type_: Type::Array("i32"),
            }],
            return_type: Type::Identifier("void"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_pointer_return_type() {
        let source = vec![
            Token::Fn,
            Token::Identifier("get_ptr"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::Star,
            Token::Identifier("i32"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "get_ptr",
            args: vec![],
            return_type: Type::Pointer("i32"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_array_return_type() {
        let source = vec![
            Token::Fn,
            Token::Identifier("get_array"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::OpenBracket,
            Token::CloseBracket,
            Token::Identifier("i32"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "get_array",
            args: vec![],
            return_type: Type::Array("i32"),
            block: vec![],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    // ===== Return Statements =====
    #[test]
    fn test_return_number() {
        let source = vec![Token::Return, Token::Number("42"), Token::Semicolon];
        let expected = vec![Ast::Return(Expression::NumberLiteral("42"))];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_return_identifier() {
        let source = vec![Token::Return, Token::Identifier("x"), Token::Semicolon];
        let expected = vec![Ast::Return(Expression::Identifier("x", vec![]))];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_return_string() {
        let source = vec![
            Token::Return,
            Token::StringLiteral("hello"),
            Token::Semicolon,
        ];
        let expected = vec![Ast::Return(Expression::StringLiteral("hello"))];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_return_dotted_identifier() {
        let source = vec![
            Token::Return,
            Token::Identifier("obj.field"),
            Token::Semicolon,
        ];
        let expected = vec![Ast::Return(Expression::Identifier("obj", vec!["field"]))];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    // ===== Function Calls =====
    #[test]
    fn test_function_call_no_args() {
        let source = vec![
            Token::Identifier("foo"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Semicolon,
        ];
        let expected = vec![Ast::Expression(Expression::FuncCall {
            identifier: "foo",
            args: vec![],
        })];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_call_one_arg() {
        let source = vec![
            Token::Identifier("print"),
            Token::OpenParen,
            Token::StringLiteral("hello"),
            Token::CloseParen,
            Token::Semicolon,
        ];
        let expected = vec![Ast::Expression(Expression::FuncCall {
            identifier: "print",
            args: vec![Expression::StringLiteral("hello")],
        })];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_call_multiple_args() {
        let source = vec![
            Token::Identifier("add"),
            Token::OpenParen,
            Token::Number("1"),
            Token::Comma,
            Token::Number("2"),
            Token::CloseParen,
            Token::Semicolon,
        ];
        let expected = vec![Ast::Expression(Expression::FuncCall {
            identifier: "add",
            args: vec![
                Expression::NumberLiteral("1"),
                Expression::NumberLiteral("2"),
            ],
        })];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_call_mixed_args() {
        let source = vec![
            Token::Identifier("process"),
            Token::OpenParen,
            Token::Identifier("x"),
            Token::Comma,
            Token::Number("42"),
            Token::Comma,
            Token::StringLiteral("test"),
            Token::CloseParen,
            Token::Semicolon,
        ];
        let expected = vec![Ast::Expression(Expression::FuncCall {
            identifier: "process",
            args: vec![
                Expression::Identifier("x", vec![]),
                Expression::NumberLiteral("42"),
                Expression::StringLiteral("test"),
            ],
        })];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_dotted_function_call() {
        let source = vec![
            Token::Identifier("io.println"),
            Token::OpenParen,
            Token::StringLiteral("Hello"),
            Token::CloseParen,
            Token::Semicolon,
        ];
        let expected = vec![Ast::Expression(Expression::FuncCall {
            identifier: "io.println",
            args: vec![Expression::StringLiteral("Hello")],
        })];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    // ===== Expression Parsing =====
    #[test]
    fn test_expression_simple_identifier() {
        let tokens = vec![Token::Identifier("foo")];
        let result = Expression::parse(tokens);
        assert_eq!(Expression::Identifier("foo", vec![]), result);
    }

    #[test]
    fn test_expression_dotted_identifier() {
        let tokens = vec![Token::Identifier("std.io.println")];
        let result = Expression::parse(tokens);
        assert_eq!(Expression::Identifier("std", vec!["io", "println"]), result);
    }

    #[test]
    fn test_expression_string_literal() {
        let tokens = vec![Token::StringLiteral("test")];
        let result = Expression::parse(tokens);
        assert_eq!(Expression::StringLiteral("test"), result);
    }

    #[test]
    fn test_expression_number_literal() {
        let tokens = vec![Token::Number("123")];
        let result = Expression::parse(tokens);
        assert_eq!(Expression::NumberLiteral("123"), result);
    }

    #[test]
    fn test_expression_nested_function_call() {
        let tokens = vec![
            Token::Identifier("outer"),
            Token::OpenParen,
            Token::Identifier("inner"),
            Token::OpenParen,
            Token::Number("1"),
            Token::CloseParen,
            Token::CloseParen,
        ];
        let result = Expression::parse(tokens);
        assert_eq!(
            Expression::FuncCall {
                identifier: "outer",
                args: vec![Expression::FuncCall {
                    identifier: "inner",
                    args: vec![Expression::NumberLiteral("1")],
                }],
            },
            result
        );
    }

    // ===== Type Parsing =====
    #[test]
    fn test_type_simple() {
        let tokens = vec![Token::Identifier("i32")];
        let result = Type::parse(&tokens);
        assert_eq!(Type::Identifier("i32"), result);
    }

    #[test]
    fn test_type_pointer() {
        let tokens = vec![Token::Star, Token::Identifier("i32")];
        let result = Type::parse(&tokens);
        assert_eq!(Type::Pointer("i32"), result);
    }

    #[test]
    fn test_type_array() {
        let tokens = vec![
            Token::OpenBracket,
            Token::CloseBracket,
            Token::Identifier("i32"),
        ];
        let result = Type::parse(&tokens);
        assert_eq!(Type::Array("i32"), result);
    }

    #[test]
    #[should_panic(expected = "unknown type")]
    fn test_type_invalid() {
        let tokens = vec![Token::Semicolon];
        Type::parse(&tokens);
    }

    // ===== Argument Parsing =====
    #[test]
    fn test_args_empty() {
        let tokens: Vec<Token> = vec![];
        let result = Arg::parse_args(&tokens);
        assert_eq!(Vec::<Arg>::new(), result);
    }

    #[test]
    fn test_args_single() {
        let tokens = vec![
            Token::Identifier("x"),
            Token::Colon,
            Token::Identifier("i32"),
        ];
        let result = Arg::parse_args(&tokens);
        assert_eq!(
            vec![Arg {
                identifier: "x",
                type_: Type::Identifier("i32"),
            }],
            result
        );
    }

    #[test]
    fn test_args_multiple() {
        let tokens = vec![
            Token::Identifier("x"),
            Token::Colon,
            Token::Identifier("i32"),
            Token::Comma,
            Token::Identifier("y"),
            Token::Colon,
            Token::Identifier("f64"),
        ];
        let result = Arg::parse_args(&tokens);
        assert_eq!(
            vec![
                Arg {
                    identifier: "x",
                    type_: Type::Identifier("i32"),
                },
                Arg {
                    identifier: "y",
                    type_: Type::Identifier("f64"),
                },
            ],
            result
        );
    }

    #[test]
    fn test_args_with_pointer() {
        let tokens = vec![
            Token::Identifier("ptr"),
            Token::Colon,
            Token::Star,
            Token::Identifier("i32"),
        ];
        let result = Arg::parse_args(&tokens);
        assert_eq!(
            vec![Arg {
                identifier: "ptr",
                type_: Type::Pointer("i32"),
            }],
            result
        );
    }

    #[test]
    fn test_args_with_array() {
        let tokens = vec![
            Token::Identifier("arr"),
            Token::Colon,
            Token::OpenBracket,
            Token::CloseBracket,
            Token::Identifier("i32"),
        ];
        let result = Arg::parse_args(&tokens);
        assert_eq!(
            vec![Arg {
                identifier: "arr",
                type_: Type::Array("i32"),
            }],
            result
        );
    }

    // ===== Complex Integration Tests =====
    #[test]
    fn test_main_function() {
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
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_body() {
        let source = vec![
            Token::Fn,
            Token::Identifier("add"),
            Token::OpenParen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Identifier("i32"),
            Token::Comma,
            Token::Identifier("b"),
            Token::Colon,
            Token::Identifier("i32"),
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("i32"),
            Token::OpenSquirrely,
            Token::Return,
            Token::Identifier("a"),
            Token::Semicolon,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "add",
            args: vec![
                Arg {
                    identifier: "a",
                    type_: Type::Identifier("i32"),
                },
                Arg {
                    identifier: "b",
                    type_: Type::Identifier("i32"),
                },
            ],
            return_type: Type::Identifier("i32"),
            block: vec![Ast::Return(Expression::Identifier("a", vec![]))],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_multiple_functions() {
        let source = vec![
            Token::Fn,
            Token::Identifier("foo"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("void"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
            Token::Fn,
            Token::Identifier("bar"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("void"),
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let expected = vec![
            Ast::FuncDef {
                identifier: "foo",
                args: vec![],
                return_type: Type::Identifier("void"),
                block: vec![],
            },
            Ast::FuncDef {
                identifier: "bar",
                args: vec![],
                return_type: Type::Identifier("void"),
                block: vec![],
            },
        ];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_with_multiple_statements() {
        let source = vec![
            Token::Fn,
            Token::Identifier("test"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Colon,
            Token::Identifier("void"),
            Token::OpenSquirrely,
            Token::Identifier("foo"),
            Token::OpenParen,
            Token::CloseParen,
            Token::Semicolon,
            Token::Identifier("bar"),
            Token::OpenParen,
            Token::Number("1"),
            Token::CloseParen,
            Token::Semicolon,
            Token::Return,
            Token::Number("0"),
            Token::Semicolon,
            Token::CloseSquirrely,
        ];
        let expected = vec![Ast::FuncDef {
            identifier: "test",
            args: vec![],
            return_type: Type::Identifier("void"),
            block: vec![
                Ast::Expression(Expression::FuncCall {
                    identifier: "foo",
                    args: vec![],
                }),
                Ast::Expression(Expression::FuncCall {
                    identifier: "bar",
                    args: vec![Expression::NumberLiteral("1")],
                }),
                Ast::Return(Expression::NumberLiteral("0")),
            ],
        }];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_empty_input() {
        let source: Vec<Token> = vec![];
        let expected: Vec<Ast> = vec![];
        let result = parse(source);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complex_nested_calls() {
        let tokens = vec![
            Token::Identifier("func1"),
            Token::OpenParen,
            Token::Identifier("func2"),
            Token::OpenParen,
            Token::Number("1"),
            Token::Comma,
            Token::Number("2"),
            Token::CloseParen,
            Token::Comma,
            Token::StringLiteral("test"),
            Token::CloseParen,
        ];
        let result = Expression::parse(tokens);
        assert_eq!(
            Expression::FuncCall {
                identifier: "func1",
                args: vec![
                    Expression::FuncCall {
                        identifier: "func2",
                        args: vec![
                            Expression::NumberLiteral("1"),
                            Expression::NumberLiteral("2"),
                        ],
                    },
                    Expression::StringLiteral("test"),
                ],
            },
            result
        );
    }
}
