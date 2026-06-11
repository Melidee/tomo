use peeking_take_while::PeekableExt;
use std::{iter::Peekable, process::ExitCode};

use crate::{
    error::{Error, Result},
    lexer::Token,
    parser::ExpressionPart::Operand,
};

pub struct Parser<'a> {
    tokens: Peekable<Box<dyn Iterator<Item = Token<'a>> + 'a>>,
}

impl<'a> Parser<'a> {
    pub fn from_iter<I: Iterator<Item = Token<'a>> + 'a>(iter: I) -> Self {
        let boxed: Box<dyn Iterator<Item = Token<'a>> + 'a> = Box::new(iter);
        Self {
            tokens: boxed.peekable(),
        }
    }

    /// Peeks the next token in the parser, returning an UnexpectedEndOfInput
    /// error if one is not found.
    fn peek_expect(&mut self) -> Result<&Token<'a>> {
        self.tokens
            .peek()
            .ok_or(Error::UnexpectedEndOfInput("token".to_string()))
    }

    /// Takes the next token in the parser, returning an UnexpectedEndOfInput
    /// error if one is not found.
    fn next_expect(&mut self) -> Result<Token<'a>> {
        self.tokens
            .next()
            .ok_or(Error::UnexpectedEndOfInput("token".to_string()))
    }

    /// Expects a certain symbol to come next in the parser tokens, returning an
    /// error if there are no more tokens, or if the found token does not match
    /// the expected symbol. This consumes the next value of the iterator.
    ///
    /// ### Examples
    ///
    /// ```
    /// use tomo::{lexer::Token, error::Error, parser::Parser};
    ///
    /// let mut parser = Parser::from_iter(vec![Token::OpenParen, Token::CloseParen].into_iter());
    ///
    /// assert_eq!(parser.expect_symbol(Token::OpenParen).unwrap(), ());
    /// assert_eq!(
    ///     parser.expect_symbol(Token::Comma).unwrap_err(),
    ///     Error::UnexpectedToken(Token::Comma.to_string(), Token::CloseParen.to_string())
    /// );
    /// ```
    pub fn expect_symbol(&mut self, symbol: Token<'a>) -> Result<()> {
        let next_expect = self
            .tokens
            .next()
            .ok_or(Error::UnexpectedEndOfInput(symbol.to_string()))?;
        if symbol == next_expect {
            Ok(())
        } else {
            Err(Error::UnexpectedToken(
                symbol.to_string(),
                next_expect.to_string(),
            ))
        }
    }

    /// Expects the next token of the parser to be an identifier, returning an
    /// error if there are no more tokens, or if the found token is not an
    /// identifier. This consumes the next value of the iterator. The returned
    /// type is a parser::Identifier, as that is usually what a Token::Identifier
    /// will be converted to.
    ///
    /// ### Examples
    ///
    /// ```
    /// use tomo::{lexer::Token, error::Error, parser::Parser};
    ///
    /// let mut parser = Parser::from_iter(vec![Token::OpenParen, Token::CloseParen].into_iter());
    ///
    /// assert_eq!(parser.expect_symbol(Token::OpenParen).unwrap(), ());
    /// assert_eq!(
    ///     parser.expect_symbol(Token::Comma).unwrap_err(),
    ///     Error::UnexpectedToken(Token::Comma.to_string(), Token::CloseParen.to_string())
    /// );
    /// ```
    pub fn expect_identifier(&mut self) -> Result<Identifier<'a>> {
        match self.next_expect()? {
            Token::Identifier(id) => Ok(Identifier(id)),
            found => Err(Error::UnexpectedToken(
                "identifier".to_string(),
                found.to_string(),
            )),
        }
    }

    /// Consumes tokens into a new parser until peeking a specific token. This
    /// does *not consume* the separating token.
    ///
    /// ### Examples
    ///
    /// ```
    /// use tomo::{lexer::Token, error::Error, parser::Parser};
    ///
    /// let mut parser = Parser::from_iter(vec![
    ///     Token::Identifier("x"),
    ///     Token::Plus,
    ///     Token::Identifier("y"),
    ///     Token::Semicolon,
    ///     Token::Identifier("z"),
    /// ].into_iter());
    ///
    /// let mut  expr_tokens = parser.collect_until(Token::Semicolon);
    /// // collected tokens contain the symbols before the semicolon
    /// expr_tokens.expect_identifier().expect("next is identifier");
    /// expr_tokens.expect_symbol(Token::Plus).expect("next is semicolon");
    /// expr_tokens.expect_identifier().expect("next is identifier");
    ///
    /// // original parser contains tokens after the separator
    /// parser.expect_symbol(Token::Semicolon).expect("next is semicolon");
    /// parser.expect_identifier().expect("next is identifier");
    /// ```
    pub fn collect_until(&mut self, token: Token<'a>) -> Parser<'a> {
        let collected = self
            .tokens
            .by_ref()
            .peeking_take_while(move |t| *t != token)
            .collect::<Vec<Token<'a>>>();
        let boxed: Box<dyn Iterator<Item = Token<'a>> + 'a> = Box::new(collected.into_iter());
        Self {
            tokens: boxed.peekable(),
        }
    }

    fn collect_until_closing(&mut self, closing_token: Token<'a>) -> Parser<'a> {
        let opening_token = match closing_token {
            Token::CloseBracket => Token::OpenBracket,
            Token::CloseParen => Token::OpenParen,
            Token::CloseSquirrely => Token::OpenSquirrely,
            _ => panic!(
                "collect_until_closing is only for symbols with a matching closing pair, use collect_until instead"
            ),
        };
        let mut depth = 1;
        let collected = self
            .tokens
            .by_ref()
            .peeking_take_while(move |t| {
                if t == &opening_token {
                    depth += 1;
                } else if *t == closing_token {
                    depth -= 1;
                }
                depth != 0
            })
            .collect::<Vec<Token<'a>>>();
        let boxed: Box<dyn Iterator<Item = Token<'a>> + 'a> = Box::new(collected.into_iter());
        Self {
            tokens: boxed.peekable(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TopLevelAst<'a> {
    Module(Identifier<'a>),
    Use(Identifier<'a>),
    Const(Const<'a>),
    Fn(Function<'a>),
}

impl<'a> TopLevelAst<'a> {
    pub fn parse(parser: &mut Parser<'a>) -> Result<TopLevelAst<'a>> {
        match parser.peek_expect()? {
            Token::Module => {
                parser.expect_symbol(Token::Module)?;
                let id = parser.expect_identifier()?;
                parser.expect_symbol(Token::Semicolon)?;
                Ok(TopLevelAst::Module(id))
            }
            Token::Use => {
                parser.expect_symbol(Token::Use)?;
                let id = parser.expect_identifier()?;
                parser.expect_symbol(Token::Semicolon)?;
                Ok(TopLevelAst::Use(id))
            }
            Token::Const => Ok(TopLevelAst::Const(Const::parse(parser)?)),
            Token::Fn => Ok(TopLevelAst::Fn(Function::parse(parser)?)),
            found => Err(Error::UnexpectedToken(
                "one of `module`, `use`, `const`, or `fn`".to_string(),
                found.to_string(),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'a>(&'a str);

#[derive(Debug, PartialEq)]
pub struct Const<'a> {
    identifier: Identifier<'a>,
    type_: Type<'a>,
    expression: Expression<'a>,
}

impl<'a> Const<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self> {
        parser.expect_symbol(Token::Const)?;
        let id = parser.expect_identifier()?;
        parser.expect_symbol(Token::Colon)?;
        let mut type_tokens = parser.collect_until(Token::Equal);
        let type_ = Type::parse(&mut type_tokens).map_err(|err| match parser.next_expect() {
            Ok(found) => err.swap_found(found.to_string()),
            Err(error) => error,
        })?;
        parser.expect_symbol(Token::Equal)?;
        let mut expr_tokens = parser.collect_until(Token::Semicolon);
        parser.expect_symbol(Token::Semicolon)?;
        let expression = Expression::parse(&mut expr_tokens)?;
        Ok(Const {
            identifier: id,
            type_,
            expression,
        })
    }
}

#[derive(Debug, PartialEq)]
struct Function<'a> {
    pub identifier: Identifier<'a>,
    pub args: Vec<DefArg<'a>>,
    pub return_type: Type<'a>,
    pub block: Block<'a>,
}

impl<'a> Function<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self> {
        parser.expect_symbol(Token::Fn)?;
        let identifier = parser.expect_identifier()?;
        let args = DefArg::parse_args(parser)?;
        parser.expect_symbol(Token::Colon)?;
        let mut type_tokens = parser.collect_until(Token::OpenSquirrely);
        let return_type = Type::parse(&mut type_tokens)?;
        parser.expect_symbol(Token::OpenSquirrely)?;
        let mut block_tokens = parser.collect_until(Token::CloseSquirrely);
        let block = Block::parse_body(&mut block_tokens)?;
        parser.expect_symbol(Token::CloseSquirrely)?;
        Ok(Self {
            identifier,
            args,
            return_type,
            block,
        })
    }
}

#[derive(Debug, PartialEq)]
struct DefArg<'a> {
    pub identifier: Identifier<'a>,
    pub type_: Type<'a>,
}

impl<'a> DefArg<'a> {
    fn parse_args(parser: &mut Parser<'a>) -> Result<Vec<Self>> {
        parser.expect_symbol(Token::OpenParen)?;
        let mut args_tokens = parser.collect_until_closing(Token::CloseParen);
        let mut args = vec![];
        while args_tokens.tokens.peek().is_some() {
            let mut arg_tokens = args_tokens.collect_until(Token::Comma);
            args.push(Self::parse(&mut arg_tokens));
        }
        parser.expect_symbol(Token::CloseParen)?;
        args.into_iter().collect() // Vec<Result<T>> -> Result<Vec<T>>
    }

    fn parse(parser: &mut Parser<'a>) -> Result<Self> {
        let identifier = parser.expect_identifier()?;
        parser.expect_symbol(Token::Colon)?;
        let type_ = Type::parse(parser)?;
        Ok(Self { identifier, type_ })
    }
}

#[derive(Debug, PartialEq)]
struct Block<'a> {
    statements: Vec<Statement<'a>>,
}

impl<'a> Block<'a> {
    fn parse_body(parser: &mut Parser<'a>) -> Result<Self> {
        let mut statements = vec![];
        while parser.tokens.peek().is_some() {
            statements.push(Statement::parse(parser)?);
        }
        Ok(Self { statements })
    }
}

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    Expression(Expression<'a>),
    Return(Expression<'a>),
}

impl<'a> Statement<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self> {
        let mut statement_tokens = parser.collect_until(Token::Semicolon);
        let statement = match statement_tokens.peek_expect()? {
            Token::Return => {
                statement_tokens.expect_symbol(Token::Return)?;
                Ok(Self::Return(Expression::parse(&mut statement_tokens)?))
            }
            Token::StringLiteral(_) | Token::Number(_) | Token::Identifier(_) => {
                Ok(Self::Expression(Expression::parse(&mut statement_tokens)?))
            }
            found => Err(Error::unexpected("statement", found.to_string())),
        };
        parser.expect_symbol(Token::Semicolon)?;
        statement
    }
}

#[derive(Debug, PartialEq)]
pub enum Type<'a> {
    Identifier(Identifier<'a>),
}

impl<'a> Type<'a> {
    fn parse(tokens: &mut Parser<'a>) -> Result<Self> {
        match tokens
            .next_expect()
            .map_err(|_| Error::unexpected("type", ""))?
        {
            Token::Identifier(id) => Ok(Self::Identifier(Identifier(id))),
            other => Err(Error::unexpected("type", other.to_string())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    Call(Identifier<'a>, Vec<Expression<'a>>),
    StringLiteral(&'a str),
    IntegerLiteral(u64),
    FloatLiteral(f64),
    Prefix(Operator, Box<Expression<'a>>),
    Infix(Operator, Box<Expression<'a>>),
}

impl<'a> Expression<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self> {
        let mut tokens = vec![];
        while let Some(token) = parser.tokens.next() {
            tokens.push(token);
        }
        match parser.peek_expect()? {
            Token::Identifier(_) => {
                let id = parser.expect_identifier()?;
                Ok(match parser.tokens.peek() {
                    Some(Token::OpenParen) => Self::parse_call(parser)?,
                    None => Self::Identifier(id),
                    Some(_) => panic!("invalid token"),
                })
            }
            Token::Number(num) => Self::parse_number(num),
            Token::StringLiteral(string) => Ok(Self::StringLiteral(string)),
            expr => panic!("not an expression {}", expr),
        }
    }

    fn parse_operators(tokens: &[Token<'a>]) -> Result<Self> {
        tokens.split(|t| matches!(t, Token::Plus | Token::Minus | Token::Star | Token::Slash));
        todo!()
    }

    fn parse_call(parser: &mut Parser<'a>) -> Result<Self> {
        todo!()
    }

    fn parse_number(num: &str) -> Result<Self> {
        if num.contains('.') {
            let float = num.parse::<f64>()?;
            Ok(Self::FloatLiteral(float))
        } else {
            let int = num.parse::<u64>()?;
            Ok(Self::IntegerLiteral(int))
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionPart<'a> {
    OpenParen,
    CloseParen,
    Operator(Operator),
    Operand(Vec<Token<'a>>),
}

impl<'a> ExpressionPart<'a> {
    fn split_expression(expr_parser: &mut Parser<'a>) -> Result<Vec<Self>> {
        let mut parts = vec![];
        while let Some(token) = expr_parser.tokens.peek() {
            // check if the last token of the last expression part is an identifier
            let last_is_id = matches!(
                parts.last(),
                Some(ExpressionPart::Operand(tokens))
                    if matches!(tokens.last(), Some(Token::Identifier(_)))
            );
            match token {
                // if the opening paren is the args to a function or method call
                Token::OpenParen if last_is_id => {
                    expr_parser.tokens.next(); // consume openparen
                    let mut args_tokens = expr_parser
                        .collect_until_closing(Token::CloseParen)
                        .tokens
                        .collect::<Vec<Token>>();
                    expr_parser.tokens.next(); // consume closeparen
                    args_tokens.insert(0, Token::OpenParen);
                    args_tokens.push(Token::CloseParen);
                    if let Some(ExpressionPart::Operand(last_part)) = parts.last_mut() {
                        last_part.extend(args_tokens);
                    }
                }
                Token::OpenParen | Token::CloseParen => {
                    parts.push(match token {
                        Token::OpenParen => Self::OpenParen,
                        Token::CloseParen => Self::CloseParen,
                        _ => unreachable!(),
                    });
                    expr_parser.tokens.next(); // consume paren token
                }
                _ if Operator::from_token(token).is_ok() => {
                    let operator = Operator::from_token(token)?;
                    parts.push(Self::Operator(operator));
                    expr_parser.tokens.next(); // consume operator token
                }
                _ => {
                    let operand_tokens = expr_parser
                        .tokens
                        .by_ref()
                        .peeking_take_while(|t| {
                            Operator::from_token(t).is_err()
                                && !matches!(t, Token::OpenParen | Token::CloseParen)
                        })
                        .collect::<Vec<Token>>();
                    parts.push(Self::Operand(operand_tokens));
                }
            }
        }
        Ok(parts)
    }
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    DoubleEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

impl Operator {
    fn from_token<'a>(token: &Token<'a>) -> Result<Self> {
        match token {
            Token::Plus => Ok(Self::Plus),
            Token::Minus => Ok(Self::Minus),
            Token::Star => Ok(Self::Star),
            Token::Slash => Ok(Self::Slash),
            other => Err(Error::unexpected("operator", other.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::lexer::Token;

    #[test]
    fn parse_module_parses_simple_name() {
        let mut parser = Parser::from_iter(
            vec![Token::Module, Token::Identifier("hello"), Token::Semicolon].into_iter(),
        );

        let result = TopLevelAst::parse(&mut parser);
        let expected = Ok(TopLevelAst::Module(Identifier("hello")));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_module_parses_complex_identifier() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Module,
                Token::Identifier("hello.world_here"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let result = TopLevelAst::parse(&mut parser);
        let expected = Ok(TopLevelAst::Module(Identifier("hello.world_here")));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_module_fails_no_identifier() {
        let mut parser = Parser::from_iter(vec![Token::Module, Token::Semicolon].into_iter());

        let result = TopLevelAst::parse(&mut parser);
        let expected = Err(Error::UnexpectedToken(
            "identifier".to_string(),
            Token::Semicolon.to_string(),
        ));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_module_fails_no_semicolon_with_eoi() {
        let mut parser =
            Parser::from_iter(vec![Token::Module, Token::Identifier("hello")].into_iter());

        let result = TopLevelAst::parse(&mut parser);
        let expected = Err(Error::UnexpectedEndOfInput(Token::Semicolon.to_string()));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_use_parses_simple_name() {
        let mut parser = Parser::from_iter(
            vec![Token::Use, Token::Identifier("hello"), Token::Semicolon].into_iter(),
        );

        let result = TopLevelAst::parse(&mut parser);
        let expected = Ok(TopLevelAst::Use(Identifier("hello")));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_use_parses_complex_identifier() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Use,
                Token::Identifier("hello.world_here"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let result = TopLevelAst::parse(&mut parser);
        let expected = Ok(TopLevelAst::Use(Identifier("hello.world_here")));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_use_fails_no_identifier() {
        let mut parser = Parser::from_iter(vec![Token::Use, Token::Semicolon].into_iter());

        let result = TopLevelAst::parse(&mut parser);
        let expected = Err(Error::UnexpectedToken(
            "identifier".to_string(),
            Token::Semicolon.to_string(),
        ));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_use_fails_no_semicolon_with_eoi() {
        let mut parser =
            Parser::from_iter(vec![Token::Use, Token::Identifier("hello")].into_iter());

        let result = TopLevelAst::parse(&mut parser);
        let expected = Err(Error::UnexpectedEndOfInput(Token::Semicolon.to_string()));
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_const_expr_happy_path() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Identifier("pi"),
                Token::Colon,
                Token::Identifier("f64"),
                Token::Equal,
                Token::Number("3.14"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let expected = Ok(Const {
            identifier: Identifier("pi"),
            type_: Type::Identifier(Identifier("f64")),
            expression: Expression::FloatLiteral(3.14),
        });
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_const_fails_no_identifier() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Colon,
                Token::Identifier("f64"),
                Token::Equal,
                Token::Number("3.14"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let expected = Err(Error::UnexpectedToken(
            "identifier".to_string(),
            Token::Colon.to_string(),
        ));
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_const_fails_no_type_colon() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Identifier("pi"),
                Token::Identifier("f64"),
                Token::Equal,
                Token::Number("3.14"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let expected = Err(Error::UnexpectedToken(
            Token::Colon.to_string(),
            Token::Identifier("f64").to_string(),
        ));
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_const_fails_no_type() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Identifier("pi"),
                Token::Colon,
                Token::Equal,
                Token::Number("3.14"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let expected = Err(Error::UnexpectedToken(
            "type".to_string(),
            Token::Equal.to_string(),
        ));
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_const_fails_no_equal() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Identifier("pi"),
                Token::Colon,
                Token::Identifier("f64"),
                Token::Number("3.14"),
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let expected = Err(Error::UnexpectedEndOfInput(Token::Equal.to_string()));
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_const_fails_no_expression() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Identifier("pi"),
                Token::Colon,
                Token::Identifier("f64"),
                Token::Equal,
                Token::Semicolon,
            ]
            .into_iter(),
        );

        let expected = Err(Error::UnexpectedEndOfInput("token".to_string()));
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_const_fails_no_semicolon() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Const,
                Token::Identifier("pi"),
                Token::Colon,
                Token::Identifier("f64"),
                Token::Equal,
                Token::Number("3.14"),
            ]
            .into_iter(),
        );

        let expected = Err(Error::UnexpectedEndOfInput(Token::Semicolon.to_string()));
        let result = Const::parse(&mut parser);
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_function_happy_path() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Fn,
                Token::Identifier("my_func"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("i32"),
                Token::CloseParen,
                Token::Colon,
                Token::Identifier("i32"),
                Token::OpenSquirrely,
                Token::Return,
                Token::Identifier("x"),
                Token::Semicolon,
                Token::CloseSquirrely,
            ]
            .into_iter(),
        );

        let expected = Ok(Function {
            identifier: Identifier("my_func"),
            args: vec![DefArg {
                identifier: Identifier("x"),
                type_: Type::Identifier(Identifier("i32")),
            }],
            return_type: Type::Identifier(Identifier("i32")),
            block: Block {
                statements: vec![Statement::Return(Expression::Identifier(Identifier("x")))],
            },
        });
        let result = Function::parse(&mut parser);
        assert_eq!(expected, result)
    }

    #[test]
    fn split_expression_empty() {
        let mut parser = Parser::from_iter(vec![].into_iter());
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn split_expression_single_identifier() {
        let mut parser = Parser::from_iter(vec![Token::Identifier("x")].into_iter());
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Identifier("x")]));
    }

    #[test]
    fn split_expression_single_number() {
        let mut parser = Parser::from_iter(vec![Token::Number("42")].into_iter());
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Number("42")]));
    }

    #[test]
    fn split_expression_simple_addition() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("a"),
                Token::Plus,
                Token::Identifier("b"),
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Identifier("a")]));
        assert_eq!(result[1], ExpressionPart::Operator(Operator::Plus));
        assert_eq!(result[2], ExpressionPart::Operand(vec![Token::Identifier("b")]));
    }

    #[test]
    fn split_expression_multiple_operators() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("a"),
                Token::Plus,
                Token::Identifier("b"),
                Token::Star,
                Token::Identifier("c"),
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 5);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Identifier("a")]));
        assert_eq!(result[1], ExpressionPart::Operator(Operator::Plus));
        assert_eq!(result[2], ExpressionPart::Operand(vec![Token::Identifier("b")]));
        assert_eq!(result[3], ExpressionPart::Operator(Operator::Star));
        assert_eq!(result[4], ExpressionPart::Operand(vec![Token::Identifier("c")]));
    }

    #[test]
    fn split_expression_function_call() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(
            result[0],
            ExpressionPart::Operand(vec![
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
            ])
        );
    }

    #[test]
    fn split_expression_function_call_with_multiple_args() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("add"),
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Comma,
                Token::Identifier("b"),
                Token::CloseParen,
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        println!("{:#?}", result);
        assert_eq!(
            result[0],
            ExpressionPart::Operand(vec![
                Token::Identifier("add"),
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Comma,
                Token::Identifier("b"),
                Token::CloseParen,
            ])
        );
    }

    #[test]
    fn split_expression_parenthesized_group() {
        let mut parser = Parser::from_iter(
            vec![
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Plus,
                Token::Identifier("b"),
                Token::CloseParen,
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 5);
        assert_eq!(result[0], ExpressionPart::OpenParen);
        assert_eq!(result[1], ExpressionPart::Operand(vec![Token::Identifier("a")]));
        assert_eq!(result[2], ExpressionPart::Operator(Operator::Plus));
        assert_eq!(result[3], ExpressionPart::Operand(vec![Token::Identifier("b")]));
        assert_eq!(result[4], ExpressionPart::CloseParen);
    }

    #[test]
    fn split_expression_call_with_operators_around() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::Number("1"),
                Token::CloseParen,
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Identifier("x")]));
        assert_eq!(result[1], ExpressionPart::Operator(Operator::Plus));
        assert_eq!(
            result[2],
            ExpressionPart::Operand(vec![
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::Number("1"),
                Token::CloseParen,
            ])
        );
    }

    #[test]
    fn split_expression_nested_parens() {
        let mut parser = Parser::from_iter(
            vec![
                Token::OpenParen,
                Token::OpenParen,
                Token::Identifier("a"),
                Token::CloseParen,
                Token::CloseParen,
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 5);
        assert_eq!(result[0], ExpressionPart::OpenParen);
        assert_eq!(result[1], ExpressionPart::OpenParen);
        assert_eq!(result[2], ExpressionPart::Operand(vec![Token::Identifier("a")]));
        assert_eq!(result[3], ExpressionPart::CloseParen);
        assert_eq!(result[4], ExpressionPart::CloseParen);
    }

    #[test]
    fn split_expression_subtraction() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Number("10"),
                Token::Minus,
                Token::Number("5"),
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Number("10")]));
        assert_eq!(result[1], ExpressionPart::Operator(Operator::Minus));
        assert_eq!(result[2], ExpressionPart::Operand(vec![Token::Number("5")]));
    }

    #[test]
    fn split_expression_division() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("x"),
                Token::Slash,
                Token::Number("2"),
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], ExpressionPart::Operand(vec![Token::Identifier("x")]));
        assert_eq!(result[1], ExpressionPart::Operator(Operator::Slash));
        assert_eq!(result[2], ExpressionPart::Operand(vec![Token::Number("2")]));
    }

    #[test]
    fn split_expression_chained_calls() {
        let mut parser = Parser::from_iter(
            vec![
                Token::Identifier("obj.method"),
                Token::OpenParen,
                Token::CloseParen,
            ]
            .into_iter(),
        );
        let result = ExpressionPart::split_expression(&mut parser).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(
            result[0],
            ExpressionPart::Operand(vec![
                Token::Identifier("obj.method"),
                Token::OpenParen,
                Token::CloseParen,
            ])
        );
    }
}
