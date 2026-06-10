use peeking_take_while::PeekableExt;
use std::iter::Peekable;

use crate::{
    error::{Error, Result},
    lexer::Token,
};

pub struct Parser<'a> {
    tokens: Peekable<Box<dyn Iterator<Item = Token<'a>> + 'a>>,
}

impl<'a> Parser<'a> {
    pub fn from_iter<I: Iterator<Item = Token<'a>> + 'static>(iter: I) -> Self {
        let boxed: Box<dyn Iterator<Item = Token<'a>> + 'a> = Box::new(iter);
        Self {
            tokens: boxed.peekable(),
        }
    }

    pub fn parse_top_level(&mut self) -> Result<TopLevelAst<'a>> {
        match self.peek_expect()? {
            Token::Module => {
                self.expect_symbol(Token::Module)?;
                let id = self.expect_identifier()?;
                self.expect_symbol(Token::Semicolon)?;
                Ok(TopLevelAst::Module(id))
            }
            Token::Use => {
                self.expect_symbol(Token::Use)?;
                let id = self.expect_identifier()?;
                self.expect_symbol(Token::Semicolon)?;
                Ok(TopLevelAst::Use(id))
            }
            Token::Const => Ok(TopLevelAst::Const(Const::parse(self)?)),
            Token::Fn => Ok(TopLevelAst::Fn(Function::parse(self)?)),
            found => Err(Error::UnexpectedToken(
                "one of `module`, `use`, `const`, or `fn`".to_string(),
                found.to_string(),
            )),
        }
    }

    fn peek_expect(&mut self) -> Result<&Token<'a>> {
        self.tokens
            .peek()
            .ok_or(Error::UnexpectedEndOfInput("token".to_string()))
    }

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
    /// ```rs
    /// let mut parser = Parser::from_iter(vec![Token::OpenParen, Token::CloseParen].into_iter());
    /// 
    /// assert_eq!(parser.expect_symbol(Token::OpenParen).unwrap(), ());
    /// assert_eq!(
    ///     parser.expect_symbol(Token::Comma).unwrap_err(), 
    ///     Error::UnexpectedToken(Token::Comma.to_string(), Token::CloseParen.to_string())
    /// );
    /// ```
    pub fn expect_symbol(&mut self, symbol: Token<'a>) -> Result<()> {
        let next_expect = self.next_expect()?;
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
    /// ```rs
    /// let mut parser = Parser::from_iter(vec![Token::OpenParen, Token::CloseParen].into_iter());
    /// 
    /// assert_eq!(parser.expect_symbol(Token::OpenParen).unwrap(), ());
    /// assert_eq!(
    ///     parser.expect_symbol(Token::Comma).unwrap_err(), 
    ///     Error::UnexpectedToken(Token::Comma.to_string(), Token::CloseParen.to_string())
    /// );
    /// ```
    fn expect_identifier(&mut self) -> Result<Identifier<'a>> {
        match self.next_expect()? {
            Token::Identifier(id) => Ok(Identifier(id)),
            found => Err(Error::UnexpectedToken(
                "identifier".to_string(),
                found.to_string(),
            )),
        }
    }

    fn collect_until(&mut self, token: Token<'a>) -> Parser<'a> {
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
                depth == 0
            })
            .collect::<Vec<Token<'a>>>();
        let boxed: Box<dyn Iterator<Item = Token<'a>> + 'a> = Box::new(collected.into_iter());
        Self {
            tokens: boxed.peekable(),
        }
    }
}

pub enum TopLevelAst<'a> {
    Module(Identifier<'a>),
    Use(Identifier<'a>),
    Const(Const<'a>),
    Fn(Function<'a>),
}

pub struct Identifier<'a>(&'a str);

struct Const<'a> {
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
        let type_ = Type::parse(&mut type_tokens)?;
        parser.expect_symbol(Token::Equal)?;
        let mut expr_tokens = parser.collect_until(Token::Semicolon);
        let expression = Expression::parse(&mut expr_tokens)?;
        parser.expect_symbol(Token::Semicolon)?;
        Ok(Const {
            identifier: id,
            type_,
            expression,
        })
    }
}

struct Function<'a> {
    identifier: Identifier<'a>,
    args: Vec<DefArg<'a>>,
    return_type: Type<'a>,
    block: Block<'a>,
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

struct DefArg<'a> {
    identifier: Identifier<'a>,
    type_: Type<'a>,
}

impl<'a> DefArg<'a> {
    fn parse_args(parser: &mut Parser<'a>) -> Result<Vec<Self>> {
        parser.expect_symbol(Token::OpenParen)?;
        let mut args_tokens = parser.collect_until_closing(Token::OpenParen);
        let mut args = vec![];
        while args_tokens.tokens.peek().is_some() {
            let mut arg_tokens = args_tokens.collect_until(Token::Comma);
            args.push(Self::parse(&mut arg_tokens));
        }
        args.into_iter().collect() // Vec<Result<T>> -> Result<Vec<T>>
    }

    fn parse(parser: &mut Parser<'a>) -> Result<Self> {
        let identifier = parser.expect_identifier()?;
        parser.expect_symbol(Token::Colon)?;
        let type_ = Type::parse(parser)?;
        Ok(Self { identifier, type_ })
    }
}

struct Block<'a> {
    statements: Vec<Statement<'a>>,
}

impl<'a> Block<'a> {
    fn parse_body(tokens: &mut Parser<'a>) -> Result<Self> {
        todo!()
    }
}

enum Statement<'a> {
    Expression(Expression<'a>),
    Return(Expression<'a>),
}

enum Type<'a> {
    Identifier(Identifier<'a>),
}

impl<'a> Type<'a> {
    fn parse(tokens: &mut Parser<'a>) -> Result<Self> {
        todo!()
    }
}

enum Expression<'a> {
    Identifier(Identifier<'a>),
    Call(Identifier<'a>, Vec<Expression<'a>>),
    Literal(Literal<'a>),
}

impl<'a> Expression<'a> {
    fn parse(expr_tokens: &mut Parser<'a>) -> Result<Self> {
        todo!()
    }
}

enum Literal<'a> {
    String(&'a str),
    Integer(i128),
    Float(f64),
}
