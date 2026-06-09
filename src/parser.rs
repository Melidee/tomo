use std::iter::Peekable;

use crate::{
    error::{Error, Result},
    lexer::{Lexer, Token},
};

struct Parser<'a> {
    tokens: Peekable<Box<dyn Iterator<Item = Token<'a>> + 'a>>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let boxed: Box<dyn Iterator<Item = Token<'a>> + 'a> = Box::new(lexer);
        Self {
            tokens: boxed.peekable(),
        }
    }

    fn parse_top_level(&mut self) -> Result<TopLevelAst<'a>> {
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

    fn expect_symbol(&mut self, symbol: Token<'a>) -> Result<()> {
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

    fn expect_identifier(&mut self) -> Result<Identifier<'a>> {
        match self.next_expect()? {
            Token::Identifier(id) => Ok(Identifier(id)),
            found => Err(Error::UnexpectedToken(
                "identifier".to_string(),
                found.to_string(),
            )),
        }
    }

    fn collect_until(&mut self, token: &Token<'a>) -> Vec<Token<'a>> {
        self.tokens.by_ref().take_while(|t| t != token).collect()
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
        let type_tokens = parser.collect_until(&Token::Equal);
        let type_ = Type::parse(type_tokens)?;
        let expr_tokens = parser.collect_until(&Token::Semicolon);
        let expression = Expression::parse(expr_tokens)?;
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

struct DefArg<'a> {
    identifier: Identifier<'a>,
    type_: Type<'a>,
}

struct Block<'a> {
    statements: Vec<Statement<'a>>,
}

enum Statement<'a> {
    Expression(Expression<'a>),
    Return(Expression<'a>),
}

enum Type<'a> {
    Identifier(Identifier<'a>),
}

impl<'a> Type<'a> {
    fn parse(tokens: Vec<Token<'a>>) -> Result<Self> {
        todo!()
    }
}

enum Expression<'a> {
    Identifier(Identifier<'a>),
    Call(Identifier<'a>, Vec<Expression<'a>>),
    Literal(Literal<'a>),
}

impl<'a> Expression<'a> {
    fn parse(expr_tokens: Vec<Token<'a>>) -> Result<Self> {
        todo!()
    }
}

enum Literal<'a> {
    String(&'a str),
    Integer(i128),
    Float(f64),
}
