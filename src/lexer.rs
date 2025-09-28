use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Use,
    Fn,
    Return,
    OpenParen,
    CloseParen,
    OpenSquirrely,
    CloseSquirrely,
    Colon,
    Semicolon,
    Identifier(&'a str),
    StringLiteral(&'a str),
    Number(&'a str),
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    fn chomp<C: Fn((usize, char)) -> bool>(
        &mut self,
        start: usize,
        condition: C,
    ) -> Option<&'a str> {
        let mut string_length = 0;
        while let Some(chi) = self.chars.peek()
            && condition(*chi)
        {
            self.chars.next(); // advance consumption of the iterator
            string_length += 1;
        }
        self.source.get(start..=start + string_length)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (i, ch) = self.chars.next()?;
        match ch {
            '"' => {
                let is_not_closing_quote = |(i, ch)| return ch != '"';
                let literal = &self.chomp(i, is_not_closing_quote)?[1..];
                self.chars.next(); // throw away closing quote
                Some(Token::StringLiteral(literal))
            }
            'a'..='z' | 'A'..='Z' | '.' | '_' => {
                let is_id_char = |(_, c)| {
                    ('a'..='z').contains(&c)
                        || ('A'..='Z').contains(&c)
                        || ('0'..='9').contains(&c)
                        || c == '.'
                        || c == '_'
                };
                let identifier = self.chomp(i, is_id_char)?;
                let token = match identifier {
                    "use" => Token::Use,
                    "return" => Token::Return,
                    "fn" => Token::Fn,
                    id => Token::Identifier(id),
                };
                Some(token)
            }
            '0'..='9' => {
                let is_digit = |(_, c)| ('0'..'9').contains(&c) || c == '.';
                let number = self.chomp(i, is_digit)?;
                Some(Token::Number(number))
            }
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenSquirrely),
            '}' => Some(Token::CloseSquirrely),
            ';' => Some(Token::Semicolon),
            ':' => Some(Token::Colon),
            ' ' | '\t' | '\n' => self.next(), // ignore whitespace and try to parse the next token
            _ => panic!("Unknown Character: '{ch}'"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token};

    #[test]
    fn parse_hello_world() {
        let source = "use std.io; i32 main() { io.println(\"Hello, world!\"); return 0; }";
        let expected = vec![
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
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_str_literal() {
        let source = "\"hello\"";
        let expected = vec![Token::StringLiteral("hello")];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_keywords() {
        let source = "hello use return fn";
        let expected = vec![
            Token::Identifier("hello"),
            Token::Use,
            Token::Return,
            Token::Fn,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result)
    }

    #[test]
    fn parse_number() {
        let source = "42";
        let expected = vec![Token::Number("42")];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result)
    }
}
