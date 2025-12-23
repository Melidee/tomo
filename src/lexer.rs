use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Module,
    Use,
    Fn,
    Self_,
    Let,
    Return,
    Const,
    Type,
    Struct,
    OpenParen,
    CloseParen,
    OpenSquirrely,
    CloseSquirrely,
    OpenBracket,
    CloseBracket,
    Colon,
    Semicolon,
    Comma,
    Slash,
    Star,
    Equal,
    Plus,
    Minus,
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

    fn chomp<C: FnMut((usize, char)) -> bool>(
        &mut self,
        start: usize,
        mut condition: C,
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
                let is_not_closing_quote = |(_, ch)| return ch != '"';
                let literal = &self.chomp(i, is_not_closing_quote)?[1..];
                self.chars.next(); // throw away closing quote
                Some(Token::StringLiteral(literal))
            }
            '/' if matches!(self.chars.peek(), Some((_, '/'))) => {
                self.chomp(i, |(_, ch)| ch != '\n');
                self.next()
            }
            '/' if matches!(self.chars.peek(), Some((_, '*'))) => {
                while let Some((_, ch)) = self.chars.next() {
                    if ch == '*' && self.chars.peek().is_some_and(|(_, ch2)| *ch2 == '/') {
                        break;
                    }
                }
                self.next()
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
                    "module" => Token::Module,
                    "use" => Token::Use,
                    "fn" => Token::Fn,
                    "self" => Token::Self_,
                    "let" => Token::Let,
                    "return" => Token::Return,
                    "const" => Token::Const,
                    "type" => Token::Type,
                    "struct" => Token::Struct,
                    id => Token::Identifier(id),
                };
                Some(token)
            }
            '0'..='9' => {
                let is_digit = |(_, c)| ('0'..='9').contains(&c) || c == '.';
                let number = self.chomp(i, is_digit)?;
                Some(Token::Number(number))
            }
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenSquirrely),
            '}' => Some(Token::CloseSquirrely),
            '[' => Some(Token::OpenBracket),
            ']' => Some(Token::CloseBracket),
            ';' => Some(Token::Semicolon),
            ':' => Some(Token::Colon),
            ',' => Some(Token::Comma),
            '/' => Some(Token::Slash),
            '*' => Some(Token::Star),
            '=' => Some(Token::Equal),
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            ' ' | '\t' | '\n' => self.next(), // ignore whitespace and try to parse the next token
            _ => panic!("Unknown Character: '{ch}'"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token};

    // ===== Keywords =====
    #[test]
    fn test_all_keywords() {
        let source = "module use fn return const";
        let expected = vec![
            Token::Module,
            Token::Use,
            Token::Fn,
            Token::Return,
            Token::Const,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_keywords_no_whitespace() {
        let source = "module(use)fn";
        let expected = vec![
            Token::Module,
            Token::OpenParen,
            Token::Use,
            Token::CloseParen,
            Token::Fn,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== Identifiers =====
    #[test]
    fn test_simple_identifier() {
        let source = "hello world foo bar";
        let expected = vec![
            Token::Identifier("hello"),
            Token::Identifier("world"),
            Token::Identifier("foo"),
            Token::Identifier("bar"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_identifier_with_underscore() {
        let source = "_foo foo_bar my_var_123";
        let expected = vec![
            Token::Identifier("_foo"),
            Token::Identifier("foo_bar"),
            Token::Identifier("my_var_123"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_identifier_with_dots() {
        let source = "std.io std.collections.Vec module.submodule.func";
        let expected = vec![
            Token::Identifier("std.io"),
            Token::Identifier("std.collections.Vec"),
            Token::Identifier("module.submodule.func"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_identifier_mixed_case() {
        let source = "MyClass someFunc CONSTANT";
        let expected = vec![
            Token::Identifier("MyClass"),
            Token::Identifier("someFunc"),
            Token::Identifier("CONSTANT"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== Numbers =====
    #[test]
    fn test_integer() {
        let source = "0 42 123456";
        let expected = vec![
            Token::Number("0"),
            Token::Number("42"),
            Token::Number("123456"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_float() {
        let source = "3.14 0.5 123.456";
        let expected = vec![
            Token::Number("3.14"),
            Token::Number("0.5"),
            Token::Number("123.456"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== String Literals =====
    #[test]
    fn test_empty_string() {
        let source = r#""""#;
        let expected = vec![Token::StringLiteral("")];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_string_with_spaces() {
        let source = r#""Hello, world!""#;
        let expected = vec![Token::StringLiteral("Hello, world!")];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_multiple_strings() {
        let source = r#""first" "second" "third""#;
        let expected = vec![
            Token::StringLiteral("first"),
            Token::StringLiteral("second"),
            Token::StringLiteral("third"),
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_string_with_special_chars() {
        let source = r#""123!@#$%^&*()_+-=[]{}|;:,.<>?""#;
        let expected = vec![Token::StringLiteral("123!@#$%^&*()_+-=[]{}|;:,.<>?")];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== Punctuation =====
    #[test]
    fn test_parentheses() {
        let source = "())(()";
        let expected = vec![
            Token::OpenParen,
            Token::CloseParen,
            Token::CloseParen,
            Token::OpenParen,
            Token::OpenParen,
            Token::CloseParen,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_braces() {
        let source = "{}}{{}";
        let expected = vec![
            Token::OpenSquirrely,
            Token::CloseSquirrely,
            Token::CloseSquirrely,
            Token::OpenSquirrely,
            Token::OpenSquirrely,
            Token::CloseSquirrely,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_brackets() {
        let source = "[][[]]";
        let expected = vec![
            Token::OpenBracket,
            Token::CloseBracket,
            Token::OpenBracket,
            Token::OpenBracket,
            Token::CloseBracket,
            Token::CloseBracket,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_all_punctuation() {
        let source = "(){}[];:,*";
        let expected = vec![
            Token::OpenParen,
            Token::CloseParen,
            Token::OpenSquirrely,
            Token::CloseSquirrely,
            Token::OpenBracket,
            Token::CloseBracket,
            Token::Semicolon,
            Token::Colon,
            Token::Comma,
            Token::Star,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== Comments =====
    #[test]
    fn test_single_line_comment() {
        let source = "// this is a comment\nfn";
        let expected = vec![Token::Fn];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_comment_at_end() {
        let source = "fn // comment";
        let expected = vec![Token::Fn];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_multiple_comments() {
        let source = "// comment 1\nfn // comment 2\n// comment 3\nuse";
        let expected = vec![Token::Fn, Token::Use];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_comment_with_code_like_content() {
        let source = "// fn use return const\nmodule";
        let expected = vec![Token::Module];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== Whitespace =====
    #[test]
    fn test_whitespace_handling() {
        let source = "  fn   \t  use  \n  return  ";
        let expected = vec![Token::Fn, Token::Use, Token::Return];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_newlines() {
        let source = "fn\n\nuse\n\n\nreturn";
        let expected = vec![Token::Fn, Token::Use, Token::Return];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_tabs() {
        let source = "fn\t\tuse\t\t\treturn";
        let expected = vec![Token::Fn, Token::Use, Token::Return];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    // ===== Complex Examples =====
    #[test]
    fn test_function_declaration() {
        let source = "fn add(a: i32, b: i32) { return a; }";
        let expected = vec![
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
            Token::OpenSquirrely,
            Token::Return,
            Token::Identifier("a"),
            Token::Semicolon,
            Token::CloseSquirrely,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_array_syntax() {
        let source = "const arr: [3]i32 = [1, 2, 3];";
        let expected = vec![
            Token::Const,
            Token::Identifier("arr"),
            Token::Colon,
            Token::OpenBracket,
            Token::Number("3"),
            Token::CloseBracket,
            Token::Identifier("i32"),
            Token::Equal,
            Token::OpenBracket,
            Token::Number("1"),
            Token::Comma,
            Token::Number("2"),
            Token::Comma,
            Token::Number("3"),
            Token::CloseBracket,
            Token::Semicolon,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_pointer_syntax() {
        let source = "fn deref(ptr: *i32) { return *ptr; }";
        let expected = vec![
            Token::Fn,
            Token::Identifier("deref"),
            Token::OpenParen,
            Token::Identifier("ptr"),
            Token::Colon,
            Token::Star,
            Token::Identifier("i32"),
            Token::CloseParen,
            Token::OpenSquirrely,
            Token::Return,
            Token::Star,
            Token::Identifier("ptr"),
            Token::Semicolon,
            Token::CloseSquirrely,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_hello_world() {
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
        assert_eq!(expected, result);
    }

    // ===== Edge Cases =====
    #[test]
    fn test_empty_source() {
        let source = "";
        let expected: Vec<Token> = vec![];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_only_whitespace() {
        let source = "   \t\n  \t  \n\n";
        let expected: Vec<Token> = vec![];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_only_comments() {
        let source = "// comment 1\n// comment 2\n// comment 3";
        let expected: Vec<Token> = vec![];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }

    #[test]
    #[should_panic(expected = "Unknown Character: '@'")]
    fn test_unknown_character() {
        let source = "fn @ use";
        let _ = Lexer::new(source).collect::<Vec<Token>>();
    }

    #[test]
    #[should_panic(expected = "Unknown Character: '&'")]
    fn test_unknown_ampersand() {
        let source = "& foo";
        let _ = Lexer::new(source).collect::<Vec<Token>>();
    }

    #[test]
    fn test_adjacent_tokens_no_space() {
        let source = "fn(){}[]";
        let expected = vec![
            Token::Fn,
            Token::OpenParen,
            Token::CloseParen,
            Token::OpenSquirrely,
            Token::CloseSquirrely,
            Token::OpenBracket,
            Token::CloseBracket,
        ];
        let result = Lexer::new(source).collect::<Vec<Token>>();
        assert_eq!(expected, result);
    }
}
