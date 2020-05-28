use crate::lexer::{LexError, Token};
use std::collections::HashMap;

fn reserve_keyword() -> HashMap<String, Token> {
    let mut keywords = HashMap::new();
    keywords.insert("true".to_string(), Token::True);
    keywords.insert("false".to_string(), Token::False);
    keywords.insert("let".to_string(), Token::Let);
    keywords.insert("in".to_string(), Token::In);
    keywords.insert("if".to_string(), Token::If);
    keywords.insert("then".to_string(), Token::Then);
    keywords.insert("else".to_string(), Token::Else);
    keywords.insert("fun".to_string(), Token::Fun);
    keywords.insert("rec".to_string(), Token::Rec);
    keywords
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    keywords: HashMap<String, Token>,
    pub tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
            keywords: reserve_keyword(),
            tokens: Vec::new(),
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    /// Take a look at a next token.
    /// If it exists, return a reference of it.
    fn peek(&self) -> Option<&u8> {
        if self.is_eof() {
            None
        } else {
            Some(&self.input[self.pos])
        }
    }

    /// Peek a next token and if it exists, return it.
    fn next(&mut self) -> Option<u8> {
        match self.peek() {
            Some(&c) => {
                self.pos += 1;
                Some(c)
            }
            None => None,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn lex(&mut self) -> Result<&Vec<Token>, LexError> {
        loop {
            let token = match self.peek() {
                Some(b'0'..=b'9') => self.lex_number(),
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') => self.lex_identifier(),
                Some(b' ') | Some(b'\n') => self.skip_spaces(),
                None => break,
                _ => self.lex_symbol(),
            }?;
            if token != Token::WhiteSpace {
                self.tokens.push(token);
            }
        }
        Ok(&self.tokens)
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        let number_literal = self.take_while(|b| b"0123456789".contains(&b));
        let num = number_literal.parse::<u64>().unwrap();
        Ok(Token::Number(num))
    }

    fn lex_identifier(&mut self) -> Result<Token, LexError> {
        let identifier = self.take_while(|&b| b.is_ascii_alphanumeric() || b == b'_');
        let token = match self.keywords.get(&identifier) {
            Some(keyword) => keyword.clone(),
            None => Token::Identifier(identifier),
        };
        Ok(token)
    }

    fn skip_spaces(&mut self) -> Result<Token, LexError> {
        self.skip_while(|&b| b == b' ' || b == b'\n');
        Ok(Token::WhiteSpace)
    }

    fn take_while(&mut self, satisfy: impl Fn(&u8) -> bool) -> String {
        let mut taken_chars = String::new();
        while !self.is_eof() && satisfy(self.peek().unwrap()) {
            taken_chars.push(self.next().unwrap() as char);
        }
        taken_chars
    }

    fn skip_while(&mut self, satisfy: impl Fn(&u8) -> bool) -> () {
        while !self.is_eof() && satisfy(self.peek().unwrap()) {
            self.next();
        }
    }

    fn lex_symbol(&mut self) -> Result<Token, LexError> {
        match self.next() {
            Some(b'+') => Ok(Token::Plus),
            Some(b'*') => Ok(Token::Asterisk),
            Some(b'<') => Ok(Token::Lt),
            Some(b'>') => Ok(Token::Gt),
            Some(b'(') => Ok(Token::LParen),
            Some(b')') => Ok(Token::RParen),
            Some(b'=') => Ok(Token::Equal),
            Some(b'-') => match self.peek() {
                Some(b'>') => {
                    self.next();
                    Ok(Token::RArrow)
                }
                Some(_) => Ok(Token::Minus),
                None => Err(LexError::UnexpectedEof),
            },
            Some(b';') => match self.peek() {
                Some(b';') => {
                    self.next();
                    Ok(Token::SemiColon)
                }
                Some(_) => Err(LexError::UnexpectedToken(format!(
                    "Expected `;;`, but got {}",
                    self.peek().unwrap()
                ))),
                None => Err(LexError::UnexpectedEof),
            },
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::lexer::{LexError, Token};
    #[test]
    fn test_lex_if() -> Result<(), LexError> {
        let mut lexer = Lexer::new("if a < b then if a > b then 1 else (b - a) * 4 else 4;;");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![
                Token::If,
                Token::Identifier("a".to_string()),
                Token::Lt,
                Token::Identifier("b".to_string()),
                Token::Then,
                Token::If,
                Token::Identifier("a".to_string()),
                Token::Gt,
                Token::Identifier("b".to_string()),
                Token::Then,
                Token::Number(1),
                Token::Else,
                Token::LParen,
                Token::Identifier("b".to_string()),
                Token::Minus,
                Token::Identifier("a".to_string()),
                Token::RParen,
                Token::Asterisk,
                Token::Number(4),
                Token::Else,
                Token::Number(4),
                Token::SemiColon,
            ],
        );
        Ok(())
    }

    #[test]
    fn test_lex_let() -> Result<(), LexError> {
        let mut lexer = Lexer::new("let a = 3 in a + 2;;");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![
                Token::Let,
                Token::Identifier("a".to_string()),
                Token::Equal,
                Token::Number(3),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Number(2),
                Token::SemiColon,
            ],
        );
        Ok(())
    }

    #[test]
    fn test_lex_fun() -> Result<(), LexError> {
        let mut lexer = Lexer::new("let f = fun x -> fun y -> x + y in f 2 3;;");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![
                Token::Let,
                Token::Identifier("f".to_string()),
                Token::Equal,
                Token::Fun,
                Token::Identifier("x".to_string()),
                Token::RArrow,
                Token::Fun,
                Token::Identifier("y".to_string()),
                Token::RArrow,
                Token::Identifier("x".to_string()),
                Token::Plus,
                Token::Identifier("y".to_string()),
                Token::In,
                Token::Identifier("f".to_string()),
                Token::Number(2),
                Token::Number(3),
                Token::SemiColon,
            ],
        );
        Ok(())
    }
}
