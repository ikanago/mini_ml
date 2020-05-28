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
    pub fn lex_all(&mut self) -> Result<&Vec<Token>, LexError> {
        while !self.is_eof() {
            match self.lex() {
                Ok(Token::WhiteSpace) => continue,
                Ok(token) => self.tokens.push(token),
                Err(err) => return Err(err),
            }
        }
        Ok(&self.tokens)
    }

    fn lex(&mut self) -> Result<Token, LexError> {
        match self.input[self.pos] {
            b'+' => self.lex_plus(),
            b'-' => self.lex_hyphen(),
            b'*' => self.lex_asterisk(),
            b'<' => self.lex_lt(),
            b'>' => self.lex_gt(),
            b'0'..=b'9' => self.lex_number(),
            b'a'..=b'z' | b'A'..=b'Z' => self.lex_identifier(),
            b'(' => self.lex_lparen(),
            b')' => self.lex_rparen(),
            b'=' => self.lex_equal(),
            b' ' | b'\n' => self.skip_spaces(),
            b';' => self.lex_semicolon(),
            _ => unimplemented!(),
        }
    }

    fn lex_plus(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::Plus)
    }

    fn lex_hyphen(&mut self) -> Result<Token, LexError> {
        self.next();
        match self.peek() {
            Some(b'>') => {
                self.next();
                Ok(Token::RArrow)
            }
            Some(_) => Ok(Token::Minus),
            None => Err(LexError::Eof),
        }
    }

    fn lex_asterisk(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::Asterisk)
    }

    fn lex_lt(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::Lt)
    }

    fn lex_gt(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::Gt)
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

    fn lex_lparen(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::LParen)
    }

    fn lex_rparen(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::RParen)
    }

    fn lex_equal(&mut self) -> Result<Token, LexError> {
        self.next();
        Ok(Token::Equal)
    }

    fn skip_spaces(&mut self) -> Result<Token, LexError> {
        self.skip_while(|&b| b == b' ' || b == b'\n');
        Ok(Token::WhiteSpace)
    }

    fn lex_semicolon(&mut self) -> Result<Token, LexError> {
        self.next();
        match self.peek() {
            Some(b';') => {
                self.next();
                Ok(Token::SemiColon)
            }
            Some(_) => Err(LexError::UnexpectedToken(format!(
                "Expected `;;`, but got {}",
                self.peek().unwrap()
            ))),
            None => Err(LexError::Eof),
        }
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
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::lexer::{LexError, Token};
    #[test]
    fn test_lex_if() -> Result<(), LexError> {
        let mut lexer = Lexer::new("if a < b then if a > b then 1 else (b - a) * 4 else 4;;");
        let tokens = lexer.lex_all()?;
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
        let tokens = lexer.lex_all()?;
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
        let tokens = lexer.lex_all()?;
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
