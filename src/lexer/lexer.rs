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
    keywords.insert("with".to_string(), Token::With);
    keywords.insert("match".to_string(), Token::Match);
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
            Some(b'[') => Ok(Token::LBracket),
            Some(b']') => Ok(Token::RBracket),
            Some(b'=') => Ok(Token::Equal),
            Some(b'-') => match self.peek() {
                Some(b'>') => {
                    self.next();
                    Ok(Token::RArrow)
                }
                Some(_) => Ok(Token::Minus),
                None => Err(LexError::UnexpectedEof),
            },
            Some(b':') => match self.peek() {
                Some(b':') => {
                    self.next();
                    Ok(Token::Cons)
                }
                Some(_) => Err(LexError::UnexpectedToken(format!(
                    "Expected `;;`, but got {}",
                    self.peek().unwrap()
                ))),
                None => Err(LexError::UnexpectedEof),
            },
            Some(b';') => match self.peek() {
                Some(b';') => {
                    self.next();
                    Ok(Token::DoubleSemicolon)
                }
                Some(_) => Ok(Token::Semicolon),
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
    fn test_lex_keywords() -> Result<(), LexError> {
        let mut lexer = Lexer::new("true false let in if then else match with fun rec");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![
                Token::True,
                Token::False,
                Token::Let,
                Token::In,
                Token::If,
                Token::Then,
                Token::Else,
                Token::Match,
                Token::With,
                Token::Fun,
                Token::Rec,
            ],
        );
        Ok(())
    }

    #[test]
    fn test_lex_numbers() -> Result<(), LexError> {
        let mut lexer = Lexer::new("123456789 987654321");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![Token::Number(123456789), Token::Number(987654321),],
        );
        Ok(())
    }

    #[test]
    fn test_lex_identifiers() -> Result<(), LexError> {
        let mut lexer = Lexer::new("let f abc aBC A_b_C");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![
                Token::Let,
                Token::Identifier("f".to_string()),
                Token::Identifier("abc".to_string()),
                Token::Identifier("aBC".to_string()),
                Token::Identifier("A_b_C".to_string()),
            ],
        );
        Ok(())
    }

    #[test]
    fn test_lex_symbols() -> Result<(), LexError> {
        let mut lexer = Lexer::new("+-* < >()[ ] =->; ;;");
        let tokens = lexer.lex()?;
        assert_eq!(
            tokens,
            &vec![
                Token::Plus,
                Token::Minus,
                Token::Asterisk,
                Token::Lt,
                Token::Gt,
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::Equal,
                Token::RArrow,
                Token::Semicolon,
                Token::DoubleSemicolon,
            ],
        );
        Ok(())
    }
}
