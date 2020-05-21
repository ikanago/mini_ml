use std::collections::HashMap;
use std::str::from_utf8;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Number(u64),
    Identifier(String),
    True,
    False,
    Plus,
    Asterisk,
    If,
    Then,
    Else,
    SemiColon,
}

fn reserve_keyword() -> HashMap<String, Token> {
    let mut keywords = HashMap::new();
    keywords.insert("true".to_string(), Token::True);
    keywords.insert("false".to_string(), Token::False);
    keywords.insert("if".to_string(), Token::If);
    keywords.insert("then".to_string(), Token::Then);
    keywords.insert("else".to_string(), Token::Else);
    keywords
}

#[derive(Debug)]
pub enum LexError {}

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

    pub fn lex(&mut self) -> Result<&Vec<Token>, LexError> {
        while self.pos < self.input.len() {
            let token = match self.input[self.pos] {
                b'+' => self.lex_plus(),
                b'*' => self.lex_asterisk(),
                b'0'..=b'9' => self.lex_number(),
                b'a'..=b'z' | b'A'..=b'Z' => self.lex_identifier(),
                b' ' => self.skip_spaces(),
                b';' => self.lex_semicolon(),
                _ => unimplemented!(),
            };
            if let Some(token) = token {
                self.tokens.push(token);
            }
        }
        Ok(&self.tokens)
    }

    fn lex_plus(&mut self) -> Option<Token> {
        self.pos += 1;
        Some(Token::Plus)
    }

    fn lex_asterisk(&mut self) -> Option<Token> {
        self.pos += 1;
        Some(Token::Asterisk)
    }

    fn lex_number(&mut self) -> Option<Token> {
        let start = self.pos;
        let end = self.read_many(|b| b"0123456789".contains(&b));
        let num = from_utf8(&self.input[start..end])
            .unwrap()
            .parse::<u64>()
            .unwrap();
        self.pos = end;
        Some(Token::Number(num))
    }

    fn lex_identifier(&mut self) -> Option<Token> {
        let start = self.pos;
        let end = self.read_many(|b| b.is_ascii_alphanumeric() || b == b'_');
        self.pos = end;
        let identifier = from_utf8(&self.input[start..end]).unwrap().to_string();
        let token = match self.keywords.get(&identifier) {
            Some(keyword) => keyword.clone(),
            None => Token::Identifier(identifier),
        };
        Some(token)
    }

    fn skip_spaces(&mut self) -> Option<Token> {
        let skipped_pos = self.read_many(|b| b == b' ');
        self.pos = skipped_pos;
        None
    }

    fn lex_semicolon(&mut self) -> Option<Token> {
        if self.pos + 1 == self.input.len() {
            return None;
        } else if self.input[self.pos] != b';' {
            return None;
        }
        self.pos += 2;
        Some(Token::SemiColon)
    }

    fn read_many(&self, satisfy: impl Fn(u8) -> bool) -> usize {
        let mut pos = self.pos;
        while pos < self.input.len() && satisfy(self.input[pos]) {
            pos += 1;
        }
        pos
    }
}
