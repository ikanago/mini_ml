use std::collections::HashMap;
use std::str::from_utf8;

#[derive(Clone, Debug)]
pub enum Token {
    Number(usize),
    Identifier(String),
    Plus,
    Asterisk,
    If,
}

fn reserve_keyword() -> HashMap<String, Token> {
    let mut keywords = HashMap::new();
    keywords.insert("if".to_string(), Token::If);
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
                _ => unimplemented!()
            };
            self.tokens.push(token);
        }
        Ok(&self.tokens)
    }

    fn lex_plus(&mut self) -> Token {
        self.pos += 1;
        Token::Plus
    }

    fn lex_asterisk(&mut self) -> Token {
        self.pos += 1;
        Token::Asterisk
    }

    fn lex_number(&mut self) -> Token {
        let start = self.pos;
        let end = self.read_many(|b| b"0123456789".contains(&b));
        let num = from_utf8(&self.input[start..end]).unwrap().parse::<usize>().unwrap();
        self.pos = end;
        Token::Number(num)
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.pos;
        let end = self.read_many(|b| b.is_ascii_alphanumeric() || b == b'_');
        self.pos = end;
        let identifier = from_utf8(&self.input[start..end]).unwrap().to_string();
        match self.keywords.get(&identifier) {
            Some(keyword) => keyword.clone(),
            None => Token::Identifier(identifier),
        }
    }

    fn read_many(&self, satisfy: impl Fn(u8) -> bool) -> usize {
        let mut pos = self.pos;
        while pos < self.input.len() && satisfy(self.input[pos]) {
            pos += 1;
        }
        pos
    }
}
