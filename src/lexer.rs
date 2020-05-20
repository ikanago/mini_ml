use std::str::from_utf8;

#[derive(Debug)]
pub enum Token {
    Number(usize),
    Identifier(String),
    Plus,
    Asterisk,
}

#[derive(Debug)]
pub enum LexError {}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    pub tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
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
        unimplemented!()
    }

    fn read_many(&self, satisfy: impl Fn(u8) -> bool) -> usize {
        let mut pos = self.pos;
        while pos < self.input.len() && satisfy(self.input[pos]) {
            pos += 1;
        }
        pos
    }
}
