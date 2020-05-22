use crate::lexer::Token;
use crate::parser::syntax::{BinOpKind, Expr};
use crate::parser::ParseError;

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    /// Take a look at a next token and return its kind.
    fn peek(&self) -> Option<&Token> {
        if self.tokens.len() == self.pos {
            return None;
        }
        Some(&self.tokens[self.pos])
    }

    /// Return current token and move `pos` forward.
    fn next(&mut self) -> Option<Token> {
        if self.tokens.len() == self.pos {
            return None;
        }
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        Some(token)
    }

    /// Check if a current token has expected type and proceed to next one.
    fn expect_token(&mut self, expected_token: Token) -> Result<(), ParseError> {
        self.next().ok_or(ParseError::Eof).and_then(|token| {
            if token == expected_token {
                Ok(())
            } else {
                println!("{:?}", token);
                println!("{:?}", expected_token);
                Err(ParseError::UnexpectedToken)
            }
        })
    }

    /// Parse tokens and build AST.
    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut asts = Vec::new();
        loop {
            let ast = self.parse_expr()?;
            self.expect_token(Token::SemiColon)?;
            asts.push(ast);
            if self.peek().is_none() {
                break;
            }
        }
        Ok(asts)
    }

    /// BNF:
    /// EXPR ::= IF_EXPR | LET_EXPR | COMP
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(&Token::If) => self.parse_if(),
            Some(&Token::Let) => self.parse_let(),
            _ => self.parse_compare(),
        }
    }

    /// BNF:
    /// IF_EXPR ::= `if` COMP `then` EXPR `else` EXPR
    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::If)?;
        let condition = self.parse_compare()?;
        self.expect_token(Token::Then)?;
        let then = self.parse_expr()?;
        self.expect_token(Token::Else)?;
        let els = self.parse_expr()?;
        Ok(Expr::If(Box::new(condition), Box::new(then), Box::new(els)))
    }

    /// BNF:
    /// LET_EXPR ::= `let` IDENT = `EXPR` IN `EXPR`
    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::Let)?;
        let bound_var_name = self
            .next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token {
                Token::Identifier(var) => Ok(var),
                _ => return Err(ParseError::UnexpectedToken),
            })?;
        self.expect_token(Token::Equal)?;
        let initilizer = self.parse_expr()?;
        self.expect_token(Token::In)?;
        let body = self.parse_expr()?;
        Ok(Expr::Let(
            bound_var_name,
            Box::new(initilizer),
            Box::new(body),
        ))
    }

    /// BNF:
    /// COMP ::= ADD ((`<` ADD) | (`>` ADD))?
    fn parse_compare(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_add()?;
        match self.peek() {
            Some(&Token::Lt) => {
                self.next();
                let rhs = self.parse_add()?;
                Ok(Expr::BinOp(BinOpKind::Lt, Box::new(lhs), Box::new(rhs)))
            }
            Some(&Token::Gt) => {
                self.next();
                let rhs = self.parse_add()?;
                Ok(Expr::BinOp(BinOpKind::Gt, Box::new(lhs), Box::new(rhs)))
            }
            _ => Ok(lhs),
        }
    }

    /// BNF:
    /// ADD ::= MUL (`+` MUL)?
    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_mul()?;
        loop {
            if self.peek() == Some(&Token::Plus) {
                self.next();
                let rhs = self.parse_mul()?;
                lhs = Expr::BinOp(BinOpKind::Add, Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    /// MUL ::= PRIM (`*` PRIM)?
    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary()?;
        loop {
            if self.peek() == Some(&Token::Asterisk) {
                self.next();
                let rhs = self.parse_primary()?;
                lhs = Expr::BinOp(BinOpKind::Mul, Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    /// PRIM ::= U64 | IDENT | TRUE | FALSE | `(` COMP `)`
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token {
                Token::Number(n) => Ok(Expr::U64(n)),
                Token::Identifier(var) => Ok(Expr::Var(var)),
                Token::True => Ok(Expr::Bool(true)),
                Token::False => Ok(Expr::Bool(false)),
                Token::LParen => {
                    let node = self.parse_compare()?;
                    match self.next() {
                        Some(Token::RParen) => Ok(node),
                        Some(_) => Err(ParseError::UnexpectedToken),
                        None => Err(ParseError::UnclosedParen),
                    }
                }
                _ => unimplemented!(),
            })
    }
}
