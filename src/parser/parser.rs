use crate::lexer::Token;
use crate::parser::syntax::{BinOpKind, Expr};
use crate::parser::ParseError;

#[derive(Clone, Debug, PartialEq)]
enum DefinedAt {
    Let,
    Fun,
}

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
                eprintln!("Actual:   {:?}", token);
                eprintln!("Expected: {:?}", expected_token);
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
    /// EXPR ::= IF_EXPR | LET_EXPR | FUN_EXPR | COMP
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(&Token::If) => self.parse_if(),
            Some(&Token::Let) => self.parse_let(),
            Some(&Token::Fun) => self.parse_fun(),
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
        let bound_var_name = match self.next() {
            Some(Token::Identifier(var)) => var,
            Some(_) => return Err(ParseError::UnexpectedToken),
            None => return Err(ParseError::Eof),
        };
        let initializer = if let Some(&Token::Identifier(_)) = self.peek() {
            self.parse_multi_args_fun(DefinedAt::Let)?
        } else {
            self.expect_token(Token::Equal)?;
            self.parse_expr()?
        };
        self.expect_token(Token::In)?;
        let body = self.parse_expr()?;
        Ok(Expr::Let(
            bound_var_name,
            Box::new(initializer),
            Box::new(body),
        ))
    }

    /// BNF:
    /// FUN_EXPR ::= `fun` IDENT `->` EXPR
    fn parse_fun(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::Fun)?;
        let arg = match self.next() {
            Some(Token::Identifier(var)) => Ok(var),
            Some(_) => Err(ParseError::UnexpectedToken),
            None => Err(ParseError::Eof),
        }?;
        if let Some(&Token::Identifier(_)) = self.peek() {
            let fun_inner = self.parse_multi_args_fun(DefinedAt::Fun)?;
            Ok(Expr::Fun(arg, Box::new(fun_inner)))
        } else {
            self.expect_token(Token::RArrow)?;
            let body = self.parse_expr()?;
            Ok(Expr::Fun(arg, Box::new(body)))
        }
    }

    fn parse_multi_args_fun(&mut self, defined_at: DefinedAt) -> Result<Expr, ParseError> {
        match self.next() {
            Some(Token::Identifier(var)) => Ok(Expr::Fun(
                var,
                Box::new(self.parse_multi_args_fun(defined_at)?),
            )),
            Some(Token::RArrow) if defined_at == DefinedAt::Fun => self.parse_expr(),
            Some(Token::Equal) if defined_at == DefinedAt::Let => self.parse_expr(),
            Some(_) => Err(ParseError::UnexpectedToken),
            None => Err(ParseError::Eof),
        }
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
    /// MUL ::= FUNC_APPLY (`*` FUNC_APPLY)?
    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_fn_apply()?;
        loop {
            if self.peek() == Some(&Token::Asterisk) {
                self.next();
                let rhs = self.parse_fn_apply()?;
                lhs = Expr::BinOp(BinOpKind::Mul, Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    /// FUN_APLLY ::= PRIM PRIM?
    fn parse_fn_apply(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_primary()?;
        loop {
            // Try to read as function application.
            match self.parse_primary() {
                Ok(arg) => node = Expr::Apply(Box::new(node), Box::new(arg)),
                Err(ParseError::NonTerminalSymbol) => {
                    // If this is not function application, roll back token position
                    // because next token is passed over.
                    self.pos -= 1;
                    break;
                }
                Err(err) => return Err(err),
            }
        }
        Ok(node)
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
                    let node = self.parse_expr()?;
                    match self.next() {
                        Some(Token::RParen) => Ok(node),
                        Some(_) => Err(ParseError::UnexpectedToken),
                        None => Err(ParseError::UnclosedParen),
                    }
                }
                _ => Err(ParseError::NonTerminalSymbol),
            })
    }
}
