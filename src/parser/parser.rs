use crate::lexer::Token;
use crate::parser::ParseError;
use crate::parser::{BinOpKind, Expr};
use std::collections::VecDeque;

///Specify caller of `Parser::parse_multi_args_fun`.
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

    // If a next token is expected type, proceed to next.
    fn consume(&mut self, expected_token: Token) -> Result<bool, ParseError> {
        match self.peek() {
            Some(next_token) => {
                if next_token == &expected_token {
                    self.next();
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            None => Err(ParseError::Eof),
        }
    }

    /// Check if a next token has expected type and proceed to next.
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
}

impl<'a> Parser<'a> {
    /// Parse tokens and build AST.
    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut asts = Vec::new();
        loop {
            let ast = self.parse_expr()?;
            self.expect_token(Token::DoubleSemicolon)?;
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
            Some(&Token::Match) => self.parse_match(),
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
    /// LET_EXPR ::= `let` `rec`? IDENT = EXPR `in` EXPR
    ///            | `let` `rec`? IDENT IDENT* = EXPR `in` EXPR
    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::Let)?;
        let is_recursive = self.peek() == Some(&Token::Rec);
        if is_recursive {
            self.next();
        }
        let bound_var_name = match self.next() {
            Some(Token::Identifier(var)) => var,
            Some(_) => return Err(ParseError::UnexpectedToken),
            None => return Err(ParseError::Eof),
        };
        let initializer = if let Some(&Token::Identifier(_)) = self.peek() {
            // function
            self.parse_multi_args_fun(DefinedAt::Let)?
        } else {
            // value
            self.expect_token(Token::Equal)?;
            self.parse_expr()?
        };
        self.expect_token(Token::In)?;
        let body = self.parse_expr()?;

        if is_recursive {
            let first_arg = match &initializer {
                Expr::Fun(arg, _) => arg.clone(),
                _ => unreachable!(),
            };
            Ok(Expr::LetRec(
                bound_var_name,
                first_arg,
                Box::new(initializer),
                Box::new(body),
            ))
        } else {
            Ok(Expr::Let(
                bound_var_name,
                Box::new(initializer),
                Box::new(body),
            ))
        }
    }

    /// BNF:
    /// MATCH_EXPR ::=
    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::Match)?;
        let condition = self.parse_expr()?;
        self.expect_token(Token::With)?;
        let mut clauses = Vec::new();

        while self.consume(Token::VerticalBar)? {
            let pattern = match self.peek() {
                Some(Token::LBracket) => {
                    self.parse_primary()?
                }
                Some(Token::Identifier(_)) => {
                    self.parse_cons()?
                }
                _ => return Err(ParseError::UnexpectedToken),
            };
            self.expect_token(Token::RArrow)?;
            let arm = self.parse_expr()?;
            clauses.push((pattern, arm));
        }
        Ok(Expr::Match(Box::new(condition), clauses))
    }

    /// BNF:
    /// FUN_EXPR ::= `fun` IDENT `->` EXPR
    ///            | `fun` IDENT* `->` EXPR
    fn parse_fun(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::Fun)?;
        // First argument (required)
        let arg = match self.next() {
            Some(Token::Identifier(var)) => Ok(var),
            Some(_) => Err(ParseError::UnexpectedToken),
            None => Err(ParseError::Eof),
        }?;
        if let Some(&Token::Identifier(_)) = self.peek() {
            // other arguments (optional)
            let fun_inner = self.parse_multi_args_fun(DefinedAt::Fun)?;
            Ok(Expr::Fun(arg, Box::new(fun_inner)))
        } else {
            self.expect_token(Token::RArrow)?;
            let body = self.parse_expr()?;
            Ok(Expr::Fun(arg, Box::new(body)))
        }
    }

    /// Parse functions with multiple arguments
    /// such as `let f x y = ...` or `let f = fun x y -> ...`.
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
    /// COMP ::= CONS ((`<` CONS) | (`>` CONS))?
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
    /// CONS ::= ADD (`::` CONS)?
    fn parse_cons(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_mul()?;
        self.expect_token(Token::Cons)?;
        let rhs = self.parse_mul()?;
        Ok(Expr::Cons(Box::new(lhs), Box::new(rhs)))
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
            } else if self.peek() == Some(&Token::Minus) {
                self.next();
                let rhs = self.parse_mul()?;
                lhs = Expr::BinOp(BinOpKind::Sub, Box::new(lhs), Box::new(rhs))
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
                Token::Number(n) => Ok(Expr::I64(n)),
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
                Token::LBracket => {
                    if self.consume(Token::RBracket)? {
                        return Ok(Expr::Nil);
                    }
                    let mut expr_array = VecDeque::new();
                    while !self.consume(Token::RBracket)? {
                        let element = self.parse_expr()?;
                        expr_array.push_back(element);
                        self.consume(Token::Semicolon)?;
                    }
                    Ok(Expr::Array(expr_array))
                }
                _ => Err(ParseError::NonTerminalSymbol),
            })
    }
}
