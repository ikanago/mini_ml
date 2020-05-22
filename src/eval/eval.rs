use crate::eval::TypeError;
use crate::parser::syntax::{BinOpKind, Expr};
use std::collections::HashMap;

pub struct Eval {
    environment: HashMap<String, Expr>,
}

impl Eval {
    pub fn new() -> Self {
        Self {
            environment: HashMap::new(),
        }
    }

    pub fn eval(&mut self, vec_ast: &Vec<Expr>) -> Result<Vec<Expr>, TypeError> {
        let mut result = vec![];
        for ast in vec_ast.iter() {
            let expr = self.eval_expression(&ast)?;
            result.push(expr);
        }
        Ok(result)
    }

    fn eval_expression(&mut self, ast: &Expr) -> Result<Expr, TypeError> {
        match &ast {
            &Expr::Var(var) => match self.environment.get(var) {
                Some(expr) => Ok(expr.clone()),
                None => Err(TypeError::NotBound(var.clone())),
            },
            &Expr::U64(n) => Ok(Expr::U64(*n)),
            &Expr::Bool(b) => Ok(Expr::Bool(*b)),
            &Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.eval_expression(lhs)?;
                let rhs = self.eval_expression(rhs)?;
                Eval::apply_operator(op.clone(), lhs, rhs)
            }
            &Expr::Let(var, init, body) => {
                let init = self.eval_expression(init)?;
                self.environment.insert(var.clone(), init);
                self.eval_expression(body)
            }
            &Expr::If(condition, then, els) => {
                let condition = self.eval_expression(condition)?;
                match condition {
                    Expr::Bool(true) => self.eval_expression(then),
                    Expr::Bool(false) => self.eval_expression(els),
                    _ => Err(TypeError::UnexpectedType(
                        "`if` condition must be bool".to_string(),
                    )),
                }
            }
        }
    }

    fn apply_operator(op: BinOpKind, lhs: Expr, rhs: Expr) -> Result<Expr, TypeError> {
        match (op, lhs, rhs) {
            (BinOpKind::Add, Expr::U64(n), Expr::U64(m)) => Ok(Expr::U64(n + m)),
            (BinOpKind::Mul, Expr::U64(n), Expr::U64(m)) => Ok(Expr::U64(n * m)),
            (BinOpKind::Lt, Expr::U64(n), Expr::U64(m)) => Ok(Expr::Bool(n < m)),
            (BinOpKind::Gt, Expr::U64(n), Expr::U64(m)) => Ok(Expr::Bool(n > m)),
            _ => Err(TypeError::UnsupportedOperandType(
                "Both arguments must be u64".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::eval::Eval;
    use crate::eval::TypeError;
    use crate::lexer::lexer::Lexer;
    use crate::parser::parser::Parser;
    use crate::parser::syntax::Expr;
    use std::collections::HashMap;
    #[test]
    fn test_eval_if() -> Result<(), TypeError> {
        let mut lexer = Lexer::new("if a < b then if a > b then 1 else (a + b) * 4 else 4;;");
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let vec_ast = parser.parse().unwrap();
        let mut environment: HashMap<String, Expr> = HashMap::new();
        environment.insert("a".to_string(), Expr::U64(2));
        environment.insert("b".to_string(), Expr::U64(3));
        let mut evaluator = Eval::new(environment);
        let result = evaluator.eval(&vec_ast)?;
        assert_eq!(result, vec![Expr::U64(20)],);
        Ok(())
    }

    #[test]
    fn test_eval_let() -> Result<(), TypeError> {
        let mut lexer = Lexer::new("let a = 3 in a + 2;;");
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let vec_ast = parser.parse().unwrap();
        let environment: HashMap<String, Expr> = HashMap::new();
        let mut evaluator = Eval::new(environment);
        let result = evaluator.eval(&vec_ast)?;
        assert_eq!(result, vec![Expr::U64(5)],);
        Ok(())
    }
}
