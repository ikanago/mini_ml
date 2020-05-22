use crate::eval::TypeError;
use crate::parser::syntax::{BinOpKind, Expr};
use std::collections::HashMap;

pub struct Eval {
    vec_ast: Vec<Expr>,
    environment: HashMap<String, Expr>,
}

impl Eval {
    pub fn new(vec_ast: Vec<Expr>, environment: HashMap<String, Expr>) -> Self {
        Self {
            vec_ast,
            environment,
        }
    }

    pub fn eval(&self) -> Result<Vec<Expr>, TypeError> {
        let mut result = vec![];
        for ast in self.vec_ast.iter() {
            let expr = self.eval_expression(&ast)?;
            result.push(expr);
        }
        Ok(result)
    }

    fn eval_expression(&self, ast: &Expr) -> Result<Expr, TypeError> {
        match &ast {
            &Expr::Var(var) => match self.environment.get(var) {
                Some(expr) => Ok(expr.clone()),
                None => Err(TypeError::NotBound(var.clone())),
            },
            &Expr::U64(n) => Ok(Expr::U64(*n)),
            &Expr::Bool(b) => Ok(Expr::Bool(*b)),
            &Expr::Binop(op, lhs, rhs) => {
                Eval::apply_operator(op.clone(), lhs.clone(), rhs.clone())
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

    fn apply_operator(op: BinOpKind, lhs: Box<Expr>, rhs: Box<Expr>) -> Result<Expr, TypeError> {
        match (op, *lhs, *rhs) {
            (BinOpKind::Plus, Expr::U64(n), Expr::U64(m)) => Ok(Expr::U64(n + m)),
            (BinOpKind::Plus, _, _) => Err(TypeError::UnsupportedOperandType(
                "Both arguments must be u64".to_string(),
            )),
            (BinOpKind::Mult, Expr::U64(n), Expr::U64(m)) => Ok(Expr::U64(n * m)),
            (BinOpKind::Mult, _, _) => Err(TypeError::UnsupportedOperandType(
                "Both arguments must be u64".to_string(),
            )),
            _ => unreachable!(),
        }
    }
}
