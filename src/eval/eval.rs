use crate::eval::{ExprVal, TypeError};
use crate::parser::syntax::{BinOpKind, Expr};
use std::collections::HashMap;

pub fn eval(vec_ast: &Vec<Expr>) -> Result<Vec<ExprVal>, TypeError> {
    let mut result = vec![];
    for ast in vec_ast.iter() {
        let mut environment: HashMap<String, ExprVal> = HashMap::new();
        let expr = eval_expression(&ast, &mut environment)?;
        result.push(expr);
    }
    Ok(result)
}

fn eval_expression(
    ast: &Expr,
    environment: &mut HashMap<String, ExprVal>,
) -> Result<ExprVal, TypeError> {
    match &ast {
        &Expr::Var(var) => match environment.get(var) {
            Some(expr) => Ok(expr.clone()),
            None => Err(TypeError::NotBound(var.clone())),
        },
        &Expr::U64(n) => Ok(ExprVal::U64(*n)),
        &Expr::Bool(b) => Ok(ExprVal::Bool(*b)),
        &Expr::BinOp(op, lhs, rhs) => {
            let lhs = eval_expression(lhs, environment)?;
            let rhs = eval_expression(rhs, environment)?;
            apply_operator(op.clone(), lhs, rhs)
        }
        &Expr::Let(var, init, body) => {
            let init = eval_expression(init, environment)?;
            environment.insert(var.clone(), init);
            eval_expression(body, environment)
        }
        &Expr::If(condition, then, els) => {
            let condition = eval_expression(condition, environment)?;
            match condition {
                ExprVal::Bool(true) => eval_expression(then, environment),
                ExprVal::Bool(false) => eval_expression(els, environment),
                _ => Err(TypeError::UnexpectedType(
                    "`if` condition must be bool".to_string(),
                )),
            }
        }
        &Expr::Fun(name, body) => unimplemented!(),
        &Expr::Apply(_, _) => unimplemented!(),
    }
}

fn apply_operator(op: BinOpKind, lhs: ExprVal, rhs: ExprVal) -> Result<ExprVal, TypeError> {
    match (op, lhs, rhs) {
        (BinOpKind::Add, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::U64(n + m)),
        (BinOpKind::Mul, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::U64(n * m)),
        (BinOpKind::Lt, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::Bool(n < m)),
        (BinOpKind::Gt, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::Bool(n > m)),
        _ => Err(TypeError::UnsupportedOperandType(
            "Both arguments must be u64".to_string(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::eval::eval;
    use crate::eval::{ExprVal, TypeError};
    use crate::interpret;
    use crate::lexer::lexer::Lexer;
    use crate::parser::parser::Parser;

    #[test]
    fn test_eval_let() -> Result<(), TypeError> {
        let source_code =
            "let a = 2 in let b = 3 in if a < b then if a > b then 1 else (a + b) * 4 else 4;;";
        let result = interpret!(source_code)?;
        assert_eq!(result, vec![ExprVal::U64(20)],);
        Ok(())
    }
}
