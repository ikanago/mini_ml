use crate::eval::{EvalError, ExprVal};
use crate::parser::syntax::{BinOpKind, Expr};
use std::collections::HashMap;

pub fn eval(vec_ast: &[Expr]) -> Result<Vec<ExprVal>, EvalError> {
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
) -> Result<ExprVal, EvalError> {
    match &ast {
        &Expr::Var(var) => match environment.get(var) {
            Some(expr) => Ok(expr.clone()),
            None => Err(EvalError::NotBound(var.clone())),
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
                _ => Err(EvalError::UnexpectedType(
                    "`if` condition must be bool".to_string(),
                )),
            }
        }
        &Expr::Fun(arg, body) => Ok(ExprVal::ProcV(
            arg.clone(),
            body.clone(),
            environment.clone(),
        )),
        &Expr::Apply(fun, arg) => {
            let fun = eval_expression(fun, environment)?;
            let arg = eval_expression(arg, environment)?;
            match fun {
                ExprVal::ProcV(arg_name, body, mut captured_env) => {
                    captured_env.insert(arg_name, arg);
                    eval_expression(&body, &mut captured_env)
                }
                _ => Err(EvalError::UnexpectedType("Not a function".to_string())),
            }
        }
    }
}

fn apply_operator(op: BinOpKind, lhs: ExprVal, rhs: ExprVal) -> Result<ExprVal, EvalError> {
    match (op, lhs, rhs) {
        (BinOpKind::Add, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::U64(n + m)),
        (BinOpKind::Sub, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::U64(n - m)),
        (BinOpKind::Mul, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::U64(n * m)),
        (BinOpKind::Lt, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::Bool(n < m)),
        (BinOpKind::Gt, ExprVal::U64(n), ExprVal::U64(m)) => Ok(ExprVal::Bool(n > m)),
        _ => Err(EvalError::UnsupportedOperandType(
            "Both arguments must be u64".to_string(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::{EvalError, ExprVal};
    use crate::interpret;

    #[test]
    fn test_eval_let() -> Result<(), EvalError> {
        let source_code =
            "let a = 2 in let b = 4 in if a < b then if a > b then 1 else (b - a) * 4 else 4;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::U64(8)],);
        Ok(())
    }

    #[test]
    fn test_eval_fun() -> Result<(), EvalError> {
        let source_code = "let apply = fun f -> fun x -> fun y -> if x < y then f x + y else f x * y in apply (fun x -> x + 1) 5 3;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::U64(18)],);

        let source_code = "let apply = fun f x y -> if x < y then f x + y else f x * y in apply (fun x -> x + 1) 5 3;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::U64(18)],);

        let source_code =
            "let apply f x y = if x < y then f x + y else f x * y in apply (fun x -> x + 1) 5 3;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::U64(18)],);
        Ok(())
    }

    #[test]
    #[should_panic]
    fn test_invalid_fun_definition() {
        let source_code = "let f = fun x y = x + y in f 1 2;;";
        interpret(source_code, false, false);
        let source_code = "let f x y -> x + y in f 1 2;;";
        interpret(source_code, false, false);
    }
}
