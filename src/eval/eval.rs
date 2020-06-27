use crate::eval::{Env, EvalError, ExprVal};
use crate::parser::{BinOpKind, Expr};
use std::collections::{HashMap, VecDeque};

pub fn eval(vec_ast: &[Expr]) -> Result<Vec<ExprVal>, EvalError> {
    let mut result = vec![];
    for ast in vec_ast.iter() {
        let mut environment: Env = HashMap::new();
        let expr = eval_expression(&ast, &mut environment)?;
        result.push(expr);
    }
    Ok(result)
}

fn eval_expression(ast: &Expr, environment: &mut Env) -> Result<ExprVal, EvalError> {
    match &ast {
        &Expr::Var(var) => match environment.get(var) {
            Some(expr) => Ok(expr.clone()),
            None => Err(EvalError::NotBound(var.clone())),
        },
        &Expr::I64(n) => Ok(ExprVal::I64(*n)),
        &Expr::Bool(b) => Ok(ExprVal::Bool(*b)),
        &Expr::Nil | &Expr::Cons(_, _) => unimplemented!(),
        &Expr::Array(array) => {
            let mut evaled_array = VecDeque::new();
            for element in array {
                evaled_array.push_back(eval_expression(element, environment)?);
            }
            Ok(ExprVal::Array(evaled_array))
        }
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
        &Expr::LetRec(var, arg, init, body) => {
            let init = match eval_expression(init, environment) {
                Ok(ExprVal::Closure(arg, body, env)) => {
                    ExprVal::ClosureRec(var.clone(), arg, body, env)
                }
                Ok(init) => init,
                Err(err) => return Err(err),
            };
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
        &Expr::Match(condition, patterns) => {
            let condition = eval_expression(condition, environment)?;
            match condition {
                ExprVal::Array(mut array) => {
                    match_pattern_array(&mut array, patterns.clone(), environment)
                }
                _ => unimplemented!(),
            }
        }
        &Expr::Fun(arg, body) => Ok(ExprVal::Closure(
            arg.clone(),
            body.clone(),
            environment.clone(),
        )),
        &Expr::Apply(fun, arg) => {
            let fun = eval_expression(fun, environment)?;
            let arg = eval_expression(arg, environment)?;
            match fun.clone() {
                ExprVal::Closure(arg_name, body, mut captured_env) => {
                    captured_env.insert(arg_name, arg);
                    eval_expression(&body, &mut captured_env)
                }
                ExprVal::ClosureRec(closure_name, arg_name, body, mut captured_env) => {
                    captured_env.insert(closure_name, fun);
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
        (BinOpKind::Add, ExprVal::I64(n), ExprVal::I64(m)) => Ok(ExprVal::I64(n + m)),
        (BinOpKind::Sub, ExprVal::I64(n), ExprVal::I64(m)) => Ok(ExprVal::I64(n - m)),
        (BinOpKind::Mul, ExprVal::I64(n), ExprVal::I64(m)) => Ok(ExprVal::I64(n * m)),
        (BinOpKind::Lt, ExprVal::I64(n), ExprVal::I64(m)) => Ok(ExprVal::Bool(n < m)),
        (BinOpKind::Gt, ExprVal::I64(n), ExprVal::I64(m)) => Ok(ExprVal::Bool(n > m)),
        _ => Err(EvalError::UnsupportedOperandType(
            "Both arguments must be i64".to_string(),
        )),
    }
}

/// Find a matching pattern for an array and execute its match arm.
fn match_pattern_array(
    array: &mut VecDeque<ExprVal>,
    patterns: Vec<(Expr, Expr)>,
    environment: &mut Env,
) -> Result<ExprVal, EvalError> {
    let mut patterns = patterns.iter();
    loop {
        match patterns.next() {
            Some((pattern, arm)) => match pattern.clone() {
                Expr::Nil if array.len() == 0 => break eval_expression(arm, environment),
                Expr::Cons(head, tail) => {
                    match (*head, *tail) {
                        (Expr::Var(head), Expr::Var(tail)) => {
                            environment.insert(head, array.pop_front().unwrap());
                            environment.insert(tail, ExprVal::Array(array.clone()));
                            break eval_expression(arm, environment);
                        }
                        _ => unimplemented!(),
                    }
                }
                _ => continue,
            },
            None => break Err(EvalError::PatternsNotExhaustive),
        }
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
        assert_eq!(result, vec![ExprVal::I64(8)],);
        Ok(())
    }

    #[test]
    fn test_eval_fun() {
        let source_code = "let apply = fun f -> fun x -> fun y -> if x < y then f x + y else f x * y in apply (fun x -> x + 1) 5 3;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::I64(18)],);

        let source_code = "let apply = fun f x y -> if x < y then f x + y else f x * y in apply (fun x -> x + 1) 5 3;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::I64(18)],);

        let source_code =
            "let apply f x y = if x < y then f x + y else f x * y in apply (fun x -> x + 1) 5 3;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::I64(18)],);
    }

    #[test]
    fn test_eval_rec_fun() {
        let source_code = "let rec fact x = if x > 0 then x * fact (x - 1) else 1 in fact 5;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::I64(120)],);
    }

    #[test]
    fn test_eval_list_pattern_matching() {
        let source_code = "let a = [1; 2; 3; 4; 5] in let rec len x = match x with | [] -> 0 | head::tail -> 1 + len tail in len a;;";
        let result = interpret(source_code, false, false);
        assert_eq!(result, vec![ExprVal::I64(5)],);
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
