use crate::parser::{BinOpKind, Expr, Substitutions, Type};
use std::collections::{HashMap, VecDeque};

pub type TypeEnv = HashMap<String, Type>;

type Restrictions = VecDeque<(Type, Type)>;

// Convert type substitutions into type restrictions.
fn subst_to_restr(substitutions: Substitutions) -> Restrictions {
    substitutions
        .iter()
        .map(|(count, ty)| (Type::TyVar(*count), ty.clone()))
        .collect()
}

/// Apply given type substitutions to each type restrictions.
fn map_substitute(restrictions: &Restrictions, substitutions: &Substitutions) -> Restrictions {
    restrictions
        .iter()
        .map(|(ty1, ty2)| {
            (
                ty1.substitute_type(&substitutions),
                ty2.substitute_type(&substitutions),
            )
        })
        .collect()
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    DifferentType(String),
    NotBound(String),
    ViolateOccurCheck,
}

pub struct Typer<'a> {
    // Represent next type variable.
    counter: usize,
    vec_ast: &'a Vec<Expr>,
}

impl<'a> Typer<'a> {
    pub fn new(vec_ast: &'a Vec<Expr>) -> Self {
        Self {
            counter: 0,
            vec_ast,
        }
    }

    pub fn infer_type(&mut self) -> Result<(), TypeError> {
        let mut env: HashMap<String, Type> = HashMap::new();
        for ast in self.vec_ast {
            self.typing_expression(ast, &mut env)?;
        }
        Ok(())
    }

    fn typing_expression(
        &mut self,
        expr: &Expr,
        environment: &mut TypeEnv,
    ) -> Result<(Substitutions, Type), TypeError> {
        match &expr {
            &Expr::Var(var) => match environment.get(var) {
                Some(var_type) => Ok((vec![], var_type.clone())),
                None => Err(TypeError::NotBound(var.clone())),
            },
            &Expr::I64(_) => Ok((vec![], Type::TyI64)),
            &Expr::Bool(_) => Ok((vec![], Type::TyBool)),
            &Expr::BinOp(op, lhs, rhs) => {
                let (subst_l, lhs) = self.typing_expression(lhs, environment)?;
                let (subst_r, rhs) = self.typing_expression(rhs, environment)?;
                let (mut restrictions, binop_type) = Self::typing_operator(op.clone(), lhs, rhs);
                restrictions.append(&mut subst_to_restr(subst_l));
                restrictions.append(&mut subst_to_restr(subst_r));
                let mut unified_subst = Self::unify(&mut restrictions)?;
                let binop_type = binop_type.substitute_type(&mut unified_subst);
                Ok((unified_subst, binop_type))
            }
            &Expr::Let(var, init, body) => {
                let (_, init) = self.typing_expression(init, environment)?;
                environment.insert(var.clone(), init);
                let (_, body) = self.typing_expression(body, environment)?;
                Ok((vec![], body))
            }
            &Expr::LetRec(var, arg, init, body) => {
                let arg_type = self.fresh_type_var();
                let dummy_init = Type::TyFun(
                    Box::new(arg_type.clone()),
                    Box::new(self.fresh_type_var()),
                );
                // environment.insert(arg.clone(), arg_type);
                environment.insert(var.clone(), dummy_init);
                let (_, init) = self.typing_expression(init, environment)?;
                environment.remove(var);
                environment.remove(arg);
                environment.insert(var.clone(), init);
                self.typing_expression(body, environment)
            }
            &Expr::If(cond, then, els) => {
                let (subst_cond, cond) = self.typing_expression(cond, environment)?;
                let (subst_then, then) = self.typing_expression(then, environment)?;
                let (subst_els, els) = self.typing_expression(els, environment)?;
                let mut restrictions = VecDeque::new();
                restrictions.append(&mut subst_to_restr(subst_cond));
                restrictions.append(&mut subst_to_restr(subst_then));
                restrictions.append(&mut subst_to_restr(subst_els));
                restrictions.push_back((cond, Type::TyBool));
                restrictions.push_back((then.clone(), els));
                let mut unified_subst = Self::unify(&mut restrictions)?;
                let if_body_type = then.substitute_type(&mut unified_subst);
                Ok((unified_subst, if_body_type))
            }
            &Expr::Fun(arg, body) => {
                let arg_type = self.fresh_type_var();
                environment.insert(arg.clone(), arg_type.clone());
                let (mut subst_body, body_type) = self.typing_expression(body, environment)?;
                let arg_type = arg_type.substitute_type(&mut subst_body);
                Ok((
                    subst_body,
                    Type::TyFun(Box::new(arg_type), Box::new(body_type)),
                ))
            }
            &Expr::Apply(fun, arg) => {
                let (_, fun) = self.typing_expression(fun, environment)?;
                let (_, arg) = self.typing_expression(arg, environment)?;
                match fun.clone() {
                    Type::TyFun(ty1, ty2) => {
                        let mut restrictions = VecDeque::new();
                        restrictions.push_back((arg, *ty1));
                        let mut unified_subst = Self::unify(&mut restrictions)?;
                        let apply_type = ty2.substitute_type(&mut unified_subst);
                        Ok((unified_subst, apply_type))
                    }
                    // Case such as `let f x y = x y in ...`.
                    Type::TyVar(ty_var) => Ok((
                        vec![],
                        Type::TyFun(Box::new(Type::TyVar(ty_var)), Box::new(arg)),
                    )),
                    _ => Err(TypeError::DifferentType(format!(
                        "Expected function, but got {:?}",
                        fun
                    ))),
                }
            }
            _ => unimplemented!(),
        }
    }

    // When called, return new type variable with current counter value
    // and increment counter.
    fn fresh_type_var(&mut self) -> Type {
        let v = self.counter;
        self.counter += 1;
        Type::TyVar(v)
    }

    fn typing_operator(op: BinOpKind, lhs: Type, rhs: Type) -> (Restrictions, Type) {
        let restrictions =
            VecDeque::from(vec![(lhs.clone(), Type::TyI64), (rhs.clone(), Type::TyI64)]);
        match op {
            BinOpKind::Add => (restrictions, Type::TyI64),
            BinOpKind::Sub => (restrictions, Type::TyI64),
            BinOpKind::Mul => (restrictions, Type::TyI64),
            BinOpKind::Lt => (restrictions, Type::TyBool),
            BinOpKind::Gt => (restrictions, Type::TyBool),
        }
    }

    /// Solve unification problem on given type restrictions and return substitutions as a solution.
    fn unify(restrictions: &mut Restrictions) -> Result<Substitutions, TypeError> {
        match restrictions.pop_front() {
            None => Ok(Substitutions::new()),
            Some((ty1, ty2)) if ty1 == ty2 => Self::unify(restrictions),
            Some((ty1, ty2)) => match (ty1, ty2) {
                (Type::TyFun(arg1, body1), Type::TyFun(arg2, body2)) => {
                    restrictions.push_back((*arg1, *arg2));
                    restrictions.push_back((*body1, *body2));
                    Self::unify(restrictions)
                }
                (Type::TyVar(ty_var), ty) | (ty, Type::TyVar(ty_var)) => {
                    let free_type_vars = ty.get_free_type_vars();
                    match free_type_vars.get(&ty_var) {
                        Some(_) => Err(TypeError::ViolateOccurCheck),
                        None => {
                            let mut substitution = vec![(ty_var, ty.clone())];
                            let mut restrictions = map_substitute(restrictions, &substitution);
                            let mut rest_substitution = Self::unify(&mut restrictions)?;
                            substitution.append(&mut rest_substitution);
                            Ok(substitution)
                        }
                    }
                }
                _ => Err(TypeError::DifferentType("".to_string())),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::parser::parser::Parser;
    use crate::parser::typing::{TypeError, Typer};

    /// Parse source code and check if type inference is done successfully.
    fn typing_process(source_code: &str) -> Result<(), TypeError> {
        let mut lexer = Lexer::new(source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let asts = parser.parse().unwrap();
        let mut typer = Typer::new(&asts);
        typer.infer_type()
    }

    #[test]
    fn typing_function_definition() {
        let source_code = "fun x -> fun y -> x + y + 1;;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn typing_let() {
        let source_code = "let a = 1 in let b = 2 in a + b * 3;;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn typing_not_bound_variable() {
        let source_code = "let a = 1 in x + 42;;";
        let result = typing_process(source_code);
        assert_eq!(result, Err(TypeError::NotBound("x".to_string())));
    }

    #[test]
    fn typing_if_else() {
        let source_code =
            "fun x -> fun y -> if x < y then if x > y then x + y else x - y else x * y;;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn typing_different_type_if_body() {
        let source_code =
            "fun x -> fun y -> if x < y then if x > y then x + y else true else x * y;;";
        let result = typing_process(source_code);
        assert_eq!(result, Err(TypeError::DifferentType("".to_string())));
    }

    #[test]
    fn typing_invalid_if_condition() {
        let source_code = "fun x -> fun y -> if x then if x > y then x + y else x - y else x * y;;";
        let result = typing_process(source_code);
        assert_eq!(result, Err(TypeError::DifferentType("".to_string())));
    }

    #[test]
    fn typing_func_apply() {
        let source_code = "let f x = x * 2 in let g y = y + 3 in let a = 1 in f (g a);;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn typing_invalid_func_apply() {
        let source_code = "let f x = x * 2 in let g y = false in let a = 1 in f (g a);;";
        let result = typing_process(source_code);
        assert_eq!(result, Err(TypeError::DifferentType("".to_string())));
    }

    #[test]
    fn typing_func_apply_as_type_variable() {
        let source_code = "let f x y = x y in let g z = z * 2 in f g 2;;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn typing_let_rec() {
        let source_code = "let rec f x = if x > 0 then 1 + f (x - 1) else 0 in let x = 1 in f x;;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }
}
