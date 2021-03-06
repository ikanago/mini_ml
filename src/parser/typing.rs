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
    // Mapping variable names to their types.
    environment: TypeEnv,
}

impl<'a> Typer<'a> {
    pub fn new(vec_ast: &'a Vec<Expr>) -> Self {
        Self {
            counter: 0,
            vec_ast,
            environment: TypeEnv::new(),
        }
    }

    pub fn infer_type(&mut self) -> Result<(), TypeError> {
        for ast in self.vec_ast {
            self.typing_expression(ast)?;
            self.environment = TypeEnv::new();
        }
        Ok(())
    }

    fn typing_expression(&mut self, expr: &Expr) -> Result<(Substitutions, Type), TypeError> {
        match &expr {
            &Expr::Var(var) => self.typing_var(var),
            &Expr::I64(_) => Ok((vec![], Type::TyI64)),
            &Expr::Bool(_) => Ok((vec![], Type::TyBool)),
            &Expr::Nil => self.typing_nil(),
            &Expr::Cons(lhs, rhs) => self.typing_cons(lhs, rhs),
            &Expr::BinOp(op, lhs, rhs) => self.typing_binop(op, lhs, rhs),
            &Expr::Let(var, init, body) => self.typing_let(var, init, body),
            &Expr::LetRec(var, arg, init, body) => self.typing_let_rec(var, arg, init, body),
            &Expr::If(cond, then, els) => self.typing_if(cond, then, els),
            &Expr::Match(cond, patterns) => self.typing_match(cond, patterns),
            &Expr::Fun(arg, body) => self.typing_fun(arg, body),
            &Expr::Apply(fun, arg) => self.typing_apply_fun(fun, arg),
        }
    }

    fn typing_var(&mut self, var: &str) -> Result<(Substitutions, Type), TypeError> {
        match self.environment.get(var) {
            Some(var_type) => Ok((vec![], var_type.clone())),
            None => Err(TypeError::NotBound(var.to_string())),
        }
    }

    fn typing_nil(&mut self) -> Result<(Substitutions, Type), TypeError> {
        let ty_var = self.fresh_type_var();
        Ok((vec![], Type::TyList(Box::new(ty_var))))
    }

    fn typing_cons(
        &mut self,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let (_, lhs) = self.typing_expression(lhs)?;
        let (_, rhs) = self.typing_expression(rhs)?;
        match rhs {
            Type::TyList(ty) => {
                let mut restrictions = VecDeque::from(vec![(lhs.clone(), *ty)]);
                let unified_subst = Self::unify(&mut restrictions)?;
                Ok((unified_subst, Type::TyList(Box::new(lhs))))
            }
            Type::TyVar(n) => {
                let list_type = Type::TyList(Box::new(lhs));
                let mut restrictions = VecDeque::from(vec![(Type::TyVar(n), list_type.clone())]);
                let unified_subst = Self::unify(&mut restrictions)?;
                Ok((unified_subst, list_type))
            }
            _ => Err(TypeError::DifferentType(format!(
                "Expected array but got {:?}",
                rhs
            ))),
        }
    }

    fn typing_binop(
        &mut self,
        op: &BinOpKind,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let (_, lhs) = self.typing_expression(lhs)?;
        let (_, rhs) = self.typing_expression(rhs)?;
        let (mut restrictions, binop_type) = Self::typing_operator(op.clone(), lhs, rhs);
        let mut unified_subst = Self::unify(&mut restrictions)?;
        let binop_type = binop_type.substitute_type(&mut unified_subst);
        Ok((unified_subst, binop_type))
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

    fn typing_let(
        &mut self,
        var: &str,
        init: &Box<Expr>,
        body: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let (_, init) = self.typing_expression(init)?;
        self.environment.insert(var.to_string(), init);
        let (_, body) = self.typing_expression(body)?;
        Ok((vec![], body))
    }

    fn typing_let_rec(
        &mut self,
        var: &str,
        arg: &str,
        init: &Box<Expr>,
        body: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let arg_type = self.fresh_type_var();
        let dummy_init = Type::TyFun(Box::new(arg_type.clone()), Box::new(self.fresh_type_var()));
        // environment.insert(arg.clone(), arg_type);
        self.environment.insert(var.to_string(), dummy_init);
        let (_, init) = self.typing_expression(init)?;
        self.environment.remove(var);
        self.environment.remove(arg);
        self.environment.insert(var.to_string(), init);
        self.typing_expression(body)
    }

    fn typing_if(
        &mut self,
        cond: &Box<Expr>,
        then: &Box<Expr>,
        els: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let (subst_cond, cond) = self.typing_expression(cond)?;
        let (subst_then, then) = self.typing_expression(then)?;
        let (subst_els, els) = self.typing_expression(els)?;
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

    fn typing_match(
        &mut self,
        cond: &Box<Expr>,
        patterns: &Vec<(Expr, Expr)>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let (_, cond) = self.typing_expression(cond)?;
        self.typing_match_pattern_array(cond, patterns)
    }

    fn typing_match_pattern_array(
        &mut self,
        cond_type: Type,
        patterns: &Vec<(Expr, Expr)>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let mut restrictions = Restrictions::new();
        let mut arm_types = Vec::new();
        for (pattern, arm) in patterns.iter() {
            let arm_type = self.match_patterns(&cond_type, pattern, arm, &mut restrictions)?;
            arm_types.push(arm_type);
        }
        if arm_types.len() < 2 {
            return Err(TypeError::DifferentType("".to_string()));
        }
        let arm_restrictions = vec_to_tuples(&arm_types);
        restrictions.append(&mut VecDeque::from(arm_restrictions));
        let unified_subst = Self::unify(&mut restrictions)?;
        Ok((unified_subst, arm_types.first().unwrap().clone()))
    }

    fn match_patterns(
        &mut self,
        cond_type: &Type,
        pattern: &Expr,
        arm: &Expr,
        restrictions: &mut Restrictions,
    ) -> Result<Type, TypeError> {
        match pattern.clone() {
            Expr::Nil => {
                let (arm_subst, arm) = self.typing_expression(arm)?;
                restrictions.append(&mut subst_to_restr(arm_subst));
                Ok(arm)
            }
            Expr::Cons(head, tail) => match (*head, *tail) {
                (Expr::Var(head), Expr::Var(tail)) => {
                    let head_type = self.fresh_type_var();
                    self.environment.insert(head, head_type);
                    self.environment.insert(tail, cond_type.clone());
                    let (arm_subst, arm) = self.typing_expression(arm)?;
                    restrictions.append(&mut subst_to_restr(arm_subst));
                    Ok(arm)
                }
                (Expr::Var(head), Expr::Nil) => {
                    let head_type = self.fresh_type_var();
                    self.environment.insert(head, head_type);
                    let (arm_subst, arm) = self.typing_expression(arm)?;
                    restrictions.append(&mut subst_to_restr(arm_subst));
                    Ok(arm)
                }
                (Expr::Var(head), Expr::Cons(x, y)) => {
                    let head_type = self.fresh_type_var();
                    self.environment.insert(head, head_type);
                    self.match_patterns(cond_type, &Expr::Cons(x, y), arm, restrictions)
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn typing_fun(
        &mut self,
        arg: &str,
        body: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let arg_type = self.fresh_type_var();
        self.environment.insert(arg.to_string(), arg_type.clone());
        let (mut subst_body, body_type) = self.typing_expression(body)?;
        let arg_type = arg_type.substitute_type(&mut subst_body);
        Ok((
            subst_body,
            Type::TyFun(Box::new(arg_type), Box::new(body_type)),
        ))
    }

    fn typing_apply_fun(
        &mut self,
        fun: &Box<Expr>,
        arg: &Box<Expr>,
    ) -> Result<(Substitutions, Type), TypeError> {
        let (subst_fun, fun) = self.typing_expression(fun)?;
        let (subst_arg, arg) = self.typing_expression(arg)?;
        match fun.clone() {
            Type::TyFun(ty1, ty2) => {
                let mut restrictions = VecDeque::new();
                restrictions.push_back((arg, *ty1));
                restrictions.append(&mut subst_to_restr(subst_fun));
                restrictions.append(&mut subst_to_restr(subst_arg));
                let mut unified_subst = Self::unify(&mut restrictions)?;
                let apply_type = ty2.substitute_type(&mut unified_subst);
                Ok((unified_subst, apply_type))
            }
            // Case such as `let f x y = x y in ...`.
            Type::TyVar(ty_var) => {
                let mut restrictions = VecDeque::new();
                let apply_type = self.fresh_type_var();
                restrictions.push_back((Type::TyVar(ty_var), Type::TyFun(Box::new(arg), Box::new(apply_type.clone()))));
                let mut unified_subst = Self::unify(&mut restrictions)?;
                let apply_type = apply_type.substitute_type(&mut unified_subst);
                Ok((
                    unified_subst,
                    apply_type,
                ))
            }
            _ => Err(TypeError::DifferentType(format!(
                "Expected function, but got {:?}",
                fun
            ))),
        }
    }

    // When called, return new type variable with current counter value
    // and increment counter.
    fn fresh_type_var(&mut self) -> Type {
        let v = self.counter;
        self.counter += 1;
        Type::TyVar(v)
    }

    /// Solve unification problem on given type restrictions and return substitutions as a solution.
    fn unify(restrictions: &mut Restrictions) -> Result<Substitutions, TypeError> {
        match restrictions.pop_front() {
            None => Ok(Substitutions::new()),
            Some((ty1, ty2)) if ty1 == ty2 => Self::unify(restrictions),
            Some((ty1, ty2)) => match (ty1.clone(), ty2.clone()) {
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
                (Type::TyList(ty1), Type::TyList(ty2)) => {
                    restrictions.push_back((*ty1, *ty2));
                    Self::unify(restrictions)
                }
                _ => Err(TypeError::DifferentType(format!("{:?}, {:?}", ty1, ty2))),
            },
        }
    }
}

fn vec_to_tuples<T: Clone>(vector: &Vec<T>) -> Vec<(T, T)> {
    let mut tuples_vec = Vec::new();
    for i in 0..vector.len() - 1 {
        tuples_vec.push((vector[i].clone(), vector[i + 1].clone()));
    }
    tuples_vec
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
        assert_eq!(
            result,
            Err(TypeError::DifferentType("TyI64, TyBool".to_string()))
        );
    }

    #[test]
    fn typing_invalid_if_condition() {
        let source_code = "fun x -> fun y -> if x then if x > y then x + y else x - y else x * y;;";
        let result = typing_process(source_code);
        assert_eq!(
            result,
            Err(TypeError::DifferentType("TyI64, TyBool".to_string()))
        );
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
        assert_eq!(
            result,
            Err(TypeError::DifferentType("TyBool, TyI64".to_string()))
        );
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

    #[test]
    fn typing_match_integer_list() {
        let source_code = "let a = [1; 2; 3;] in let rec len x = match x with | [] -> 0 | head::tail -> 1 + len tail in len a;;";
        let result = typing_process(source_code);
        assert_eq!(result, Ok(()));
    }
}
