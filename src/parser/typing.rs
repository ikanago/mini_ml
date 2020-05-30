use crate::parser::syntax::{BinOpKind, Expr, Substitutions, Type, TypeEnv};
use std::collections::{HashMap, VecDeque};

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
    NotBound,
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
        env.insert("a".to_string(), Type::TyI64);
        env.insert("b".to_string(), Type::TyI64);
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
                None => Err(TypeError::NotBound),
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
            &Expr::Fun(arg, body) => {
                let arg_type = self.fresh_type_var();
                environment.insert(arg.clone(), arg_type.clone());
                let (mut substitutions, body_type) = self.typing_expression(body, environment)?;
                let arg_type = arg_type.substitute_type(&mut substitutions);
                Ok((
                    substitutions,
                    Type::TyFun(Box::new(arg_type), Box::new(body_type)),
                ))
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
            _ => unimplemented!(),
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
    use crate::parser::typing::{Typer, TypeError};

    #[test]
    fn typing_binary_operator1() {
        let source_code = "a - b * 42 + 42;;";
        let mut lexer = Lexer::new(source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let asts = parser.parse().unwrap();
        let mut typer = Typer::new(&asts);
        assert_eq!(typer.infer_type(), Ok(()));
    }

    #[test]
    fn typing_not_bound_variable() {
        let source_code = "x + 42;;";
        let mut lexer = Lexer::new(source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let asts = parser.parse().unwrap();
        let mut typer = Typer::new(&asts);
        assert_eq!(typer.infer_type(), Err(TypeError::NotBound));
    }

    #[test]
    fn typing_function_definition() {
        let source_code = "fun x -> fun y -> x + y + 1;;";
        let mut lexer = Lexer::new(source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let asts = parser.parse().unwrap();
        let mut typer = Typer::new(&asts);
        assert_eq!(typer.infer_type(), Ok(()));
    }
}
