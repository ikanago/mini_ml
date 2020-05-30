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
    DifferentType,
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

    pub fn infer_type(&self) -> Result<(), TypeError> {
        let mut env: HashMap<String, Type> = HashMap::new();
        env.insert("a".to_string(), Type::TyI64);
        env.insert("b".to_string(), Type::TyI64);
        for ast in self.vec_ast {
            Self::typing_expression(ast, &mut env)?;
        }
        Ok(())
    }

    fn typing_expression(
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
                let (subst_l, lhs) = Self::typing_expression(lhs, environment)?;
                let (subst_r, rhs) = Self::typing_expression(rhs, environment)?;
                let (mut restrictions, binop_type) = Self::typing_operator(op.clone(), lhs, rhs)?;
                restrictions.append(&mut subst_to_restr(subst_l));
                restrictions.append(&mut subst_to_restr(subst_r));
                let unified_subst = Self::unify(&mut restrictions)?;
                let binop_type = binop_type.substitute_type(&unified_subst);
                Ok((unified_subst, binop_type))
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

    fn typing_operator(
        op: BinOpKind,
        lhs: Type,
        rhs: Type,
    ) -> Result<(Restrictions, Type), TypeError> {
        let restrictions =
            VecDeque::from(vec![(lhs.clone(), Type::TyI64), (rhs.clone(), Type::TyI64)]);
        match op {
            BinOpKind::Add => match (lhs, rhs) {
                (Type::TyI64, Type::TyI64) => Ok((restrictions, Type::TyI64)),
                _ => Err(TypeError::DifferentType),
            },
            BinOpKind::Sub => match (lhs, rhs) {
                (Type::TyI64, Type::TyI64) => Ok((restrictions, Type::TyI64)),
                _ => Err(TypeError::DifferentType),
            },
            BinOpKind::Mul => match (lhs, rhs) {
                (Type::TyI64, Type::TyI64) => Ok((restrictions, Type::TyI64)),
                _ => Err(TypeError::DifferentType),
            },
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
                            let substitution = vec![(ty_var, ty.clone())];
                            let mut restrictions: Restrictions =
                                map_substitute(restrictions, &substitution);
                            restrictions.push_back((Type::TyVar(ty_var), ty));
                            Self::unify(&mut restrictions)
                        }
                    }
                }
                _ => Err(TypeError::DifferentType),
            },
        }
    }
}
