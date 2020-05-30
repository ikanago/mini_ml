use std::collections::{HashMap, HashSet, VecDeque};

pub type TypeEnv = HashMap<String, Type>;

// Type substitution rules of type variable.
pub type Substitutions = Vec<(usize, Type)>;

#[derive(Clone, Debug, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Lt,
    Gt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var(String),
    I64(i64),
    Bool(bool),
    Array(VecDeque<Expr>),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    Fun(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Nil,
    Cons(String, String),
    Var(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TyI64,
    TyBool,
    TyVar(usize),
    TyFun(Box<Type>, Box<Type>),
}

impl Type {
    fn get_free_type_vars(&self) -> HashSet<usize> {
        fn inner(ty: &Type, free_type_vars: &mut HashSet<usize>) {
            match &ty {
                &Type::TyI64 => (),
                &Type::TyBool => (),
                &Type::TyVar(n) => {
                    free_type_vars.insert(*n);
                }
                &Type::TyFun(ty1, ty2) => {
                    inner(ty1, free_type_vars);
                    inner(ty2, free_type_vars);
                }
            };
        }
        let mut free_type_vars = HashSet::new();
        inner(&self, &mut free_type_vars);
        free_type_vars
    }

    fn substitute_type(&self, substitutions: &Substitutions) -> Type {
        match &self {
            &Type::TyI64 => Type::TyI64,
            &Type::TyBool => Type::TyBool,
            &Type::TyVar(n) => {
                let mut substituted_type = Type::TyVar(*n);
                for (ty_var_count, ty) in substitutions {
                    if n == ty_var_count {
                        substituted_type = ty.clone();
                    }
                }
                substituted_type.substitute_type(substitutions)
            }
            &Type::TyFun(ty1, ty2) => {
                let ty1 = ty1.substitute_type(substitutions);
                let ty2 = ty2.substitute_type(substitutions);
                Type::TyFun(Box::new(ty1), Box::new(ty2))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::syntax::*;

    #[test]
    fn test_substitution1() {
        let alpha = Type::TyVar(0);
        let fun_type = Type::TyFun(Box::new(alpha), Box::new(Type::TyBool));
        assert_eq!(
            fun_type.substitute_type(&vec![(0, Type::TyI64)]),
            Type::TyFun(Box::new(Type::TyI64), Box::new(Type::TyBool))
        )
    }

    #[test]
    fn test_substitution2() {
        let alpha = Type::TyVar(0);
        let beta = Type::TyVar(1);
        assert_eq!(
            beta.substitute_type(&vec![
                (1, Type::TyFun(Box::new(alpha), Box::new(Type::TyI64))),
                (0, Type::TyBool)
            ]),
            Type::TyFun(Box::new(Type::TyBool), Box::new(Type::TyI64))
        )
    }
}
