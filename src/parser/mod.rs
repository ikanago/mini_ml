pub mod parser;
pub mod typing;

use std::collections::{HashSet, VecDeque};
use std::convert::From;

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
    Nil,
    Cons(Box<Expr>, Box<Expr>),
    Array(VecDeque<Expr>),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Expr, Expr)>),
    Fun(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Nil,
    Cons(String, String),
    Var(String),
}

impl From<Expr> for Pattern {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Nil => Pattern::Nil,
            Expr::Cons(lhs, rhs) => {
                match (*lhs, *rhs) {
                    (Expr::Var(lhs), Expr::Var(rhs)) => Pattern::Cons(lhs, rhs),
                    _ => unimplemented!(),
                }
            }
            Expr::Var(var) => Pattern::Var(var),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    Eof,
    NonTerminalSymbol,
    UnexpectedToken,
    UnclosedParen,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TyI64,
    TyBool,
    TyArray(Box<Type>),
    TyVar(usize),
    TyFun(Box<Type>, Box<Type>),
}

impl Type {
    // Get unresolved type variables inside the type.
    pub fn get_free_type_vars(&self) -> HashSet<usize> {
        fn inner(ty: &Type, free_type_vars: &mut HashSet<usize>) {
            match &ty {
                &Type::TyI64 => (),
                &Type::TyBool => (),
                &Type::TyArray(ty) => inner(ty, free_type_vars),
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

    /// Apply type substitution to self.
    pub fn substitute_type(&self, substitutions: &Substitutions) -> Type {
        match &self {
            &Type::TyI64 => Type::TyI64,
            &Type::TyBool => Type::TyBool,
            &Type::TyArray(ty) => {
                let ty = ty.substitute_type(substitutions);
                Type::TyArray(Box::new(ty))
            }
            &Type::TyVar(n) => {
                substitutions
                    .iter()
                    .fold(Type::TyVar(*n), |acc, (ty_var_count, ty)| {
                        if n == ty_var_count {
                            ty.substitute_type(substitutions)
                        } else {
                            acc
                        }
                    })
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
    use crate::parser::Type;

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
