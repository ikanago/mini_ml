use crate::parser::syntax::{BinOpKind, Expr, Substitutions, Type, TypeEnv};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    DifferentType,
    NotBound,
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

    fn typing_expression(expr: &Expr, environment: &mut TypeEnv) -> Result<Type, TypeError> {
        match &expr {
            &Expr::Var(var) => match environment.get(var) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError::NotBound),
            },
            &Expr::I64(_) => Ok(Type::TyI64),
            &Expr::Bool(_) => Ok(Type::TyBool),
            &Expr::BinOp(op, lhs, rhs) => {
                let lhs = Self::typing_expression(lhs, environment)?;
                let rhs = Self::typing_expression(rhs, environment)?;
                Self::typing_operator(op.clone(), lhs, rhs)
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

    fn typing_operator(op: BinOpKind, lhs: Type, rhs: Type) -> Result<Type, TypeError> {
        match op {
            BinOpKind::Add => match (lhs, rhs) {
                (Type::TyI64, Type::TyI64) => Ok(Type::TyI64),
                _ => Err(TypeError::DifferentType),
            },
            BinOpKind::Sub => match (lhs, rhs) {
                (Type::TyI64, Type::TyI64) => Ok(Type::TyI64),
                _ => Err(TypeError::DifferentType),
            },
            BinOpKind::Mul => match (lhs, rhs) {
                (Type::TyI64, Type::TyI64) => Ok(Type::TyI64),
                _ => Err(TypeError::DifferentType),
            },
            _ => unimplemented!(),
        }
    }
}
