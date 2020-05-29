use crate::parser::syntax::{BinOpKind, Expr, Type};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    DifferentType,
    NotBound,
}

type TypeEnv = HashMap<String, Type>;

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

pub fn typing_expression(expr: &Expr, environment: &mut TypeEnv) -> Result<Type, TypeError> {
    match &expr {
        &Expr::Var(var) => match environment.get(var) {
            Some(var_type) => Ok(var_type.clone()),
            None => Err(TypeError::NotBound),
        },
        &Expr::I64(_) => Ok(Type::TyI64),
        &Expr::Bool(_) => Ok(Type::TyBool),
        &Expr::BinOp(op, lhs, rhs) => {
            let lhs = typing_expression(lhs, environment)?;
            let rhs = typing_expression(rhs, environment)?;
            typing_operator(op.clone(), lhs, rhs)
        }
        _ => unimplemented!(),
    }
}
