use std::{
    collections::HashSet,
    ops::{Add, Div, Mul, Sub},
};

use crate::{FloatType, IntType};

/// An algebraic expression.  The type parameter `T` is the type of variables.  The parser stores variables by their names, as strings, then [`Solvr`] interns those strings and replaces them by int references.
#[derive(Clone, PartialEq, Debug)]
pub enum Expr<T> {
    Variable(T),
    Literal(Data),

    Neg(Box<Self>),

    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Data {
    Integer(IntType),
    Float(FloatType),
    Bool(bool), // Unused, but kept for pattern exhaustivity
    None,
}

impl Data {
    fn negate(&self) -> Data {
        match self {
            Data::Integer(v) => Data::Integer(-v),
            Data::Float(v) => Data::Float(-v),
            _ => panic!("Type error: -bool"),
        }
    }
    fn binary_op(
        &self,
        rhs: &Data,
        int_fn: fn(IntType, IntType) -> IntType,
        float_fn: fn(FloatType, FloatType) -> FloatType,
    ) -> Data {
        match self {
            Data::Integer(v) => Data::Integer(int_fn(*v, *rhs.as_integer().expect("Type error"))),
            Data::Float(v) => Data::Float(float_fn(*v, *rhs.as_float().expect("Type error"))),
            _ => panic!("Type error: binary_op on a bool"),
        }
    }

    pub fn as_integer(&self) -> Option<&IntType> {
        if let Self::Integer(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<&FloatType> {
        if let Self::Float(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EvalError {
    IncompatibleTypesError,
    UnboundVariable,
}

pub type EvalResult<T> = Result<T, EvalError>;
pub type EvalEmptyResult = Result<(), EvalError>;

/// A common trait for variable sets
pub trait Memory<T> {
    fn get_var(&self, id: T) -> Option<&Data>;
}

/// Empty memory bank
impl<T> Memory<T> for () {
    fn get_var(&self, _: T) -> Option<&Data> {
        panic!("Trying to read from the () memory.  This is a bug in solvr, please report it!");
    }
}

impl Memory<usize> for Vec<Data> {
    fn get_var(&self, id: usize) -> Option<&Data> {
        self.get(id)
    }
}

pub type TestEvalResult = EvalResult<Data>;

impl<T: Copy + std::hash::Hash + Eq> Expr<T> {
    /// Evaluate this expression, reading variables from `mem`.
    pub fn eval<M: Memory<T>>(&self, mem: &M) -> TestEvalResult {
        match self {
            Expr::Variable(i) => {
                if let Some(v) = mem.get_var(*i) {
                    Ok(*v)
                } else {
                    Err(EvalError::UnboundVariable)
                }
            }
            Expr::Literal(data) => Ok(*data),

            Expr::Neg(expr) => Ok(expr.eval(mem)?.negate()),
            Expr::Add(lhs, rhs) => {
                Ok(lhs
                    .eval(mem)?
                    .binary_op(&rhs.eval(mem)?, IntType::add, FloatType::add))
            }
            Expr::Sub(lhs, rhs) => {
                Ok(lhs
                    .eval(mem)?
                    .binary_op(&rhs.eval(mem)?, IntType::sub, FloatType::sub))
            }
            Expr::Mul(lhs, rhs) => {
                Ok(lhs
                    .eval(mem)?
                    .binary_op(&rhs.eval(mem)?, IntType::mul, FloatType::mul))
            }
            Expr::Div(lhs, rhs) => {
                Ok(lhs
                    .eval(mem)?
                    .binary_op(&rhs.eval(mem)?, IntType::div, FloatType::div))
            }
        }
    }

    pub fn simplify(&self) -> EvalResult<(bool, Expr<T>)> {
        use Expr::*;
        // If this expression is pure, evaluate it and replace it by its result.
        // FIXME: This may result in division by zero, so the return type must be a Result
        if self.is_pure() {
            return Ok((true, Literal(self.eval(&())?)));
        }
        match self {
            Variable(_) => Ok((false, self.clone())),
            Literal(_) => Ok((false, self.clone())),
            // --x == x
            Neg(expr) if expr.is_neg() => expr.as_neg().unwrap().clone().simplify(),
            x => Ok((false, x.clone())),
        }
    }

    /// Determine if all terms of this expression verify `pred`.
    pub fn all_terms(&self, pred: fn(&Expr<T>) -> bool) -> bool {
        match self {
            Expr::Variable(_) => pred(self),
            Expr::Literal(_) => pred(self),
            Expr::Neg(expr) => pred(expr),
            Expr::Add(lhs, rhs) => lhs.all_terms(pred) && rhs.all_terms(pred),
            Expr::Sub(lhs, rhs) => lhs.all_terms(pred) && rhs.all_terms(pred),
            Expr::Mul(lhs, rhs) => lhs.all_terms(pred) && rhs.all_terms(pred),
            Expr::Div(lhs, rhs) => lhs.all_terms(pred) && rhs.all_terms(pred),
        }
    }

    /// Determine if at least one term in this expression verify `pred`.
    pub fn any_term(&self, pred: fn(&Expr<T>) -> bool) -> bool {
        match self {
            Expr::Variable(_) => pred(self),
            Expr::Literal(_) => pred(self),
            Expr::Neg(expr) => pred(expr),
            Expr::Add(lhs, rhs) => lhs.any_term(pred) || rhs.any_term(pred),
            Expr::Sub(lhs, rhs) => lhs.any_term(pred) || rhs.any_term(pred),
            Expr::Mul(lhs, rhs) => lhs.any_term(pred) || rhs.any_term(pred),
            Expr::Div(lhs, rhs) => lhs.any_term(pred) || rhs.any_term(pred),
        }
    }

    /// Determine if this expression is pure, ie contains no named variables.
    pub fn is_pure(&self) -> bool {
        self.all_terms(Expr::is_literal)
    }

    /// Determine if this expression contains at least one variable.
    pub fn is_impure(&self) -> bool {
        self.any_term(Expr::is_variable)
    }

    /// List of variables used in this expression
    pub fn variables(&self) -> HashSet<T> {
        fn go<T: Copy + Eq + std::hash::Hash>(expr: &Expr<T>, set: &mut HashSet<T>) {
            match expr {
                Expr::Variable(v) => {
                    set.insert(*v);
                }
                Expr::Literal(_) => return,
                Expr::Neg(expr) => go(expr, set),
                Expr::Add(lhs, rhs) => {
                    go(lhs, set);
                    go(rhs, set)
                }
                Expr::Sub(lhs, rhs) => {
                    go(lhs, set);
                    go(rhs, set)
                }
                Expr::Mul(lhs, rhs) => {
                    go(lhs, set);
                    go(rhs, set)
                }
                Expr::Div(lhs, rhs) => {
                    go(lhs, set);
                    go(rhs, set)
                }
            }
        }
        let mut ret = HashSet::new();
        go(self, &mut ret);
        ret
    }

    /// Returns `true` if the expr is [`Variable`].
    ///
    /// [`Variable`]: Expr::Variable
    #[must_use]
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable(..))
    }

    /// Returns `true` if the expr is [`Neg`].
    ///
    /// [`Neg`]: Expr::Neg
    #[must_use]
    pub fn is_neg(&self) -> bool {
        matches!(self, Self::Neg(..))
    }

    pub fn as_neg(&self) -> Option<&Box<Self>> {
        if let Self::Neg(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expr is [`Literal`].
    ///
    /// [`Literal`]: Expr::Literal
    #[must_use]
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal(..))
    }
}

impl<T> Expr<T> {
    fn map_variables<U>(&self, f: &impl Fn(&T) -> U) -> Expr<U> {
        use Expr::*;
        match self {
            Variable(v) => Variable(f(v)),
            Literal(l) => Literal(*l),
            Neg(expr) => Neg(Box::new(expr.map_variables(f))),
            Add(lhs, rhs) => Add(
                Box::new(lhs.map_variables(f)),
                Box::new(rhs.map_variables(f)),
            ),
            Sub(lhs, rhs) => Sub(
                Box::new(lhs.map_variables(f)),
                Box::new(rhs.map_variables(f)),
            ),
            Mul(lhs, rhs) => Mul(
                Box::new(lhs.map_variables(f)),
                Box::new(rhs.map_variables(f)),
            ),
            Div(lhs, rhs) => Div(
                Box::new(lhs.map_variables(f)),
                Box::new(rhs.map_variables(f)),
            ),
        }
    }
}
