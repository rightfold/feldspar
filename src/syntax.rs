use num::BigInt;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expr<'s: 'e, 'e, T: 'e>(pub T, pub ExprF<'s, &'e Expr<'s, 'e, T>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprF<'s, T> {
  Lit(Literal<'s>),
  Var(&'s str),
  Abs(&'s str, T),
  App(T, T),
  Tup(Vec<T>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TyExpr<'s: 'e, 'e, T: 'e>(pub T, pub TyExprF<'s, &'e TyExpr<'s, 'e, T>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TyExprF<'s, T> {
  Var(&'s str),
  Fun(T, T),
  All(&'s str, T),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal<'s> {
  Bool(bool),
  Int(Rc<BigInt>),
  Str(&'s str),
}
