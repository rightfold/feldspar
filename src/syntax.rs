use num::BigInt;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expr<'s: 'e, 'e, Ty: 'e, T: 'e>(pub T, pub ExprF<'s, Ty, &'e Expr<'s, 'e, Ty, T>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprF<'s, Ty, T> {
  Lit(Literal<'s>),
  Var(&'s str),
  Abs(&'s str, T),
  App(T, T),
  Let(&'s str, Option<Ty>, T, T),
  Tup(Vec<T>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal<'s> {
  Bool(bool),
  Int(Rc<BigInt>),
  Str(&'s str),
}
