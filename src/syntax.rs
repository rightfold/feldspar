use num::BigInt;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expr<'a: 'b, 'b, T: 'b>(pub T, pub ExprF<'a, &'b Expr<'a, 'b, T>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprF<'a, T> {
  Lit(Literal<'a>),
  Var(&'a str),
  Abs(&'a str, T),
  App(T, T),
  Tup(Vec<T>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal<'a> {
  Bool(bool),
  Int(Rc<BigInt>),
  Str(&'a str),
}
