use num::BigInt;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Expr<'a: 'b, 'b, T: 'b>(pub T, pub ExprF<'a, &'b Expr<'a, 'b, T>>);

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ExprF<'a, T> {
  Lit(Literal<'a>),
  Var(&'a str),
  Abs(&'a str, T),
  App(T, T),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Literal<'a> {
  Bool(bool),
  Int(Rc<BigInt>),
  Str(&'a str),
}
