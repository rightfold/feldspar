use pos::Pos;
use std::cell::RefCell;

/* -------------------------------------------------------------------------- */

#[derive(Clone, Debug)]
pub enum Expr<'e, 't> where 't: 'e {
  Bool(Pos, bool),
  Str (Pos, RefCell<String>),
  Var (Pos, String),
  Abs (Pos, String, &'e Expr<'e, 't>),
  App (Pos, &'e Expr<'e, 't>, &'e Expr<'e, 't>),
  Let (Pos, String, Option<&'t Ty<'t>>, &'e Expr<'e, 't>, &'e Expr<'e, 't>),
}

impl<'e, 't> Expr<'e, 't> {
  pub fn pos(&self) -> Pos {
    match *self {
      Expr::Bool(pos, _)          => pos,
      Expr::Str (pos, _)          => pos,
      Expr::Var (pos, _)          => pos,
      Expr::Abs (pos, _, _)       => pos,
      Expr::App (pos, _, _)       => pos,
      Expr::Let (pos, _, _, _, _) => pos,
    }
  }
}

/* -------------------------------------------------------------------------- */

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ID(pub usize); // FIXME: Different types.

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty<'t> {
  Var(String),
  Deferred(ID),
  Skolem(ID),
  Forall(String, &'t Ty<'t>),
  Func(&'t Ty<'t>, &'t Ty<'t>),
  Bool,
  Int,
  Str,
  Bytes,
  Tuple(Vec<&'t Ty<'t>>),
}

/* -------------------------------------------------------------------------- */

pub static TY_BOOL:  Ty<'static> = Ty::Bool;
pub static TY_INT:   Ty<'static> = Ty::Int;
pub static TY_STR:   Ty<'static> = Ty::Str;
pub static TY_BYTES: Ty<'static> = Ty::Bytes;

pub static BUILTIN_STDOUT_TY:  Ty<'static> = Ty::Int;
pub static BUILTIN_TO_UTF8_TY: Ty<'static> = Ty::Func(&TY_STR, &TY_BYTES);
pub static BUILTIN_WRITE_TY:   Ty<'static> =
  Ty::Func(&TY_INT, &Ty::Func(&TY_BYTES, &TY_INT)); // FIXME: Return io int.
