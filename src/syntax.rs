use diagnostic::Pos;
use std::cell::RefCell;
use std::fmt;

/* -------------------------------------------------------------------------- */

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr<'e, 't> where 't: 'e {
  /// A Boolean constant.
  Bool(Pos, bool),

  /// A string constant. The string will be moved out during code generation.
  Str (Pos, RefCell<String>),

  /// A variable reference.
  Var (Pos, String),

  /// A lambda.
  Abs (Pos, String, &'e Expr<'e, 't>),

  /// A function application.
  App (Pos, &'e Expr<'e, 't>, &'e Expr<'e, 't>),

  /// A let.
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

/// A type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty<'t> {
  /// A quantified type variable.
  Var(String),

  /// A unification variable.
  Deferred(ID),

  /// A Skolem.
  Skolem(ID),

  /// A universally quantified type.
  Forall(String, &'t Ty<'t>),

  /// A function type.
  Func(&'t Ty<'t>, &'t Ty<'t>),

  /// The type of Booleans.
  Bool,

  /// The type of integers.
  Int,

  /// The type of strings.
  Str,

  /// The type of byte strings.
  Bytes,

  /// The type of file handles.
  FileHandle,

  /// Tuple types.
  Tuple(Vec<&'t Ty<'t>>),
}

impl<'t> Ty<'t> {
  /// Pretty-print a type.
  pub fn pretty<Purge, W>(&'t self, purge: &Purge, into: &mut W) -> fmt::Result
    where
      Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t>,
      W: fmt::Write {
    match purge(self) {
      &Ty::Var(ref name) =>
        write!(into, "{}", name),
      &Ty::Deferred(ID(id)) =>
        write!(into, "?{}", id),
      &Ty::Skolem(ID(id)) =>
        write!(into, "!{}", id),
      &Ty::Forall(ref name, inner) => {
        write!(into, "∀ {}, ", name)?;
        inner.pretty(purge, into)?;
        write!(into, " end")?;
        Ok(())
      },
      &Ty::Func(a, b) => {
        write!(into, "(")?;
        a.pretty(purge, into)?;
        write!(into, " -> ")?;
        b.pretty(purge, into)?;
        write!(into, ")")?;
        Ok(())
      },
      &Ty::Bool =>  write!(into, "bool"),
      &Ty::Int =>   write!(into, "int"),
      &Ty::Str =>   write!(into, "str"),
      &Ty::Bytes => write!(into, "bytes"),
      &Ty::FileHandle => write!(into, "file_handle"),
      &Ty::Tuple(ref elem_tys) => {
        write!(into, "{{")?;
        for elem_ty in elem_tys {
          elem_ty.pretty(purge, into)?;
          write!(into, ",")?;
        }
        write!(into, "}}")?;
        Ok(())
      },
    }
  }

  /// Pretty-print a type to a string.
  pub fn pretty_string<Purge>(&'t self, purge: &Purge) -> String
    where Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t> {
    let mut pretty = String::new();
    self.pretty(purge, &mut pretty).unwrap();
    pretty
  }
}

/* -------------------------------------------------------------------------- */

pub static TY_BOOL:  Ty<'static> = Ty::Bool;
pub static TY_INT:   Ty<'static> = Ty::Int;
pub static TY_STR:   Ty<'static> = Ty::Str;
pub static TY_BYTES: Ty<'static> = Ty::Bytes;
pub static TY_FILE_HANDLE: Ty<'static> = Ty::FileHandle;

pub static BUILTIN_STDOUT_TY:  Ty<'static> = Ty::FileHandle;
pub static BUILTIN_TO_UTF8_TY: Ty<'static> = Ty::Func(&TY_STR, &TY_BYTES);
pub static BUILTIN_WRITE_TY:   Ty<'static> =
  Ty::Func(&TY_FILE_HANDLE, &Ty::Func(&TY_BYTES, &TY_INT)); // FIXME: Return io int.
