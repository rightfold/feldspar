use std::collections::HashMap;
use std::fmt;
use syntax::{Expr, ExprF, Literal};
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error<'s, 'ty> where 's: 'ty {
  Unify(&'ty Ty<'s, 'ty>, &'ty Ty<'s, 'ty>),
  Var(&'s str),
  RankN,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ID(usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty<'s, 'ty> where 's: 'ty {
  Var(&'s str),
  Deferred(ID),
  Skolem(ID),
  Forall(&'s str, &'ty Ty<'s, 'ty>),
  Func(&'ty Ty<'s, 'ty>, &'ty Ty<'s, 'ty>),
  Bool,
  Int,
  Str,
  Bytes,
  Tuple(Vec<&'ty Ty<'s, 'ty>>),
}

pub static TY_BOOL:  Ty<'static, 'static> = Ty::Bool;
pub static TY_INT:   Ty<'static, 'static> = Ty::Int;
pub static TY_STR:   Ty<'static, 'static> = Ty::Str;
pub static TY_BYTES: Ty<'static, 'static> = Ty::Bytes;

pub static BUILTIN_STDOUT_TY:  Ty<'static, 'static> = Ty::Int;
pub static BUILTIN_TO_UTF8_TY: Ty<'static, 'static> =
  Ty::Func(&TY_STR, &TY_BYTES);
pub static BUILTIN_WRITE_TY:   Ty<'static, 'static> =
  Ty::Func(&TY_INT, &Ty::Func(&TY_BYTES, &TY_INT)); // FIXME: Return io int.

impl<'s, 'ty> Ty<'s, 'ty> {
  pub fn pretty<Purge, W>(&'ty self, purge: &Purge, into: &mut W) -> fmt::Result
    where
      Purge: Fn(&'ty Ty<'s, 'ty>) -> &'ty Ty<'s, 'ty>,
      W: fmt::Write {
    match *purge(self) {
      Ty::Var(name) =>
        write!(into, "{}", name),
      Ty::Deferred(ID(id)) =>
        write!(into, "?{}", id),
      Ty::Skolem(ID(id)) =>
        write!(into, "!{}", id),
      Ty::Forall(name, inner) => {
        write!(into, "forall {}, ", name)?;
        inner.pretty(purge, into)?;
        write!(into, "end")?;
        Ok(())
      },
      Ty::Func(a, b) => {
        write!(into, "(")?;
        a.pretty(purge, into)?;
        write!(into, " -> ")?;
        b.pretty(purge, into)?;
        write!(into, ")")?;
        Ok(())
      },
      Ty::Bool =>  write!(into, "bool"),
      Ty::Int =>   write!(into, "int"),
      Ty::Str =>   write!(into, "str"),
      Ty::Bytes => write!(into, "bytes"),
      Ty::Tuple(ref elem_tys) => {
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

  pub fn pretty_string<Purge>(&'ty self, purge: &Purge) -> String
    where Purge: Fn(&'ty Ty<'s, 'ty>) -> &'ty Ty<'s, 'ty> {
    let mut pretty = String::new();
    self.pretty(purge, &mut pretty).unwrap();
    pretty
  }
}

pub struct Check<'s, 'ty> where 's: 'ty {
  arena: &'ty Arena<Ty<'s, 'ty>>,
  next_id: usize,
  solved: HashMap<ID, &'ty Ty<'s, 'ty>>,
}

impl<'s, 'ty> Check<'s, 'ty> {
  pub fn new(arena: &'ty Arena<Ty<'s, 'ty>>) -> Self {
    Check{arena: arena, next_id: 0, solved: HashMap::new()}
  }

  pub fn fresh(&mut self) -> &'ty Ty<'s, 'ty> {
    let ty = self.arena.alloc(Ty::Deferred(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  pub fn fresh_skolem(&mut self) -> &'ty Ty<'s, 'ty> {
    let ty = self.arena.alloc(Ty::Skolem(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  fn solve(&mut self, id: ID, ty: &'ty Ty<'s, 'ty>) {
    self.solved.insert(id, ty);
  }

  pub fn purge(&self, ty: &'ty Ty<'s, 'ty>) -> &'ty Ty<'s, 'ty> {
    if let &Ty::Deferred(id) = ty {
      self.solved.get(&id).map(|uy| self.purge(uy)).unwrap_or(ty)
    } else {
      ty
    }
  }

  pub fn unify(&mut self, ty: &'ty Ty<'s, 'ty>, uy: &'ty Ty<'s, 'ty>)
    -> Result<(), Error<'s, 'ty>> {
    match (self.purge(ty), self.purge(uy)) {
      (&Ty::Deferred(ty_id), &Ty::Deferred(uy_id))
        if ty_id == uy_id => Ok(()),
      (&Ty::Deferred(ty_id), uy_p) => {
        self.solve(ty_id, uy_p);
        Ok(())
      },
      (ty_p, &Ty::Deferred(uy_id)) => {
        self.solve(uy_id, ty_p);
        Ok(())
      },
      (&Ty::Skolem(ty_id), &Ty::Skolem(uy_id))
        if ty_id == uy_id => Ok(()),
      (&Ty::Func(ty_a, ty_b), &Ty::Func(uy_a, uy_b)) => {
        self.unify(ty_a, uy_a)?;
        self.unify(ty_b, uy_b)?;
        Ok(())
      },
      (&Ty::Bool,  &Ty::Bool)  => Ok(()),
      (&Ty::Int,   &Ty::Int)   => Ok(()),
      (&Ty::Str,   &Ty::Str)   => Ok(()),
      (&Ty::Bytes, &Ty::Bytes) => Ok(()),
      (&Ty::Tuple(ref elem_tys), &Ty::Tuple(ref elem_uys)) => {
        if elem_tys.len() != elem_uys.len() {
          Err(Error::Unify(&TY_BOOL, &TY_INT)) // FIXME: Tuple types.
        } else {
          for (ty, uy) in elem_tys.iter().zip(elem_uys.iter()) {
            self.unify(ty, uy)?;
          }
          Ok(())
        }
      },
      (a, b) => Err(Error::Unify(a, b))
    }
  }

  pub fn infer<'e, T>(
    &mut self,
    env: &HashMap<&str, &'ty Ty<'s, 'ty>>,
    expr: &Expr<'s, 'e, &'ty Ty<'s, 'ty>, T>,
  ) -> Result<&'ty Ty<'s, 'ty>, Error<'s, 'ty>> {
    match expr.1 {
      ExprF::Lit(Literal::Bool(_)) => Ok(&TY_BOOL),
      ExprF::Lit(Literal::Int(_))  => Ok(&TY_INT),
      ExprF::Lit(Literal::Str(_))  => Ok(&TY_STR),
      ExprF::Var("stdout%")  => Ok(&BUILTIN_STDOUT_TY),
      ExprF::Var("to_utf8%") => Ok(&BUILTIN_TO_UTF8_TY),
      ExprF::Var("write%")   => Ok(&BUILTIN_WRITE_TY),
      ExprF::Var(name) => env.get(&name).map(|&ty| ty).ok_or(Error::Var(name)),
      ExprF::Abs(param, body) => {
        let param_ty = self.fresh();

        let mut body_env = env.clone();
        body_env.insert(param, param_ty);
        let result_ty = self.infer(&body_env, body)?;

        let func_ty = self.arena.alloc(Ty::Func(param_ty, result_ty));
        Ok(func_ty)
      },
      ExprF::App(callee, argument) => {
        let callee_ty = self.infer(env, callee)?;
        let argument_ty = self.infer(env, argument)?;

        let result_ty = self.fresh();
        let func_ty = self.arena.alloc(Ty::Func(argument_ty, result_ty));
        self.unify(callee_ty, func_ty)?;

        Ok(result_ty)
      },
      ExprF::Let(name, name_ty_opt, value, body) => {
        let value_ty = self.infer(env, value)?;

        let mut body_env = env.clone();
        body_env.insert(name, match name_ty_opt {
          None => value_ty,
          Some(name_ty) => {
            let skolemized_ty = self.skolemize(name_ty)?;
            self.unify(skolemized_ty, value_ty)?;
            name_ty
          },
        });

        let body_ty = self.infer(&body_env, body)?;

        Ok(body_ty)
      },
      ExprF::Tup(ref elems) => {
        let mut elem_tys = Vec::with_capacity(elems.len());
        for elem in elems {
          let elem_ty = self.infer(env, elem)?;
          elem_tys.push(elem_ty);
        }
        let tuple_ty = self.arena.alloc(Ty::Tuple(elem_tys));
        Ok(tuple_ty)
      },
    }
  }

  pub fn skolemize(
    &mut self,
    ty: &'ty Ty<'s, 'ty>,
  ) -> Result<&'ty Ty<'s, 'ty>, Error<'s, 'ty>> {
    match *ty {
      Ty::Forall(name, inner) => {
        let mut ty_env = HashMap::new();
        ty_env.insert(name, self.fresh_skolem());
        self.skolemize_no_forall(&ty_env, inner)
      },
      _ => self.skolemize_no_forall(&HashMap::new(), ty),
    }
  }

  pub fn skolemize_no_forall(
    &mut self,
    ty_env: &HashMap<&str, &'ty Ty<'s, 'ty>>,
    ty: &'ty Ty<'s, 'ty>,
  ) -> Result<&'ty Ty<'s, 'ty>, Error<'s, 'ty>> {
    match *ty {
      Ty::Var(name)      => ty_env.get(name).map(|&ty| ty).ok_or(Error::Var(name)),
      Ty::Deferred(_)    => Ok(ty),
      Ty::Skolem(_)      => Ok(ty),
      Ty::Forall(_, _)   => Err(Error::RankN),
      Ty::Func(from, to) => {
        let from_ty = self.skolemize_no_forall(ty_env, from)?;
        let to_ty = self.skolemize_no_forall(ty_env, to)?;
        let ty = Ty::Func(from_ty, to_ty);
        Ok(self.arena.alloc(ty))
      },
      Ty::Bool  => Ok(ty),
      Ty::Int   => Ok(ty),
      Ty::Str   => Ok(ty),
      Ty::Bytes => Ok(ty),
      Ty::Tuple(_) => panic!("Check::skolemize_no_forall: NYI"),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_unify() {
    let arena = Arena::new();
    let mut check = Check::new(&arena);

    let ty1 = &TY_BOOL;
    let ty2 = arena.alloc(Ty::Func(ty1, ty1));
    let ty3 = check.fresh();
    let ty4 = check.fresh();
    let ty5 = check.fresh();
    let ty6 = check.fresh();

    assert_eq!(check.unify(ty1, ty1), Ok(()));

    assert_eq!(check.unify(ty2, ty2), Ok(()));

    assert_eq!(check.unify(ty1, ty2), Err(Error::Unify(ty1, ty2)));

    assert_eq!(check.unify(ty3, ty3), Ok(()));
    assert_eq!(check.unify(ty3, ty4), Ok(()));
    assert_eq!(check.unify(ty4, ty1), Ok(()));
    assert_eq!(check.purge(ty3),      ty1);
    assert_eq!(check.unify(ty5, ty1), Ok(()));
    assert_eq!(check.unify(ty5, ty3), Ok(()));
    assert_eq!(check.unify(ty6, ty2), Ok(()));
    assert_eq!(check.unify(ty5, ty6), Err(Error::Unify(ty1, ty2)));
  }

  #[test]
  fn test_infer() {
    let expr_arena = Arena::new();
    let ty_arena = Arena::new();

    let mut check = Check::new(&ty_arena);

    let mut env = HashMap::new();
    env.insert("vari", &TY_INT);
    env.insert("even", ty_arena.alloc(Ty::Func(&TY_INT, &TY_BOOL)));

    let expr = Expr(
      (),
      ExprF::App(
        expr_arena.alloc(Expr((), ExprF::Var("even"))),
        expr_arena.alloc(Expr((), ExprF::Var("vari"))),
      ),
    );
    let ty = check.infer(&env, &expr);
    assert_eq!(ty.map(|t| check.purge(t)), Ok(&TY_BOOL));
  }
}
