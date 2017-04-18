use std::collections::HashMap;
use std::fmt;
use syntax::{Expr, ID, Ty};
use syntax::Expr::*;
use syntax::{
  BUILTIN_STDOUT_TY,
  BUILTIN_TO_UTF8_TY,
  BUILTIN_WRITE_TY,
  TY_BOOL,
  TY_INT,
  TY_STR,
};
use typed_arena::Arena;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'t> {
  Unify(&'t Ty<'t>, &'t Ty<'t>),
  Var(String),
  RankN,
}

impl<'t> Ty<'t> {
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

  pub fn pretty_string<Purge>(&'t self, purge: &Purge) -> String
    where Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t> {
    let mut pretty = String::new();
    self.pretty(purge, &mut pretty).unwrap();
    pretty
  }
}

pub struct Check<'t> {
  arena: &'t Arena<Ty<'t>>,
  next_id: usize,
  solved: HashMap<ID, &'t Ty<'t>>,
}

impl<'t> Check<'t> {
  pub fn new(arena: &'t Arena<Ty<'t>>) -> Self {
    Check{arena: arena, next_id: 0, solved: HashMap::new()}
  }

  pub fn fresh_unify(&mut self) -> &'t Ty<'t> {
    let ty = self.arena.alloc(Ty::Deferred(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  pub fn fresh_skolem(&mut self) -> &'t Ty<'t> {
    let ty = self.arena.alloc(Ty::Skolem(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  fn solve(&mut self, id: ID, ty: &'t Ty<'t>) {
    self.solved.insert(id, ty);
  }

  pub fn purge(&self, ty: &'t Ty<'t>) -> &'t Ty<'t> {
    if let &Ty::Deferred(id) = ty {
      self.solved.get(&id).map(|uy| self.purge(uy)).unwrap_or(ty)
    } else {
      ty
    }
  }

  pub fn unify(&mut self, ty: &'t Ty<'t>, uy: &'t Ty<'t>)
    -> Result<(), Error<'t>> {
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

  pub fn infer<'e>(
    &mut self,
    env: &HashMap<&str, &'t Ty<'t>>,
    expr: &Expr<'e, 't>,
  ) -> Result<&'t Ty<'t>, Error<'t>> {
    match expr {
      &Bool(_, _) => Ok(&TY_BOOL),
      &Str(_, _) => Ok(&TY_STR),
      &Var(_, ref name) =>
        match name.as_ref() {
          "stdout%"  => Ok(&BUILTIN_STDOUT_TY),
          "to_utf8%" => Ok(&BUILTIN_TO_UTF8_TY),
          "write%"   => Ok(&BUILTIN_WRITE_TY),
          _ => {
            let poly_ty = env.get::<str>(name).map(|&ty| ty)
                            .ok_or_else(|| Error::Var(name.clone()))?;
            self.monomorphize(poly_ty)
          },
        },
      &Abs(_, ref param, body) => {
        let param_ty = self.fresh_unify();

        let mut body_env = env.clone();
        body_env.insert(param, param_ty);
        let result_ty = self.infer(&body_env, body)?;

        let func_ty = self.arena.alloc(Ty::Func(param_ty, result_ty));
        Ok(func_ty)
      },
      &App(_, callee, argument) => {
        let callee_ty = self.infer(env, callee)?;
        let argument_ty = self.infer(env, argument)?;

        let result_ty = self.fresh_unify();
        let func_ty = self.arena.alloc(Ty::Func(argument_ty, result_ty));
        self.unify(callee_ty, func_ty)?;

        Ok(result_ty)
      },
      &Let(_, ref name, name_ty_opt, value, body) => {
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
    }
  }

  pub fn monomorphize(&mut self, ty: &'t Ty<'t>)
    -> Result<&'t Ty<'t>, Error<'t>> {
    Self::replace_ty_vars(
      self.arena,
      &mut || self.fresh_unify(),
      &HashMap::new(),
      ty,
    )
  }

  pub fn skolemize(&mut self, ty: &'t Ty<'t>)
    -> Result<&'t Ty<'t>, Error<'t>> {
    Self::replace_ty_vars(
      self.arena,
      &mut || self.fresh_skolem(),
      &HashMap::new(),
      ty,
    )
  }

  fn replace_ty_vars<Fresh>(
    arena: &'t Arena<Ty<'t>>,
    fresh: &mut Fresh,
    ty_env: &HashMap<&str, &'t Ty<'t>>,
    ty: &'t Ty<'t>,
  ) -> Result<&'t Ty<'t>, Error<'t>>
    where Fresh: FnMut() -> &'t Ty<'t> {
    match ty {
      &Ty::Forall(ref name, inner) => {
        let mut inner_ty_env = ty_env.clone();
        inner_ty_env.insert(&name, fresh());
        Self::replace_ty_vars(arena, fresh, &inner_ty_env, inner)
      },
      _ => Self::replace_ty_vars_no_forall(arena, ty_env, ty),
    }
  }

  fn replace_ty_vars_no_forall(
    arena: &'t Arena<Ty<'t>>,
    ty_env: &HashMap<&str, &'t Ty<'t>>,
    ty: &'t Ty<'t>,
  ) -> Result<&'t Ty<'t>, Error<'t>> {
    match ty {
      &Ty::Var(ref name)  => ty_env.get::<str>(&name).map(|&ty| ty)
                              .ok_or_else(|| Error::Var(name.clone())),
      &Ty::Deferred(_)    => Ok(ty),
      &Ty::Skolem(_)      => Ok(ty),
      &Ty::Forall(_, _)   => Err(Error::RankN),
      &Ty::Func(from, to) => {
        let from_ty = Self::replace_ty_vars_no_forall(arena, ty_env, from)?;
        let to_ty = Self::replace_ty_vars_no_forall(arena, ty_env, to)?;
        let ty = Ty::Func(from_ty, to_ty);
        Ok(arena.alloc(ty))
      },
      &Ty::Bool  => Ok(ty),
      &Ty::Int   => Ok(ty),
      &Ty::Str   => Ok(ty),
      &Ty::Bytes => Ok(ty),
      &Ty::Tuple(_) => panic!("Check::skolemize_no_forall: NYI"),
    }
  }
}

#[cfg(test)]
mod test {
  use pos::Pos;
  use super::*;

  #[test]
  fn test_unify() {
    let arena = Arena::new();
    let mut check = Check::new(&arena);

    let ty1 = &TY_BOOL;
    let ty2 = arena.alloc(Ty::Func(ty1, ty1));
    let ty3 = check.fresh_unify();
    let ty4 = check.fresh_unify();
    let ty5 = check.fresh_unify();
    let ty6 = check.fresh_unify();

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

    let expr = App(
      Pos(0),
      expr_arena.alloc(Var(Pos(0), "even".to_string())),
      expr_arena.alloc(Var(Pos(0), "vari".to_string())),
    );
    let ty = check.infer(&env, &expr);
    assert_eq!(ty.map(|t| check.purge(t)), Ok(&TY_BOOL));
  }
}
