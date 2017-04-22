use std::collections::HashMap;
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

/// A type error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'t> {
  /// Two types could not be unified.
  Unify(&'t Ty<'t>, &'t Ty<'t>),

  /// The variable referenced is not in scope.
  Var(String),

  /// Higher-rank types are not yet supported.
  RankN,
}

pub struct Check<'t> {
  next_id: usize,
  solved: HashMap<ID, &'t Ty<'t>>,
}

impl<'t> Check<'t> {
  /// Create a new type checker with no solved unification variables.
  pub fn new() -> Self {
    Check{next_id: 0, solved: HashMap::new()}
  }

  /// Create a new unsolved unification variable.
  pub fn fresh_unify(&mut self, arena: &'t Arena<Ty<'t>>) -> &'t Ty<'t> {
    let ty = arena.alloc(Ty::Deferred(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  /// Create a new Skolem.
  pub fn fresh_skolem(&mut self, arena: &'t Arena<Ty<'t>>) -> &'t Ty<'t> {
    let ty = arena.alloc(Ty::Skolem(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  fn solve(&mut self, id: ID, ty: &'t Ty<'t>) {
    self.solved.insert(id, ty);
  }

  /// Recursively dereference solved unification variables.
  pub fn purge(&self, ty: &'t Ty<'t>) -> &'t Ty<'t> {
    if let &Ty::Deferred(id) = ty {
      self.solved.get(&id).map(|uy| self.purge(uy)).unwrap_or(ty)
    } else {
      ty
    }
  }

  /// Unify two types, solving unification variables.
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
      (&Ty::FileHandle, &Ty::FileHandle) => Ok(()),
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

  /// Infer the type of an expression, solving unification variables.
  pub fn infer<'e>(
    &mut self,
    arena: &'t Arena<Ty<'t>>,
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
            self.monomorphize(arena, poly_ty)
          },
        },
      &Abs(_, ref param, body) => {
        let param_ty = self.fresh_unify(arena);

        let mut body_env = env.clone();
        body_env.insert(param, param_ty);
        let result_ty = self.infer(arena, &body_env, body)?;

        let func_ty = arena.alloc(Ty::Func(param_ty, result_ty));
        Ok(func_ty)
      },
      &App(_, callee, argument) => {
        let callee_ty = self.infer(arena, env, callee)?;
        let argument_ty = self.infer(arena, env, argument)?;

        let result_ty = self.fresh_unify(arena);
        let func_ty = arena.alloc(Ty::Func(argument_ty, result_ty));
        self.unify(callee_ty, func_ty)?;

        Ok(result_ty)
      },
      &Let(_, ref name, name_ty_opt, value, body) => {
        let value_ty = self.infer(arena, env, value)?;

        let mut body_env = env.clone();
        body_env.insert(name, match name_ty_opt {
          None => value_ty,
          Some(name_ty) => {
            let skolemized_ty = self.skolemize(arena, name_ty)?;
            self.unify(skolemized_ty, value_ty)?;
            name_ty
          },
        });

        let body_ty = self.infer(arena, &body_env, body)?;

        Ok(body_ty)
      },
    }
  }

  /// Replace universally quantified type variables by new unsolved unification
  /// variables.
  ///
  /// # Example
  ///
  /// ```text
  /// >>> monomorphize ∀ a b, a -> b -> int
  /// ?0 -> ?1 -> int
  /// ```
  pub fn monomorphize(&mut self, arena: &'t Arena<Ty<'t>>, ty: &'t Ty<'t>)
    -> Result<&'t Ty<'t>, Error<'t>> {
    Self::replace_ty_vars(
      arena,
      &mut || self.fresh_unify(arena),
      &HashMap::new(),
      ty,
    )
  }

  /// Replace universally quantified type variables by new Skolems.
  ///
  /// # Example
  ///
  /// ```text
  /// >>> skolemize ∀ a b, a -> b -> int
  /// !0 -> !1 -> int
  /// ```
  pub fn skolemize(&mut self, arena: &'t Arena<Ty<'t>>, ty: &'t Ty<'t>)
    -> Result<&'t Ty<'t>, Error<'t>> {
    Self::replace_ty_vars(
      arena,
      &mut || self.fresh_skolem(arena),
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
      &Ty::FileHandle => Ok(ty),
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
    let mut check = Check::new();

    let ty1 = &TY_BOOL;
    let ty2 = arena.alloc(Ty::Func(ty1, ty1));
    let ty3 = check.fresh_unify(&arena);
    let ty4 = check.fresh_unify(&arena);
    let ty5 = check.fresh_unify(&arena);
    let ty6 = check.fresh_unify(&arena);

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

    let mut check = Check::new();

    let mut env = HashMap::new();
    env.insert("vari", &TY_INT);
    env.insert("even", ty_arena.alloc(Ty::Func(&TY_INT, &TY_BOOL)));

    let expr = App(
      Pos(0),
      expr_arena.alloc(Var(Pos(0), "even".to_string())),
      expr_arena.alloc(Var(Pos(0), "vari".to_string())),
    );
    let ty = check.infer(&ty_arena, &env, &expr);
    assert_eq!(ty.map(|t| check.purge(t)), Ok(&TY_BOOL));
  }
}
