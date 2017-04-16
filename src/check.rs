use std::collections::HashMap;
use std::fmt;
use syntax::{Expr, ExprF, Literal};
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error<'str, 'ty> {
  Unify(&'ty Ty<'ty>, &'ty Ty<'ty>),
  Var(&'str str),
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ID(usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty<'ty> {
  Deferred(ID),
  Func(&'ty Ty<'ty>, &'ty Ty<'ty>),
  Bool,
  Int,
  Str,
}

pub static TY_BOOL: Ty<'static> = Ty::Bool;
pub static TY_INT:  Ty<'static> = Ty::Int;
pub static TY_STR:  Ty<'static> = Ty::Str;

impl<'ty> Ty<'ty> {
  pub fn pretty<Purge, W>(&'ty self, purge: &Purge, into: &mut W) -> fmt::Result
    where
      Purge: Fn(&'ty Ty<'ty>) -> &'ty Ty<'ty>,
      W: fmt::Write {
    match *purge(self) {
      Ty::Deferred(ID(id)) =>
        write!(into, "t{}", id),
      Ty::Func(a, b) => {
        try!(write!(into, "("));
        try!(a.pretty(purge, into));
        try!(write!(into, " -> "));
        try!(b.pretty(purge, into));
        try!(write!(into, ")"));
        Ok(())
      },
      Ty::Bool =>
        write!(into, "bool"),
      Ty::Int =>
        write!(into, "int"),
      Ty::Str =>
        write!(into, "str"),
    }
  }

  pub fn pretty_string<Purge>(&'ty self, purge: &Purge) -> String
    where Purge: Fn(&'ty Ty<'ty>) -> &'ty Ty<'ty> {
    let mut pretty = String::new();
    self.pretty(purge, &mut pretty).unwrap();
    pretty
  }
}

pub struct Check<'ty> {
  arena: &'ty Arena<Ty<'ty>>,
  next_id: usize,
  solved: HashMap<ID, &'ty Ty<'ty>>,
}

impl<'ty> Check<'ty> {
  pub fn new(arena: &'ty Arena<Ty<'ty>>) -> Self {
    Check{
      arena: arena,
      next_id: 0,
      solved: HashMap::new(),
    }
  }

  pub fn fresh(&mut self) -> &'ty Ty<'ty> {
    let ty = self.arena.alloc(Ty::Deferred(ID(self.next_id)));
    self.next_id += 1;
    ty
  }

  fn solve(&mut self, id: ID, ty: &'ty Ty<'ty>) {
    self.solved.insert(id, ty);
  }

  pub fn purge(&self, ty: &'ty Ty<'ty>) -> &'ty Ty<'ty> {
    match ty {
      &Ty::Deferred(id) =>
        match self.solved.get(&id) {
          Some(uy) => self.purge(uy),
          None => ty,
        },
      _ => ty,
    }
  }

  pub fn unify<'str>(&mut self, ty: &'ty Ty<'ty>, uy: &'ty Ty<'ty>)
    -> Result<(), Error<'str, 'ty>> {
    match (self.purge(ty), self.purge(uy)) {
      (&Ty::Deferred(ty_id), &Ty::Deferred(uy_id))
        if ty_id == uy_id =>
        Ok(()),
      (&Ty::Deferred(ty_id), uy_p) => {
        self.solve(ty_id, uy_p);
        Ok(())
      },
      (ty_p, &Ty::Deferred(uy_id)) => {
        self.solve(uy_id, ty_p);
        Ok(())
      },
      (&Ty::Func(ty_a, ty_b), &Ty::Func(uy_a, uy_b)) => {
        try!(self.unify(ty_a, uy_a));
        try!(self.unify(ty_b, uy_b));
        Ok(())
      },
      (&Ty::Bool, &Ty::Bool) =>
        Ok(()),
      (&Ty::Int, &Ty::Int) =>
        Ok(()),
      (&Ty::Str, &Ty::Str) =>
        Ok(()),
      (a, b) => {
        Err(Error::Unify(a, b))
      },
    }
  }

  pub fn infer<'str, 'expr, T>(
    &mut self,
    env: &HashMap<&str, &'ty Ty<'ty>>,
    expr: &Expr<'str, 'expr, T>,
  ) -> Result<&'ty Ty<'ty>, Error<'str, 'ty>> {
    match expr.1 {
      ExprF::Lit(Literal::Bool(_)) =>
        Ok(&TY_BOOL),
      ExprF::Lit(Literal::Int(_)) =>
        Ok(&TY_INT),
      ExprF::Lit(Literal::Str(_)) =>
        Ok(&TY_STR),
      ExprF::Var(name) =>
        match env.get(&name) {
          Some(ty) => Ok(ty),
          None => Err(Error::Var(name)),
        },
      ExprF::Abs(param, body) => {
        let param_ty = self.fresh();

        let mut body_env = env.clone();
        body_env.insert(param, param_ty);
        let result_ty = try!(self.infer(&body_env, body));

        let func_ty = self.arena.alloc(Ty::Func(param_ty, result_ty));
        Ok(func_ty)
      },
      ExprF::App(callee, argument) => {
        let callee_ty = try!(self.infer(env, callee));
        let argument_ty = try!(self.infer(env, argument));

        let result_ty = self.fresh();
        let func_ty = self.arena.alloc(Ty::Func(argument_ty, result_ty));
        try!(self.unify(callee_ty, func_ty));

        Ok(result_ty)
      },
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
