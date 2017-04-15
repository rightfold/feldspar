use std::cell::RefCell;
use std::collections::HashMap;
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ID(usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty<'ty> {
  Deferred(ID),
  Func(&'ty Ty<'ty>, &'ty Ty<'ty>),
  Bool,
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

  pub fn unify(&mut self, ty: &'ty Ty<'ty>, uy: &'ty Ty<'ty>) -> Result<(), ()> {
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
      _ =>
        Err(()),
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

    let ty1 = arena.alloc(Ty::Bool);
    let ty2 = arena.alloc(Ty::Func(ty1, ty1));
    let ty3 = check.fresh();
    let ty4 = check.fresh();
    let ty5 = check.fresh();
    let ty6 = check.fresh();

    assert_eq!(check.unify(ty1, ty1), Ok(()));

    assert_eq!(check.unify(ty2, ty2), Ok(()));

    assert_eq!(check.unify(ty1, ty2), Err(()));

    assert_eq!(check.unify(ty3, ty3), Ok(()));
    assert_eq!(check.unify(ty3, ty4), Ok(()));
    assert_eq!(check.unify(ty4, ty1), Ok(()));
    assert_eq!(check.purge(ty3),      ty1);
    assert_eq!(check.unify(ty5, ty1), Ok(()));
    assert_eq!(check.unify(ty5, ty3), Ok(()));
    assert_eq!(check.unify(ty6, ty2), Ok(()));
    assert_eq!(check.unify(ty5, ty6), Err(()));
  }
}
