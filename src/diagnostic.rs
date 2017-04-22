use std::collections::HashMap;
use std::fmt;
use syntax::Ty;

/// A source position, represented by the offset in bytes.
#[derive(Clone, Copy, Debug)]
pub struct Pos(pub usize);

impl Pos {
  pub fn offset(&self) -> usize {
    self.0
  }
}

#[derive(Clone, Debug)]
pub enum Diagnostic<'e, 't> {
  CannotUnifyTypes(Pos, HashMap<&'e str, &'t Ty<'t>>, &'t Ty<'t>, &'t Ty<'t>),
  ValueIsNotInScope(Pos, HashMap<&'e str, &'t Ty<'t>>, &'e str),
  TypeIsNotInScope(Pos, HashMap<&'t str, &'t Ty<'t>>, &'t str),
  HigherRankTypesAreNotSupported(Pos),
}

impl<'e, 't> Diagnostic<'e, 't> {
  pub fn pos(&self) -> Pos {
    match self {
      &Diagnostic::CannotUnifyTypes(p, _, _, _) => p,
      &Diagnostic::ValueIsNotInScope(p, _, _) => p,
      &Diagnostic::TypeIsNotInScope(p, _, _) => p,
      &Diagnostic::HigherRankTypesAreNotSupported(p) => p,
    }
  }

  pub fn env(&self) -> Option<&HashMap<&'e str, &'t Ty<'t>>> {
    match self {
      &Diagnostic::CannotUnifyTypes(_, ref e, _, _) => Some(e),
      &Diagnostic::ValueIsNotInScope(_, ref e, _) => Some(e),
      &Diagnostic::TypeIsNotInScope(_, _, _) => None,
      &Diagnostic::HigherRankTypesAreNotSupported(_) => None,
    }
  }

  pub fn fmt<Purge, W>(&self, purge: &Purge, w: &mut W) -> fmt::Result
    where
      Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t>,
      W: fmt::Write {
    write!(w, "{}: ", self.pos().offset())?;
    self.fmt_summary(purge, w)?;
    for env in self.env() {
      write!(w, "\n\n")?;
      self.fmt_env(purge, env, w)?;
    }
    Ok(())
  }

  fn fmt_summary<Purge, W>(&self, purge: &Purge, w: &mut W) -> fmt::Result
    where
      Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t>,
      W: fmt::Write {
    match self {
      &Diagnostic::CannotUnifyTypes(_, _, t, u) => {
        write!(w, "cannot unify types `")?;
        t.pretty(purge, w)?;
        write!(w, "` and `")?;
        u.pretty(purge, w)?;
        write!(w, "`")?;
        Ok(())
      },
      &Diagnostic::ValueIsNotInScope(_, _, n) =>
        write!(w, "value `{}` is not in scope", n),
      &Diagnostic::TypeIsNotInScope(_, _, n) =>
        write!(w, "type `{}` is not in scope", n),
      &Diagnostic::HigherRankTypesAreNotSupported(_) =>
        write!(w, "higher-rank types are not supported"),
    }
  }

  fn fmt_env<Purge, W>(
    &self,
    purge: &Purge,
    env: &HashMap<&str, &'t Ty<'t>>,
    w: &mut W,
  ) -> fmt::Result
    where
      Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t>,
      W: fmt::Write {
    write!(w, "  Where:\n")?;
    for (k, v) in env {
      write!(w, "\n    {}\t: ", k)?;
      v.pretty(purge, w)?;
    }
    Ok(())
  }

  pub fn fmt_string<Purge>(&self, purge: &Purge) -> String
    where Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t> {
    let mut s = String::new();
    self.fmt(purge, &mut s).unwrap();
    s
  }
}
