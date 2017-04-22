use pos::Pos;
use std::collections::HashMap;
use std::fmt;
use syntax::Ty;

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

  pub fn fmt<Purge, W>(&self, purge: &Purge, w: &mut W) -> fmt::Result
    where
      Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t>,
      W: fmt::Write {
    write!(w, "{}: ", self.pos().offset())?;
    self.fmt_summary(purge, w)?;
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

  pub fn fmt_string<Purge>(&self, purge: &Purge) -> String
    where Purge: Fn(&'t Ty<'t>) -> &'t Ty<'t> {
    let mut s = String::new();
    self.fmt(purge, &mut s).unwrap();
    s
  }
}
