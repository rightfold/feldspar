use check::Ty;
use lex::{Lexeme, LexemeF, Lexer, Position};
use syntax::{Expr, ExprF, Literal};
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Error(pub Position, pub &'static str);

fn read_lexeme<'s>(lexer: &mut Lexer<'s>)
  -> Result<Lexeme<'s, Position>, Error> {
  match lexer.next() {
    None => Err(Error(lexer.next_position(), "unexpected EOF")),
    Some(Err(position)) => Err(Error(position, "unexpected input")),
    Some(Ok(lexeme)) => Ok(lexeme),
  }
}

fn read_lexeme_if<'s, T, F>(lexer: &mut Lexer<'s>, pred: F)
  -> Result<T, Error>
  where F: FnOnce(Lexeme<'s, Position>) -> Result<T, Error> {
  let lexeme = read_lexeme(lexer)?;
  pred(lexeme)
}

fn peek_lexeme<'s>(lexer: &Lexer<'s>)
  -> Result<Lexeme<'s, Position>, Error> {
  read_lexeme(&mut lexer.clone())
}

pub fn read_expr<'s, 'e, 'ty>(
  arena: &'e Arena<Expr<'s, 'e, &'ty Ty<'s, 'ty>, Position>>,
  ty_arena: &'ty Arena<Ty<'s, 'ty>>,
  lexer: &mut Lexer<'s>,
) -> Result<&'e Expr<'s, 'e, &'ty Ty<'s, 'ty>, Position>, Error> {
  read_expr_1(arena, ty_arena, lexer)
}

pub fn read_expr_1<'s, 'e, 'ty>(
  arena: &'e Arena<Expr<'s, 'e, &'ty Ty<'s, 'ty>, Position>>,
  ty_arena: &'ty Arena<Ty<'s, 'ty>>,
  lexer: &mut Lexer<'s>,
) -> Result<&'e Expr<'s, 'e, &'ty Ty<'s, 'ty>, Position>, Error> {
  let mut callee = read_expr_2(arena, ty_arena, lexer)?;
  loop {
    let lexer_clone = lexer.clone();
    match read_expr_2(arena, ty_arena, lexer) {
      Ok(argument) => {
        let call = Expr(callee.0, ExprF::App(callee, argument));
        callee = arena.alloc(call);
      },
      Err(_) => {
        lexer.clone_from(&lexer_clone);
        break;
      },
    }
  }
  Ok(callee)
}

pub fn read_expr_2<'s, 'e, 'ty>(
  arena: &'e Arena<Expr<'s, 'e, &'ty Ty<'s, 'ty>, Position>>,
  ty_arena: &'ty Arena<Ty<'s, 'ty>>,
  lexer: &mut Lexer<'s>,
) -> Result<&'e Expr<'s, 'e, &'ty Ty<'s, 'ty>, Position>, Error> {
  let Lexeme(position, lexeme) = read_lexeme(lexer)?;
  match lexeme {
    LexemeF::Identifier(name) => {
      let expr = Expr(position, ExprF::Var(name));
      Ok(arena.alloc(expr))
    },
    LexemeF::False =>
      Ok(arena.alloc(Expr(position, ExprF::Lit(Literal::Bool(false))))),
    LexemeF::True =>
      Ok(arena.alloc(Expr(position, ExprF::Lit(Literal::Bool(true))))),
    LexemeF::Fun => {
      let name = read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::Identifier(name) => Ok(name),
        _ => Err(Error(p, "expected lambda parameter")),
      })?;

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::Arrow => Ok(()),
        _ => Err(Error(p, "expected `->`")),
      })?;

      let body = read_expr(arena, ty_arena, lexer)?;

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::End => Ok(()),
        _ => Err(Error(p, "expected `end`")),
      })?;

      let expr = Expr(position, ExprF::Abs(name, body));
      Ok(arena.alloc(expr))
    },
    LexemeF::Let => {
      let mut bindings = vec![];

      loop {
        let lexer_clone = lexer.clone();

        match read_lexeme(lexer) {
          Ok(Lexeme(_, LexemeF::Val)) => (),
          _ => {
            lexer.clone_from(&lexer_clone);
            break;
          },
        }

        let name = read_lexeme_if(lexer, |Lexeme(p, l)| match l {
          LexemeF::Identifier(name) => Ok(name),
          _ => Err(Error(p, "expected let binding name")),
        })?;

        let ty = match peek_lexeme(lexer)? {
          Lexeme(_, LexemeF::Colon) => {
            read_lexeme(lexer)?;
            let ty = read_ty_expr(ty_arena, lexer)?;
            Some(ty)
          },
          _ => None,
        };

        read_lexeme_if(lexer, |Lexeme(p, l)| match l {
          LexemeF::Equals => Ok(()),
          _ => Err(Error(p, "expected `=`")),
        })?;

        let value = match read_expr(arena, ty_arena, lexer) {
          Ok(expr) => expr,
          Err(_) => {
            lexer.clone_from(&lexer_clone);
            break
          },
        };

        bindings.push((name, ty, value));
      }

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::In => Ok(()),
        _ => Err(Error(p, "expected `in`")),
      })?;

      let body = read_expr(arena, ty_arena, lexer)?;

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::End => Ok(()),
        _ => Err(Error(p, "expected `end`")),
      })?;

      let mut expr = body;
      for &(name, ty, value) in bindings.iter().rev() {
        expr = arena.alloc(Expr(position, ExprF::Let(name, ty, value, expr)));
      }
      Ok(expr)
    },
    LexemeF::LeftParenthesis => {
      let expr = read_expr(arena, ty_arena, lexer)?;
      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::RightParenthesis => Ok(()),
        _ => Err(Error(p, "expected `)`")),
      })?;
      Ok(expr)
    },
    LexemeF::LeftBrace => {
      let mut elems = vec![];
      loop {
        let lexer_clone = lexer.clone();
        match read_expr(arena, ty_arena, lexer) {
          Ok(expr) => elems.push(expr),
          Err(_) => {
            lexer.clone_from(&lexer_clone);
            break
          },
        }
        match read_lexeme(lexer) {
          Ok(Lexeme(_, LexemeF::Comma)) => (),
          _ => {
            lexer.clone_from(&lexer_clone);
            break;
          },
        }
      }
      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::RightBrace => Ok(()),
        _ => Err(Error(p, "expected `}`")),
      })?;
      let expr = Expr(position, ExprF::Tup(elems));
      Ok(arena.alloc(expr))
    },
    LexemeF::Str(value) => {
      let expr = Expr(position, ExprF::Lit(Literal::Str(value)));
      Ok(arena.alloc(expr))
    },
    _ => Err(Error(position, "expected expression")),
  }
}

pub fn read_ty_expr<'s, 'ty>(
  ty_arena: &'ty Arena<Ty<'s, 'ty>>,
  lexer: &mut Lexer<'s>,
) -> Result<&'ty Ty<'s, 'ty>, Error> {
  read_ty_expr_1(ty_arena, lexer)
}

pub fn read_ty_expr_1<'s, 'ty>(
  ty_arena: &'ty Arena<Ty<'s, 'ty>>,
  lexer: &mut Lexer<'s>,
) -> Result<&'ty Ty<'s, 'ty>, Error> {
  let mut components = vec![read_ty_expr_2(ty_arena, lexer)?];
  loop {
    let lexer_clone = lexer.clone();
    match read_lexeme(lexer) {
      Ok(Lexeme(_, LexemeF::Arrow)) => (),
      _ => {
        lexer.clone_from(&lexer_clone);
        break;
      },
    }
    match read_ty_expr_2(ty_arena, lexer) {
      Ok(argument) => {
        components.push(argument);
      },
      Err(_) => {
        lexer.clone_from(&lexer_clone);
        break;
      },
    }
  }
  let mut ty = *components.last().unwrap();
  for component in components.iter().rev().skip(1) {
    let new_ty = Ty::Func(component, ty);
    ty = ty_arena.alloc(new_ty);
  }
  Ok(ty)
}

pub fn read_ty_expr_2<'s, 'ty>(
  ty_arena: &'ty Arena<Ty<'s, 'ty>>,
  lexer: &mut Lexer<'s>,
) -> Result<&'ty Ty<'s, 'ty>, Error> {
  let Lexeme(position, lexeme) = read_lexeme(lexer)?;
  match lexeme {
    LexemeF::Identifier(name) => {
      let ty = Ty::Var(name);
      Ok(ty_arena.alloc(ty))
    },
    LexemeF::Forall => {
      let name = read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::Identifier(name) => Ok(name),
        _ => Err(Error(p, "expected type variable")),
      })?;

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::Comma => Ok(()),
        _ => Err(Error(p, "expected ','")),
      })?;

      let inner = read_ty_expr(ty_arena, lexer)?;

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::End => Ok(()),
        _ => Err(Error(p, "expected 'end'")),
      })?;

      let ty = Ty::Forall(name, inner);
      Ok(ty_arena.alloc(ty))
    },
    LexemeF::LeftParenthesis => {
      let ty = read_ty_expr(ty_arena, lexer)?;
      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::RightParenthesis => Ok(()),
        _ => Err(Error(p, "expected `)`")),
      })?;
      Ok(ty)
    },
    _ => Err(Error(position, "expected type")),
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_read_var_expr() {
    let ty_arena = Arena::new();
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("foo")),
      Ok(&Expr(Position::new(0, 1, 1), ExprF::Var("foo")))
    );
  }

  #[test]
  fn test_read_abs_expr() {
    let ty_arena = Arena::new();
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("fun foo -> bar end")),
      Ok(&Expr(
        Position::new(0, 1, 1),
        ExprF::Abs(
          "foo",
          &Expr(
            Position::new(11, 1, 12),
            ExprF::Var("bar"),
          ),
        ),
      ))
    );
  }

  #[test]
  fn test_read_app_expr() {
    let ty_arena = Arena::new();
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("foo bar baz")),
      Ok(&Expr(
        Position::new(0, 1, 1),
        ExprF::App(
          &Expr(
            Position::new(0, 1, 1),
            ExprF::App(
              &Expr(
                Position::new(0, 1, 1),
                ExprF::Var("foo"),
              ),
              &Expr(
                Position::new(4, 1, 5),
                ExprF::Var("bar"),
              ),
            ),
          ),
          &Expr(
            Position::new(8, 1, 9),
            ExprF::Var("baz"),
          ),
        ),
      ))
    );
  }

  #[test]
  fn test_read_tup_expr() {
    let ty_arena = Arena::new();
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("{}")),
      Ok(&Expr(Position::new(0, 1, 1), ExprF::Tup(vec![])))
    );
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("{x,}")),
      Ok(&Expr(
        Position::new(0, 1, 1),
        ExprF::Tup(vec![
          &Expr(
            Position::new(1, 1, 2),
            ExprF::Var("x"),
          ),
        ]),
      ))
    );
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("{x,y,}")),
      Ok(&Expr(
        Position::new(0, 1, 1),
        ExprF::Tup(vec![
          &Expr(
            Position::new(1, 1, 2),
            ExprF::Var("x"),
          ),
          &Expr(
            Position::new(3, 1, 4),
            ExprF::Var("y"),
          ),
        ]),
      ))
    );
  }

  #[test]
  fn test_read_paren_expr() {
    let ty_arena = Arena::new();
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &ty_arena, &mut Lexer::new("(foo)")),
      Ok(&Expr(Position::new(1, 1, 2), ExprF::Var("foo")))
    );
  }
}
