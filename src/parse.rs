use lex::{Lexeme, LexemeF, Lexer, Position};
use syntax::{Expr, ExprF, Literal};
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Error(pub Position, pub &'static str);

fn read_lexeme<'a>(lexer: &mut Lexer<'a>)
  -> Result<Lexeme<'a, Position>, Error> {
  match lexer.next() {
    None => Err(Error(lexer.next_position(), "unexpected EOF")),
    Some(Err(position)) => Err(Error(position, "unexpected input")),
    Some(Ok(lexeme)) => Ok(lexeme),
  }
}

fn read_lexeme_if<'a, T, F>(lexer: &mut Lexer<'a>, pred: F)
  -> Result<T, Error>
  where F: FnOnce(Lexeme<'a, Position>) -> Result<T, Error> {
  let lexeme = read_lexeme(lexer)?;
  pred(lexeme)
}

pub fn read_expr<'a, 'b>(
  arena: &'b Arena<Expr<'a, 'b, Position>>,
  lexer: &mut Lexer<'a>,
) -> Result<&'b Expr<'a, 'b, Position>, Error> {
  read_expr_1(arena, lexer)
}

pub fn read_expr_1<'a, 'b>(
  arena: &'b Arena<Expr<'a, 'b, Position>>,
  lexer: &mut Lexer<'a>,
) -> Result<&'b Expr<'a, 'b, Position>, Error> {
  let mut callee = read_expr_2(arena, lexer)?;
  loop {
    let lexer_clone = lexer.clone();
    match read_expr_2(arena, lexer) {
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

pub fn read_expr_2<'a, 'b>(
  arena: &'b Arena<Expr<'a, 'b, Position>>,
  lexer: &mut Lexer<'a>,
) -> Result<&'b Expr<'a, 'b, Position>, Error> {
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

      let body = read_expr(arena, lexer)?;

      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::End => Ok(()),
        _ => Err(Error(p, "expected `end`")),
      })?;

      let expr = Expr(position, ExprF::Abs(name, body));
      Ok(arena.alloc(expr))
    },
    LexemeF::LeftParenthesis => {
      let expr = read_expr(arena, lexer);
      read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::RightParenthesis => Ok(()),
        _ => Err(Error(p, "expected `)`")),
      })?;
      expr
    },
    LexemeF::LeftBrace => {
      let mut elems = vec![];
      loop {
        let lexer_clone = lexer.clone();
        match read_expr(arena, lexer) {
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

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_read_var_expr() {
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &mut Lexer::new("foo")),
      Ok(&Expr(Position::new(0, 1, 1), ExprF::Var("foo")))
    );
  }

  #[test]
  fn test_read_abs_expr() {
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &mut Lexer::new("fun foo -> bar end")),
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
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &mut Lexer::new("foo bar baz")),
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
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &mut Lexer::new("{}")),
      Ok(&Expr(Position::new(0, 1, 1), ExprF::Tup(vec![])))
    );
    assert_eq!(
      read_expr(&arena, &mut Lexer::new("{x,}")),
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
      read_expr(&arena, &mut Lexer::new("{x,y,}")),
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
    let arena = Arena::new();
    assert_eq!(
      read_expr(&arena, &mut Lexer::new("(foo)")),
      Ok(&Expr(Position::new(1, 1, 2), ExprF::Var("foo")))
    );
  }
}
