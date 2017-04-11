use lex::{Lexeme, LexemeF, Lexer, Position};
use syntax::{Expr, ExprF};
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Error(Position, &'static str);

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
  let lexeme = try!(read_lexeme(lexer));
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
  read_expr_2(arena, lexer)
}

pub fn read_expr_2<'a, 'b>(
  arena: &'b Arena<Expr<'a, 'b, Position>>,
  lexer: &mut Lexer<'a>,
) -> Result<&'b Expr<'a, 'b, Position>, Error> {
  let Lexeme(position, lexeme) = try!(read_lexeme(lexer));
  match lexeme {
    LexemeF::Identifier(name) => {
      let expr = Expr(position, ExprF::Var(name));
      Ok(arena.alloc(expr))
    },
    LexemeF::Fun => {
      let name = try!(read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::Identifier(name) => Ok(name),
        _ => Err(Error(p, "expected lambda parameter")),
      }));

      try!(read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::Arrow => Ok(()),
        _ => Err(Error(p, "expected `->`")),
      }));

      let body = try!(read_expr(arena, lexer));

      try!(read_lexeme_if(lexer, |Lexeme(p, l)| match l {
        LexemeF::End => Ok(()),
        _ => Err(Error(p, "expected `end`")),
      }));

      let expr = Expr(position, ExprF::Abs(name, body));
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
    let mut arena = Arena::new();
    assert_eq!(
      read_expr(&mut arena, &mut Lexer::new("foo")),
      Ok(&Expr(Position::new(0, 1, 1), ExprF::Var("foo")))
    );
  }

  #[test]
  fn test_read_abs_expr() {
    let mut arena = Arena::new();
    assert_eq!(
      read_expr(&mut arena, &mut Lexer::new("fun foo -> bar end")),
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
}
