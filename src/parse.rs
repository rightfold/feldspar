use lex::{Lexeme, LexemeF, Lexer, Position};
use syntax::{Expr, ExprF};
use typed_arena::Arena;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Error(Position, ErrorF);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ErrorF {
  UnexpectedEOF,
  ExpectedExpr,
  Lexer,
}

pub fn expr<'a, 'b>(
  arena: &'b mut Arena<Expr<'a, 'b, Position>>,
  lexer: &mut Lexer<'a>,
) -> Result<&'b Expr<'a, 'b, Position>, Error> {
  match lexer.next() {
    None => Err(Error(lexer.next_position(), ErrorF::UnexpectedEOF)),
    Some(Err(position)) => Err(Error(position, ErrorF::Lexer)),
    Some(Ok(Lexeme(position, lexeme))) =>
      match lexeme {
        LexemeF::Identifier(name) => {
          let expr = Expr(position, ExprF::Var(name));
          Ok(arena.alloc(expr))
        },
        _ => Err(Error(position, ErrorF::ExpectedExpr)),
      },
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_expr() {
    let mut arena = Arena::new();
    assert_eq!(
      expr(&mut arena, &mut Lexer::new("foo")),
      Ok(&Expr(Position::new(0, 1, 1), ExprF::Var("foo")))
    );
  }
}
