use nom::IResult;
use typed_arena::Arena;

#[derive(Clone, Copy, Debug)]
pub struct Pos<'s>(pub &'s [u8], pub usize);

impl<'s> Pos<'s> {
  pub fn offset(&self) -> usize {
    self.1
  }

  pub fn line(&self) -> usize {
    let mut line = 1;
    for (i, &byte) in self.0.iter().enumerate() {
      if i == self.offset() {
        break;
      }
      if byte as char == '\n' {
        line += 1;
      }
    }
    line
  }
}

#[derive(Clone, Debug)]
pub enum Expr<'s, 'e> where 's: 'e {
  Bool(Pos<'s>, bool),
  Abs (Pos<'s>, &'s str, &'e Expr<'s, 'e>),
}

impl<'s, 'e> Expr<'s, 'e> {
  pub fn pos(&self) -> Pos<'s> {
    match *self {
      Expr::Bool(pos, _)    => pos,
      Expr::Abs (pos, _, _) => pos,
    }
  }
}

pub mod lex {
  // FIXME: Use is_xid_start and is_xid_continue.
  // FIXME: Blacklist keywords.
  named!(pub ident<&str, &str>, take_while1!(char::is_alphabetic));

  pub mod keyw {
    named!(pub end   <&str, &str>, tag!("end"));
    named!(pub false_<&str, &str>, tag!("false"));
    named!(pub fun   <&str, &str>, tag!("fun"));
    named!(pub true_ <&str, &str>, tag!("true"));
  }

  pub mod punc {
    named!(pub arrow<&str, &str>, tag!("->"));
  }
}

pub mod expr {
  use super::*;
  use super::Expr::*;

  type A<'s, 'e> = &'e Arena<Expr<'s, 'e>>;
  type I<'s>     = &'s str;
  type O<'s, 'e> = &'e mut Expr<'s, 'e>;

  static POS: Pos<'static> = Pos(b"", 0);

  pub fn level_1<'s, 'e>(i: I<'s>, a: A<'s, 'e>) -> IResult<I<'s>, O<'s, 'e>> {
    call!(i, level_2, a)
  }

  pub fn level_2<'s, 'e>(i: I<'s>, a: A<'s, 'e>) -> IResult<I<'s>, O<'s, 'e>> {
    alt!(i,
      call!(bool, a) |
      call!(fun,  a)
    )
  }

  pub fn bool<'s, 'e>(i: I<'s>, a: A<'s, 'e>) -> IResult<I<'s>, O<'s, 'e>> {
    alt!(i,
      map!(call!(lex::keyw::false_), |_| a.alloc(Bool(POS, false))) |
      map!(call!(lex::keyw::true_),  |_| a.alloc(Bool(POS, true)))
    )
  }

  pub fn fun<'s, 'e>(i: I<'s>, a: A<'s, 'e>) -> IResult<I<'s>, O<'s, 'e>> {
    do_parse!(i,
      call!(lex::keyw::fun) >>
      param: call!(lex::ident) >>
      call!(lex::punc::arrow) >>
      body: call!(level_2, a) >>
      call!(lex::keyw::end) >>
      (a.alloc(Expr::Abs(POS, param, body)))
    )
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod expr_test {
    use super::*;

    #[test]
    fn test_bool() {
      let a = Arena::new();
      let r = expr::bool("false", &a);
      println!("{:?}", r);
    }
  }
}
