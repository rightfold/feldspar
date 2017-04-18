use nom::{ErrorKind, IResult, Needed};
use pos::Pos;
use std::cell::RefCell;
use std::vec::Drain;
use syntax::Expr::*;
use syntax::Expr;
use typed_arena::Arena;

pub mod lex {
  use super::*;

  fn space(i: &str) -> IResult<&str, ()> {
    let mut chars = i.chars();
    match chars.next() {
      Some(c) if c.is_whitespace() => IResult::Done(chars.as_str(), ()),
      Some('#') => map!(i, take_until!("\n"), |_| ()),
      Some(_) => IResult::Error(ErrorKind::Custom(0)),
      None => IResult::Incomplete(Needed::Size(1)),
    }
  }

  macro_rules! token (
    (pub $name:ident<$i:ty, $o:ty>, $submac:ident!( $($args:tt)* )) => (
      #[allow(unused_variables)]
      pub fn $name( i: $i ) -> $crate::nom::IResult<$i, $o, u32> {
        delimited!(i,
          many0!(call!($crate::parse::lex::space)),
          $submac!($($args)*),
          many0!(call!($crate::parse::lex::space))
        )
      }
    );
  );

  // FIXME: Use is_xid_start and is_xid_continue.
  fn is_ident_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '%'
  }

  token!(pub ident<&str, String>, do_parse!(
    not!(call!(keyw::any)) >>
    name: take_while1!(is_ident_continue) >>
    (name.to_string())
  ));

  token!(pub str<&str, String>, do_parse!(
    tag!("\"") >>
    name: take_until!("\"") >>
    tag!("\"") >>
    (name.to_string())
  ));

  pub mod keyw {
    named!(pub any<&str, &str>, alt!(
      call!(end)    |
      call!(false_) |
      call!(fun)    |
      call!(in_)    |
      call!(let_)   |
      call!(true_)  |
      call!(val)
    ));

    token!(pub end   <&str, &str>, tag!("end"));
    token!(pub false_<&str, &str>, tag!("false"));
    token!(pub fun   <&str, &str>, tag!("fun"));
    token!(pub in_   <&str, &str>, tag!("in"));
    token!(pub let_  <&str, &str>, tag!("let"));
    token!(pub true_ <&str, &str>, tag!("true"));
    token!(pub val   <&str, &str>, tag!("val"));
  }

  pub mod punc {
    token!(pub arrow      <&str, &str>, tag!("->"));
    token!(pub equals     <&str, &str>, tag!("="));
    token!(pub left_paren <&str, &str>, tag!("("));
    token!(pub right_paren<&str, &str>, tag!(")"));
  }
}

pub mod expr {
  use super::*;

  type A<'e, 't> = &'e Arena<Expr<'e, 't>>;
  type I<'s>     = &'s str;
  type O<'e, 't> = &'e Expr<'e, 't>;

  static POS: Pos = Pos(0);

  pub fn level_1<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    map!(i, fold_many1!(call!(level_2, a), None, |acc, arg| {
      match acc {
        Some(callee) => Some(&*a.alloc(App(POS, callee, arg))),
        None => Some(arg),
      }
    }), Option::unwrap)
  }

  pub fn level_2<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    alt!(i,
      do_parse!(
        call!(lex::punc::left_paren) >>
        expr: call!(level_1, a) >>
        call!(lex::punc::right_paren) >>
        (expr)
      ) |
      call!(bool, a) |
      call!(str,  a) |
      call!(var,  a) |
      call!(abs,  a) |
      call!(let_, a)
    )
  }

  pub fn bool<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    alt!(i,
      map!(call!(lex::keyw::false_), |_| &*a.alloc(Bool(POS, false))) |
      map!(call!(lex::keyw::true_),  |_| &*a.alloc(Bool(POS, true)))
    )
  }

  pub fn str<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    map!(i, call!(lex::str), |x| &*a.alloc(Str(POS, RefCell::new(x))))
  }

  pub fn var<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    map!(i, call!(lex::ident), |x| &*a.alloc(Var(POS, x)))
  }

  pub fn abs<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    do_parse!(i,
      call!(lex::keyw::fun) >>
      param: call!(lex::ident) >>
      call!(lex::punc::arrow) >>
      body: call!(level_1, a) >>
      call!(lex::keyw::end) >>
      (a.alloc(Abs(POS, param, body)))
    )
  }

  pub fn let_<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    let make_let = |acc, mut vals|
      drain_all(&mut vals)
      .rev()
      .fold(acc, |acc, (name, value)|
        a.alloc(Let(POS, name, None, value, acc)));
    do_parse!(i,
      call!(lex::keyw::let_) >>
      vals: many0!(do_parse!(
        call!(lex::keyw::val) >>
        name: call!(lex::ident) >>
        call!(lex::punc::equals) >>
        value: call!(level_1, a) >>
        (name, value)
      )) >>
      call!(lex::keyw::in_) >>
      body: call!(level_1, a) >>
      call!(lex::keyw::end) >>
      (make_let(body, vals))
    )
  }

  fn drain_all<T>(vec: &mut Vec<T>) -> Drain<T> {
    let range = 0 .. vec.len();
    vec.drain(range)
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
      let r = expr::level_1("false", &a);
      println!("{:?}", r);
    }

    #[test]
    fn test_var() {
      let a = Arena::new();
      let r = expr::level_1("fantastic", &a);
      println!("{:?}", r);
    }

    #[test]
    fn test_abs() {
      let a = Arena::new();
      let r = expr::level_1("fun a -> a end", &a);
      println!("{:?}", r);
    }

    #[test]
    fn test_app() {
      let a = Arena::new();
      let r = expr::level_1("foo bar (baz qux)", &a);
      println!("{:?}", r);
    }

    #[test]
    fn test_let() {
      let a = Arena::new();
      {
        let r = expr::level_1("let val x = y in z end", &a);
        println!("{:?}", r);
      }
      {
        let r = expr::level_1("let val v = w val x = y in z end", &a);
        println!("{:?}", r);
      }
    }
  }
}
