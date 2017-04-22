use diagnostic::Pos;
use nom::{ErrorKind, IResult, Needed};
use std::cell::RefCell;
use std::vec::Drain;
use syntax::Expr::*;
use syntax::{Expr, Ty};
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
      call!(forall) |
      call!(fun)    |
      call!(in_)    |
      call!(let_)   |
      call!(true_)  |
      call!(val)
    ));

    token!(pub end   <&str, &str>, tag!("end"));
    token!(pub false_<&str, &str>, tag!("false"));
    token!(pub forall<&str, &str>, alt!(tag!("forall") | tag!("∀")));
    token!(pub fun   <&str, &str>, tag!("fun"));
    token!(pub in_   <&str, &str>, tag!("in"));
    token!(pub let_  <&str, &str>, tag!("let"));
    token!(pub true_ <&str, &str>, tag!("true"));
    token!(pub val   <&str, &str>, tag!("val"));
  }

  pub mod punc {
    token!(pub arrow      <&str, &str>, tag!("->"));
    token!(pub equals     <&str, &str>, tag!("="));
    token!(pub colon      <&str, &str>, tag!(":"));
    token!(pub comma      <&str, &str>, tag!(","));
    token!(pub left_paren <&str, &str>, tag!("("));
    token!(pub right_paren<&str, &str>, tag!(")"));
  }
}

pub mod expr {
  use super::*;

  type A<'e, 't> = (&'e Arena<Expr<'e, 't>>, &'t Arena<Ty<'t>>);
  type I<'s>     = &'s str;
  type O<'e, 't> = &'e Expr<'e, 't>;

  static POS: Pos = Pos(0);

  pub fn level_1<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    map!(i, fold_many1!(call!(level_2, a), None, |acc, arg| {
      match acc {
        Some(callee) => Some(&*a.0.alloc(App(POS, callee, arg))),
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
      map!(call!(lex::keyw::false_), |_| &*a.0.alloc(Bool(POS, false))) |
      map!(call!(lex::keyw::true_),  |_| &*a.0.alloc(Bool(POS, true)))
    )
  }

  pub fn str<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    map!(i, call!(lex::str), |x| &*a.0.alloc(Str(POS, RefCell::new(x))))
  }

  pub fn var<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    map!(i, call!(lex::ident), |x| &*a.0.alloc(Var(POS, x)))
  }

  pub fn abs<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    do_parse!(i,
      call!(lex::keyw::fun) >>
      param: call!(lex::ident) >>
      call!(lex::punc::arrow) >>
      body: call!(level_1, a) >>
      call!(lex::keyw::end) >>
      (a.0.alloc(Abs(POS, param, body)))
    )
  }

  pub fn let_<'s, 'e, 't>(i: I<'s>, a: A<'e, 't>) -> IResult<I<'s>, O<'e, 't>> {
    let make_let = |acc, mut vals|
      drain_all(&mut vals)
      .rev()
      .fold(acc, |acc, (name, ty, value)|
        a.0.alloc(Let(POS, name, ty, value, acc)));
    do_parse!(i,
      call!(lex::keyw::let_) >>
      vals: many0!(do_parse!(
        call!(lex::keyw::val) >>
        name: call!(lex::ident) >>
        ty: opt!(do_parse!(
          call!(lex::punc::colon) >>
          ty: call!(ty::level_1, a.1) >>
          (ty)
        )) >>
        call!(lex::punc::equals) >>
        value: call!(level_1, a) >>
        (name, ty, value)
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

pub mod ty {
  use super::*;

  type A<'t> = &'t Arena<Ty<'t>>;
  type I<'s> = &'s str;
  type O<'t> = &'t Ty<'t>;

  pub fn level_1<'s, 't>(i: I<'s>, a: A<'t>) -> IResult<I<'s>, O<'t>> {
    do_parse!(i,
      left: call!(level_2, a) >>
      right: opt!(do_parse!(
        call!(lex::punc::arrow) >>
        right: call!(level_1, a) >>
        (right)
      )) >>
      (right.iter().fold(left, |acc, ty| a.alloc(Ty::Func(acc, ty))))
    )
  }

  pub fn level_2<'s, 't>(i: I<'s>, a: A<'t>) -> IResult<I<'s>, O<'t>> {
    alt!(i,
      do_parse!(
        call!(lex::punc::left_paren) >>
        ty: call!(level_1, a) >>
        call!(lex::punc::right_paren) >>
        (ty)
      ) |
      call!(var,    a) |
      call!(forall, a)
    )
  }

  pub fn var<'s, 't>(i: I<'s>, a: A<'t>) -> IResult<I<'s>, O<'t>> {
    map!(i, call!(lex::ident), |x| &*a.alloc(Ty::Var(x)))
  }

  pub fn forall<'s, 't>(i: I<'s>, a: A<'t>) -> IResult<I<'s>, O<'t>> {
    do_parse!(i,
      call!(lex::keyw::forall) >>
      var: call!(lex::ident) >>
      call!(lex::punc::comma) >>
      inner: call!(level_1, a) >>
      (a.alloc(Ty::Forall(var, inner)))
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
      let ta = Arena::new();
      let ea = Arena::new();
      let r = expr::level_1("false", (&ea, &ta));
      println!("{:?}", r);
    }

    #[test]
    fn test_var() {
      let ta = Arena::new();
      let ea = Arena::new();
      let r = expr::level_1("fantastic", (&ea, &ta));
      println!("{:?}", r);
    }

    #[test]
    fn test_abs() {
      let ta = Arena::new();
      let ea = Arena::new();
      let r = expr::level_1("fun a -> a end", (&ea, &ta));
      println!("{:?}", r);
    }

    #[test]
    fn test_app() {
      let ta = Arena::new();
      let ea = Arena::new();
      let r = expr::level_1("foo bar (baz qux)", (&ea, &ta));
      println!("{:?}", r);
    }

    #[test]
    fn test_let() {
      let ta = Arena::new();
      let ea = Arena::new();
      {
        let r = expr::level_1("let val x = y in z end", (&ea, &ta));
        println!("{:?}", r);
      }
      {
        let r = expr::level_1("let val v = w val x = y in z end", (&ea, &ta));
        println!("{:?}", r);
      }
    }
  }
}
