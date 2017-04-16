#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Chunk<'str> {
  pub insts: Vec<Inst<'str>>,
  pub locals: usize,
  pub captures: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Inst<'str> {
  NoOp,

  Call,
  Return,

  Pop,

  GetLocal(usize),

  New(usize, usize),
  NewI32(i32),
  NewStr(&'str str),
  NewFunc(usize),
}
