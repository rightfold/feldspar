#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Chunk {
  pub insts: Vec<Inst>,
  pub locals: usize,
  pub captures: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Inst {
  NoOp,

  Call,
  Return,

  Pop,

  GetLocal(usize),

  New(usize, usize),
  NewBool(bool),
  NewFunc(usize),
}
