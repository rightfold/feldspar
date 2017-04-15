#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Chunk {
  pub insts: Vec<Inst>,
  pub locals: u16,
  pub captures: u16,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Inst {
  NoOp,

  Call,
  Return,

  Pop,

  GetLocal(u16),

  New(u16, u16),
  NewBool(bool),
  NewFunc(usize),
}
