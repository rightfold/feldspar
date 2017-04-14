#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Chunk<'chunk> {
  pub insts: Vec<Inst<'chunk>>,
  pub locals: u16,
  pub captures: u16,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Inst<'chunk> {
  NoOp,

  Call,
  Return,

  Pop,

  New(u16, u16),
  NewBool(bool),
  NewFunc(&'chunk Chunk<'chunk>),
}
