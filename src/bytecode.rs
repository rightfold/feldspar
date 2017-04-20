#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StrID(pub usize);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ChunkID(pub usize);

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

  NewTuple(usize),
  NewI32(i32),
  NewStr(StrID),
  NewFunc(ChunkID),

  Write,
}
