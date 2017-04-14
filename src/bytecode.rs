pub enum Inst {
  NoOp,
  Return,

  Pop,

  New(u16, u16),
  NewBool(bool),
}
