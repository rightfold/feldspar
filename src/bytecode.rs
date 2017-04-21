#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StrID(pub usize);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ChunkID(pub usize);

/// A chunk of bytecode, pairing instructions with metadata.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Chunk {
  /// The bytecode instructions to be executed, starting at the first one.
  pub insts: Vec<Inst>,

  /// The amount of slots to allocate for local variables.
  pub locals: usize,

  /// The amount of free variables to capture when creating a function from
  /// this chunk.
  pub captures: usize,
}

/// A bytecode instruction.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Inst {
  /// A no-op, which does nothing.
  NoOp,

  /// Pop an argument off the stack, pop a callee of the stack, and call the
  /// callee with the argument.
  Call,

  /// Return to the caller.
  Return,

  /// Pop a value and discard it.
  Pop,

  /// Push a local variable onto the stack.
  GetLocal(usize),

  /// Pop elements of the stack, create a new tuple with those elements, and
  /// push the new tuple onto the stack.
  NewTuple(usize),

  /// Push a new integer onto the stack.
  NewI32(i32),

  /// Push a new string onto the stack.
  NewStr(StrID),

  /// Pop free variables off the stack, create a new function with those free
  /// variables as captures, and push the new function onto the stack.
  NewFunc(ChunkID),

  /// Pop a buffer off the stack, pop a handle off the stack, write the
  /// buffer to the handle, and push the number of bytes written onto the
  /// stack.
  Write,
}
