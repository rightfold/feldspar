use bytecode::{Chunk, ChunkID, Inst, StrID};
use std::io;
use value::{GC, Root};

/// A description of how the executed instruction would change the call stack.
/// If `return_` is `true`, and `call` is not `None`, a tail call will occur.
pub struct StateDiff<'gc> {
  /// Where to jump to.
  pub jump: Jump,

  /// Whether to return.
  pub return_: bool,

  /// What function to call, if any, and with which argument.
  pub call: Option<(Root<'gc>, Root<'gc>)>,
}

/// Jump to a different instruction within the same chunk.
pub enum Jump {
  Absolute(usize),
  Relative(isize),
}

/// An interpretation error.
#[derive(Clone, Debug)]
pub enum Error {
  StackUnderflow,
  UnknownStr,
  UnknownChunk,
  NotABytes,
  NotAFileHandle,
}

fn pop<'gc>(stack: &mut Vec<Root<'gc>>) -> Result<Root<'gc>, Error> {
  stack.pop().ok_or(Error::StackUnderflow)
}

/// Interpret an instruction.
pub fn interpret<'str, 'gc, 'chunk, GetChunk, GetStr>(
  gc: &'gc GC,
  get_str: &GetStr,
  get_chunk: &GetChunk,
  stack: &mut Vec<Root<'gc>>,
  locals: &mut [Root<'gc>],
  inst: &Inst,
) -> Result<StateDiff<'gc>, Error>
  where
    GetStr: Fn(StrID) -> Option<&'str str>,
    GetChunk: Fn(ChunkID) -> Option<&'chunk Chunk> {
  let mut state_diff = StateDiff{
    jump: Jump::Relative(1),
    return_: false,
    call: None,
  };

  match *inst {
    Inst::NoOp => (),

    Inst::Call => {
      let argument = pop(stack)?;
      let callee = pop(stack)?;
      state_diff.call = Some((callee, argument));
    },

    Inst::Return =>
      state_diff.return_ = true,

    Inst::Pop =>
      drop(pop(stack)?),

    Inst::GetLocal(offset) =>
      stack.push(locals[offset].clone()),

    Inst::NewTuple(elem_count) => {
      let mut ptrs = Vec::with_capacity(elem_count);
      for _ in 0 .. elem_count {
        let ptr = pop(stack)?;
        ptrs.push(ptr);
      }
      ptrs.reverse();
      let new = gc.alloc_tuple(&ptrs);
      stack.push(new);
    },

    Inst::NewI32(value) =>
      stack.push(gc.alloc_i32(value)),

    Inst::NewStr(str_id) => {
      let str_ref = get_str(str_id).ok_or(Error::UnknownStr)?;
      let new = gc.alloc_str(str_ref.to_string());
      stack.push(new);
    },

    Inst::NewFunc(chunk_id) => {
      let chunk = get_chunk(chunk_id).ok_or(Error::UnknownChunk)?;
      let mut ptrs = Vec::with_capacity(chunk.captures);
      for _ in 0 .. chunk.captures {
        let ptr = pop(stack)?;
        ptrs.push(ptr);
      }
      ptrs.reverse();
      let new = gc.alloc_closure(chunk_id, &ptrs);
      stack.push(new);
    },

    Inst::Stdout => {
      let new = gc.alloc_file_handle(io::stdout());
      stack.push(new)
    },

    Inst::Write => {
      let file_handle_root = pop(stack)?;
      let bytes_root = pop(stack)?;

      let mut write = file_handle_root.file_handle_write()
                        .ok_or(Error::NotAFileHandle)?;
      let bytes = bytes_root.bytes().ok_or(Error::NotABytes)?;

      let status = write.write(bytes).map(|i| i as i32).unwrap_or(-1);

      let result = gc.alloc_i32(status);
      stack.push(result);
    },
  }

  Ok(state_diff)
}
