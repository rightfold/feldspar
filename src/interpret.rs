use bytecode::{Chunk, ChunkID, Inst, StrID};
use libc;
use std::mem;
use value::{GC, Root};

pub struct StateDiff {
  pub jump: Jump,
  pub return_: bool,
  pub call: Option<(Root, Root)>,
}

pub enum Jump {
  Absolute(usize),
  Relative(isize),
}

pub fn interpret<'str, 'gc, 'chunk, GetChunk, GetStr>(
  gc: &'gc GC,
  get_str: &GetStr,
  get_chunk: &GetChunk,
  stack: &mut Vec<Root>,
  locals: &mut [Root],
  inst: &Inst,
) -> StateDiff
  where
    GetStr: Fn(StrID) -> &'str str,
    GetChunk: Fn(ChunkID) -> &'chunk Chunk {
  let mut state_diff = StateDiff{
    jump: Jump::Relative(1),
    return_: false,
    call: None,
  };
  match *inst {
    Inst::NoOp => (),

    Inst::Call => {
      let argument = stack.pop().unwrap();
      let callee = stack.pop().unwrap();
      state_diff.call = Some((callee, argument));
    },
    Inst::Return =>
      state_diff.return_ = true,

    Inst::Pop =>
      drop(stack.pop()),

    Inst::GetLocal(offset) =>
      stack.push(locals[offset].clone()),

    Inst::NewTuple(elem_count) => {
      let mut ptrs = Vec::with_capacity(elem_count);
      for _ in 0 .. elem_count {
        let ptr = stack.pop().unwrap();
        ptrs.push(ptr);
      }
      ptrs.reverse();
      let new = gc.alloc_tuple(&ptrs);
      stack.push(new);
    },
    Inst::NewI32(value) =>
      stack.push(gc.alloc_i32(value)),
    Inst::NewStr(str_id) => {
      let str_ref = get_str(str_id);
      let new = gc.alloc_str(str_ref.to_string());
      stack.push(new);
    },
    Inst::NewFunc(chunk_id) => {
      let chunk = get_chunk(chunk_id);
      let mut ptrs = Vec::with_capacity(chunk.captures);
      for _ in 0 .. chunk.captures {
        let ptr = stack.pop().unwrap();
        ptrs.push(ptr);
      }
      ptrs.reverse();
      let new = gc.alloc_closure(chunk_id, &ptrs);
      stack.push(new);
    },

    Inst::Write => {
      let handle = stack.pop().unwrap();
      let bytes_root = stack.pop().unwrap();
      let bytes = bytes_root.bytes().unwrap();

      let status =
        unsafe {
          libc::write(
            handle.i32().unwrap(),
            mem::transmute(bytes.as_ptr()),
            bytes.len(),
          ) as i32
        };

      let result = gc.alloc_i32(status);
      stack.push(result);
    },
  }
  state_diff
}
