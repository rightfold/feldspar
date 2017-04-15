use bytecode::{Chunk, Inst};
use std::mem;
use value::{GC, Ref};

pub struct StateDiff<'gc> {
  pub jump: Jump,
  pub return_: bool,
  pub call: Option<(Ref<'gc>, Ref<'gc>)>,
}

pub enum Jump {
  Absolute(usize),
  Relative(isize),
}

pub fn interpret<'gc, 'chunk, GetChunk>(
  gc: &'gc GC,
  get_chunk: &GetChunk,
  stack: &mut Vec<Ref<'gc>>,
  locals: &mut [Ref<'gc>],
  inst: &Inst,
) -> StateDiff<'gc>
  where GetChunk: Fn(usize) -> &'chunk Chunk {
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
    Inst::Return => state_diff.return_ = true,

    Inst::Pop => {
      stack.pop();
    },

    Inst::GetLocal(offset) => {
      let value = locals[offset].clone();
      stack.push(value);
    },

    Inst::New(ptr_count, aux_count) => {
      let new = gc.alloc(ptr_count, aux_count);
      for offset in (0 .. ptr_count).rev() {
        let ptr = stack.pop().unwrap();
        new.set_ptr(offset, &ptr);
      }
      stack.push(new);
    },
    Inst::NewBool(value) => {
      let new = gc.alloc(0, 1);
      new.aux()[0] = value as u8;
      stack.push(new);
    },
    Inst::NewFunc(chunk_id) => {
      let chunk = get_chunk(chunk_id);
      let aux_count = mem::size_of::<usize>();
      let new = gc.alloc(chunk.captures, aux_count);
      for offset in (0 .. chunk.captures).rev() {
        let ptr = stack.pop().unwrap();
        new.set_ptr(offset, &ptr);
      }
      *new.aux_usize() = chunk_id;
      stack.push(new);
    },
  }
  state_diff
}
