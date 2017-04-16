use bytecode::{Chunk, Inst};
use libc;
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

pub fn interpret<'str, 'gc, 'chunk, GetChunk, GetStr>(
  gc: &'gc GC,
  get_str: &GetStr,
  get_chunk: &GetChunk,
  stack: &mut Vec<Ref<'gc>>,
  locals: &mut [Ref<'gc>],
  inst: &Inst,
) -> StateDiff<'gc>
  where
    GetStr: Fn(usize) -> &'str str,
    GetChunk: Fn(usize) -> &'chunk Chunk {
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
    Inst::NewI32(value) => {
      let new = gc.alloc(0, 4);
      *new.aux_i32() = value;
      stack.push(new);
    },
    Inst::NewStr(str_id) => {
      let value = get_str(str_id);
      let new = gc.alloc(0, value.len());
      new.aux().copy_from_slice(value.as_bytes());
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

    Inst::Write => {
      let handle = stack.pop().unwrap();
      let bytes = stack.pop().unwrap();

      let status = unsafe {
        libc::write(
          *handle.aux_i32(),
          mem::transmute(bytes.aux().as_ptr()),
          bytes.aux().len(),
        ) as i32
      };

      let result = gc.alloc(0, 4);
      *result.aux_i32() = status;
      stack.push(result);
    },
  }
  state_diff
}
