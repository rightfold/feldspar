use bytecode::{Chunk, ChunkID, Inst, StrID};
use interpret::{Jump, interpret};
use value::{GC, Root};

pub struct Thread<'chunk, 'gc, GetStr, GetChunk> {
  gc: &'gc GC,
  get_str: GetStr,
  get_chunk: GetChunk,
  call_stack: Vec<StackFrame<'chunk, 'gc>>,
  eval_stack: Vec<Root<'gc>>,
}

struct StackFrame<'chunk, 'gc> {
  bytecode: &'chunk [Inst],
  pcounter: usize,
  locals: Vec<Root<'gc>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Status {
  Paused,
  Finished,
}

impl<'str, 'chunk, 'gc, GetStr, GetChunk> Thread<'chunk, 'gc, GetStr, GetChunk>
  where
    GetStr: Fn(StrID) -> &'str str,
    GetChunk: Fn(ChunkID) -> &'chunk Chunk {
  pub fn new(
    gc: &'gc GC,
    get_str: GetStr,
    get_chunk: GetChunk,
    bytecode: &'chunk [Inst],
    locals: usize,
  ) -> Self {
    let stack_frame = StackFrame{
      bytecode: bytecode,
      pcounter: 0,
      locals: {
        let mut locals_vec = vec![];
        locals_vec.resize(locals, gc.alloc_tuple(&[]));
        locals_vec
      },
    };
    Thread{
      gc: gc,
      get_str: get_str,
      get_chunk: get_chunk,
      call_stack: vec![stack_frame],
      eval_stack: vec![],
    }
  }

  pub fn resume(&mut self) -> Status {
    loop {
      let state_diff = match self.call_stack.last_mut() {
        None => return Status::Finished,
        Some(stack_frame) => {
          let state_diff = interpret(
            self.gc,
            &self.get_str,
            &self.get_chunk,
            &mut self.eval_stack,
            &mut stack_frame.locals,
            &stack_frame.bytecode[stack_frame.pcounter],
          );
          match state_diff.jump {
            Jump::Absolute(offset) => stack_frame.pcounter = offset,
            Jump::Relative(offset) =>
              if offset < 0 {
                stack_frame.pcounter -= -offset as usize;
              } else {
                stack_frame.pcounter += offset as usize;
              },
          }
          state_diff
        },
      };
      if state_diff.return_ {
        self.call_stack.pop();
      }
      if let Some((callee, argument)) = state_diff.call {
        let chunk = (self.get_chunk)(callee.closure_chunk().unwrap());
        self.call_stack.push(StackFrame{
          bytecode: &chunk.insts,
          pcounter: 0,
          locals: {
            let mut locals_vec = vec![];
            locals_vec.resize(chunk.locals, self.gc.alloc_tuple(&[]));
            locals_vec[0] = argument;
            for offset in 0 .. chunk.captures {
              locals_vec[offset + 1] = callee.closure_capture(offset).unwrap();
            }
            locals_vec
          },
        });
      }
    }
  }
}
