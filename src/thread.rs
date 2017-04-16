use bytecode::{Chunk, Inst};
use interpret::{Jump, interpret};
use value::{GC, Ref};

pub struct Thread<'str, 'chunk, 'gc, GetChunk> where 'str: 'chunk {
  gc: &'gc GC,
  get_chunk: GetChunk,
  call_stack: Vec<StackFrame<'str, 'chunk, 'gc>>,
  eval_stack: Vec<Ref<'gc>>,
}

struct StackFrame<'str, 'chunk, 'gc> where 'str: 'chunk {
  bytecode: &'chunk [Inst<'str>],
  pcounter: usize,
  locals: Vec<Ref<'gc>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Status {
  Paused,
  Finished,
}

impl<'str, 'chunk, 'gc, GetChunk> Thread<'str, 'chunk, 'gc, GetChunk>
  where GetChunk: Fn(usize) -> &'chunk Chunk<'str> {
  pub fn new(
    gc: &'gc GC,
    get_chunk: GetChunk,
    bytecode: &'chunk [Inst<'str>],
    locals: usize,
  ) -> Self {
    let stack_frame = StackFrame{
      bytecode: bytecode,
      pcounter: 0,
      locals: {
        let mut locals_vec = vec![];
        locals_vec.resize(locals, gc.alloc(0, 0));
        locals_vec
      },
    };
    Thread{
      gc: gc,
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
        let chunk = (self.get_chunk)(*callee.aux_usize());
        self.call_stack.push(StackFrame{
          bytecode: &chunk.insts,
          pcounter: 0,
          locals: {
            let mut locals_vec = vec![];
            locals_vec.resize(chunk.locals, self.gc.alloc(0, 0));
            locals_vec[0] = argument;
            for offset in 0 .. chunk.captures {
              locals_vec[offset + 1] = callee.get_ptr(offset).unwrap();
            }
            locals_vec
          },
        });
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_call() {
    let chunk = Chunk{
      insts: vec![
        Inst::GetLocal(1),
        Inst::GetLocal(0),
        Inst::New(2, 0),
        Inst::Return,
      ],
      locals: 2,
      captures: 1,
    };
    let gc = GC::new();
    let bytecode = [
      Inst::NewI32(1),
      Inst::NewFunc(0),
      Inst::NewI32(0),
      Inst::Call,
      Inst::Return,
    ];
    let get_chunk = |_| &chunk;
    let mut thread = Thread::new(&gc, get_chunk, &bytecode, 0);
    assert_eq!(thread.resume(), Status::Finished);
    assert_eq!(thread.call_stack.len(), 0);
    assert_eq!(thread.eval_stack.len(), 1);
    assert_eq!(thread.eval_stack[0].ptr_count(), 2);
    assert_eq!(thread.eval_stack[0].aux_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().ptr_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux_count(), 4);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux_i32(), &1);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().ptr_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux_count(), 4);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux_i32(), &0);
  }

  #[test]
  fn test_new() {
    let gc = GC::new();
    let bytecode = [
      Inst::NewI32(1),
      Inst::NewI32(0),
      Inst::New(2, 0),
      Inst::Return,
    ];
    let get_chunk = |_| panic!();
    let mut thread = Thread::new(&gc, get_chunk, &bytecode, 0);
    assert_eq!(thread.resume(), Status::Finished);
    assert_eq!(thread.call_stack.len(), 0);
    assert_eq!(thread.eval_stack.len(), 1);
    assert_eq!(thread.eval_stack[0].ptr_count(), 2);
    assert_eq!(thread.eval_stack[0].aux_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().ptr_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux_count(), 4);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux_i32(), &1);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().ptr_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux_count(), 4);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux_i32(), &0);
  }
}
