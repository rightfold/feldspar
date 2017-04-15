use bytecode::{Chunk, Inst};
use std::mem;
use value::{GC, Ref};

pub struct Thread<'chunk, 'gc, GetChunk> {
  gc: &'gc GC,
  get_chunk: GetChunk,
  call_stack: Vec<StackFrame<'chunk, 'gc>>,
  eval_stack: Vec<Ref<'gc>>,
}

struct StackFrame<'chunk, 'gc> {
  bytecode: &'chunk [Inst],
  pcounter: usize,
  locals: Vec<Option<Ref<'gc>>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Status {
  Paused,
  Finished,
}

impl<'chunk, 'gc, GetChunk> Thread<'chunk, 'gc, GetChunk>
  where GetChunk: Fn(usize) -> &'chunk Chunk {
  pub fn new(
    gc: &'gc GC,
    get_chunk: GetChunk,
    bytecode: &'chunk [Inst],
    locals: usize,
  ) -> Self {
    let stack_frame = StackFrame{
      bytecode: bytecode,
      pcounter: 0,
      locals: {
        let mut locals_vec = vec![];
        locals_vec.resize(locals, None);
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
    let mut pop_call_stack = false;
    let mut push_call_stack: Option<(Ref<'gc>, Ref<'gc>)> = None;
    loop {
      if pop_call_stack {
        self.call_stack.pop();
        pop_call_stack = false;
      }
      if let Some((callee, argument)) = push_call_stack {
        let chunk = (self.get_chunk)(*callee.aux_usize());
        self.call_stack.push(StackFrame{
          bytecode: &chunk.insts,
          pcounter: 0,
          locals: {
            let mut locals_vec = vec![];
            locals_vec.resize((chunk.locals + chunk.captures) as usize, None);
            locals_vec[0] = Some(argument);
            for offset in 0 .. chunk.captures {
              locals_vec[offset as usize + 1] = callee.get_ptr(offset);
            }
            locals_vec
          },
        });
        push_call_stack = None;
      }
      match self.call_stack.last_mut() {
        None => return Status::Finished,
        Some(stack_frame) => {
          match stack_frame.bytecode[stack_frame.pcounter] {
            Inst::NoOp =>
              stack_frame.pcounter += 1,

            Inst::Call => {
              let argument = self.eval_stack.pop().unwrap();
              let callee = self.eval_stack.pop().unwrap();
              stack_frame.pcounter += 1;
              push_call_stack = Some((callee, argument));
            },
            Inst::Return =>
              pop_call_stack = true,

            Inst::Pop => {
              self.eval_stack.pop().unwrap();
              stack_frame.pcounter += 1;
            },

            Inst::GetLocal(offset) => {
              let value = stack_frame.locals[offset as usize].clone().unwrap();
              self.eval_stack.push(value);
              stack_frame.pcounter += 1;
            },

            Inst::New(ptr_count, aux_count) => {
              let new = self.gc.alloc(ptr_count, aux_count);
              for offset in (0 .. ptr_count).rev() {
                let ptr = self.eval_stack.pop().unwrap();
                new.set_ptr(offset, &ptr);
              }
              self.eval_stack.push(new);
              stack_frame.pcounter += 1;
            },
            Inst::NewBool(value) => {
              let new = self.gc.alloc(0, 1);
              new.aux()[0] = value as u8;
              self.eval_stack.push(new);
              stack_frame.pcounter += 1;
            },
            Inst::NewFunc(chunk_id) => {
              let chunk = (self.get_chunk)(chunk_id);
              let aux_count = mem::size_of::<usize>() as u16;
              let new = self.gc.alloc(chunk.captures, aux_count);
              for offset in (0 .. chunk.captures).rev() {
                let ptr = self.eval_stack.pop().unwrap();
                new.set_ptr(offset, &ptr);
              }
              *new.aux_usize() = chunk_id;
              self.eval_stack.push(new);
              stack_frame.pcounter += 1;
            },
          }
        },
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
      locals: 1,
      captures: 1,
    };
    let gc = GC::new();
    let bytecode = [
      Inst::NewBool(true),
      Inst::NewFunc(0),
      Inst::NewBool(false),
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
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux_count(), 1);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux(), &[1]);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().ptr_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux_count(), 1);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux(), &[0]);
  }

  #[test]
  fn test_new() {
    let gc = GC::new();
    let bytecode = [
      Inst::NewBool(true),
      Inst::NewBool(false),
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
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux_count(), 1);
    assert_eq!(thread.eval_stack[0].get_ptr(0).unwrap().aux(), &[1]);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().ptr_count(), 0);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux_count(), 1);
    assert_eq!(thread.eval_stack[0].get_ptr(1).unwrap().aux(), &[0]);
  }
}
