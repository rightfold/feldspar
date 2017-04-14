use bytecode::Inst;
use value::{GC, Ref};

pub struct Thread<'a, 'b> {
  gc: &'b GC,
  call_stack: Vec<StackFrame<'a, 'b>>,
  eval_stack: Vec<Ref<'b>>,
}

struct StackFrame<'a, 'b> {
  bytecode: &'a [Inst],
  pcounter: usize,
  locals: Vec<Option<Ref<'b>>>,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Status {
  Paused,
  Finished,
}

impl<'a, 'b> Thread<'a, 'b> {
  pub fn new(gc: &'b GC, bytecode: &'a [Inst], locals: usize) -> Self {
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
      call_stack: vec![stack_frame],
      eval_stack: vec![],
    }
  }

  pub fn resume(&mut self) -> Status {
    let mut pop_call_stack = false;
    loop {
      if pop_call_stack {
        self.call_stack.pop();
        pop_call_stack = false;
      }
      match self.call_stack.last_mut() {
        None => return Status::Finished,
        Some(stack_frame) => {
          match stack_frame.bytecode[stack_frame.pcounter] {
            Inst::NoOp =>
              stack_frame.pcounter += 1,
            Inst::Return =>
              pop_call_stack = true,

            Inst::Pop => {
              self.eval_stack.pop().unwrap();
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
  fn test_resume() {
    let gc = GC::new();
    let bytecode = [
      Inst::NewBool(true),
      Inst::NewBool(false),
      Inst::New(2, 0),
      Inst::Return,
    ];
    let mut thread = Thread::new(&gc, &bytecode, 0);
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
