use bytecode::{Chunk, ChunkID, Inst, StrID};
use std::collections::HashMap;
use std::mem;
use std::ops::DerefMut;
use syntax::Expr;
use syntax::Expr::*;

pub struct Codegen {
  strs: Vec<String>,
  chunks: Vec<Chunk>,
}

impl Codegen {
  pub fn new() -> Self { Codegen{strs: vec![], chunks: vec![]} }

  pub fn str(&self, id: StrID) -> &str { &self.strs[id.0] }
  pub fn chunk(&self, id: ChunkID) -> &Chunk { &self.chunks[id.0] }

  fn new_str(&mut self, str: String) -> StrID {
    self.strs.push(str);
    StrID(self.strs.len() - 1)
  }

  fn new_chunk(&mut self, locals: usize, captures: usize, insts: Vec<Inst>) -> ChunkID {
    self.chunks.push(Chunk{insts: insts, locals: locals, captures: captures});
    ChunkID(self.chunks.len() - 1)
  }

  pub fn codegen_func<'e, 't>(
    &mut self,
    env: &HashMap<&str, usize>,
    captures: usize,
    body: &Expr<'e, 't>,
  ) -> ChunkID {
    let mut insts = vec![];
    self.codegen_expr(env, body, &mut insts);
    insts.push(Inst::Return);
    self.new_chunk(1 + captures, captures, insts)
  }

  pub fn codegen_expr<'e, 't>(
    &mut self,
    env: &HashMap<&str, usize>,
    expr: &Expr<'e, 't>,
    insts: &mut Vec<Inst>,
  ) {
    match expr {
      &Bool(_, value) =>
        insts.push(Inst::NewI32(value as i32)),
      &Str(_, ref value_cell) => {
        let mut value = String::new();
        let mut borrow = value_cell.borrow_mut();
        mem::swap(borrow.deref_mut(), &mut value);
        let str_id = self.new_str(value);
        insts.push(Inst::NewStr(str_id));
      },
      &Var(_, ref name) =>
        match name.as_ref() {
          "stdout%" =>
            insts.push(Inst::Stdout),
          "to_utf8%" => {
            let chunk_id = self.new_chunk(1, 0, vec![
              Inst::GetLocal(0),
              Inst::Return,
            ]);
            insts.push(Inst::NewFunc(chunk_id));
          },
          "write%" => {
            let action_chunk_id = self.new_chunk(3, 2, vec![
              Inst::GetLocal(1), // handle
              Inst::GetLocal(2), // bytes
              Inst::Write,
              Inst::Return,
            ]);
            let curry2_chunk_id = self.new_chunk(2, 1, vec![
              Inst::GetLocal(0), // bytes
              Inst::GetLocal(1), // handle
              Inst::NewFunc(action_chunk_id),
              Inst::Return,
            ]);
            let curry1_chunk_id = self.new_chunk(1, 0, vec![
              Inst::GetLocal(0), // handle
              Inst::NewFunc(curry2_chunk_id),
              Inst::Return,
            ]);
            insts.push(Inst::NewFunc(curry1_chunk_id));
          },
          name =>
            insts.push(Inst::GetLocal(env[name])),
        },
      &Abs(_, ref param, body) => {
        let mut body_env = HashMap::<&str, _>::new();
        body_env.insert(param, 0);
        for (i, (ref k, &v)) in env.iter().enumerate() {
          body_env.insert(k, i + 1);
          insts.push(Inst::GetLocal(v));
        }
        let chunk_id = self.codegen_func(&body_env, env.len(), body);
        insts.push(Inst::NewFunc(chunk_id));
      },
      &Let(pos, ref name, _, value, body) => {
        let abs = Abs(pos, name.clone(), body);
        let app = App(pos, &abs, value);
        self.codegen_expr(env, &app, insts);
      },
      &App(_, callee, argument) => {
        self.codegen_expr(env, callee, insts);
        self.codegen_expr(env, argument, insts);
        insts.push(Inst::Call);
      },
    }
  }
}
