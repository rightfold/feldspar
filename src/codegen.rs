use bytecode::{Chunk, ChunkID, Inst, StrID};
use std::collections::HashMap;
use syntax::{Expr, ExprF, Literal};

pub struct Codegen<'s> {
  strs: Vec<&'s str>,
  chunks: Vec<Chunk>,
}

impl<'s> Codegen<'s> {
  pub fn new() -> Self { Codegen{strs: vec![], chunks: vec![]} }

  pub fn str(&self, id: StrID) -> &'s str { self.strs[id.0] }
  pub fn chunk(&self, id: ChunkID) -> &Chunk { &self.chunks[id.0] }

  fn new_str(&mut self, str: &'s str) -> StrID {
    self.strs.push(str);
    StrID(self.strs.len() - 1)
  }

  fn new_chunk(&mut self, locals: usize, captures: usize, insts: Vec<Inst>) -> ChunkID {
    self.chunks.push(Chunk{insts: insts, locals: locals, captures: captures});
    ChunkID(self.chunks.len() - 1)
  }

  pub fn codegen_func<'e, T>(
    &mut self,
    env: &HashMap<&str, usize>,
    captures: usize,
    body: &Expr<'s, 'e, T>,
  ) -> ChunkID  {
    let mut insts = vec![];
    self.codegen_expr(env, body, &mut insts);
    insts.push(Inst::Return);
    self.new_chunk(1 + captures, captures, insts)
  }

  pub fn codegen_expr<'e, T>(
    &mut self,
    env: &HashMap<&str, usize>,
    expr: &Expr<'s, 'e, T>,
    insts: &mut Vec<Inst>,
  ) {
    match expr.1 {
      ExprF::Lit(ref lit) =>
        self.codegen_literal(lit, insts),
      ExprF::Var("stdout#") => // TODO: Move to feldspar::builtin
        insts.push(Inst::NewI32(1)),
      ExprF::Var("to_utf8#") => { // TODO: Move to feldspar::builtin
        let chunk_id = self.new_chunk(1, 0, vec![
          Inst::GetLocal(0),
          Inst::Return,
        ]);
        insts.push(Inst::NewFunc(chunk_id));
      },
      ExprF::Var("write#") => { // TODO: Move to feldspar::builtin
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
      ExprF::Var(name) =>
        insts.push(Inst::GetLocal(env[name])),
      ExprF::Abs(param, body) => {
        let mut body_env = HashMap::new();
        body_env.insert(param, 0);
        for (i, (k, &v)) in env.iter().enumerate() {
          body_env.insert(k, i + 1);
          insts.push(Inst::GetLocal(v));
        }
        let chunk_id = self.codegen_func(&body_env, env.len(), body);
        insts.push(Inst::NewFunc(chunk_id));
      },
      ExprF::App(callee, argument) => {
        self.codegen_expr(env, callee, insts);
        self.codegen_expr(env, argument, insts);
        insts.push(Inst::Call);
      },
      ExprF::Tup(ref elems) => {
        for elem in elems {
          self.codegen_expr(env, elem, insts);
        }
        insts.push(Inst::New(elems.len(), 0));
      },
    }
  }

  pub fn codegen_literal(&mut self, lit: &Literal<'s>, insts: &mut Vec<Inst>) {
    match *lit {
      Literal::Bool(value) =>
        insts.push(Inst::NewI32(value as i32)),
      Literal::Str(value) =>
        insts.push(Inst::NewStr(self.new_str(value))),
      _ => panic!("codegen_literal: NYI"),
    }
  }
}
