use bytecode::{Chunk, ChunkID, Inst, StrID};
use std::collections::HashMap;
use syntax::{Expr, ExprF, Literal};

pub struct Codegen<'str> {
  pub strs: Vec<&'str str>,
  pub chunks: Vec<Chunk>,
}

impl<'str> Codegen<'str> {
  pub fn new() -> Self {
    Codegen{
      strs: vec![],
      chunks: vec![],
    }
  }

  fn codegen_func_bytecode(
    &mut self,
    locals: usize,
    captures: usize,
    insts: Vec<Inst>,
  ) -> ChunkID {
    self.chunks.push(Chunk{
      insts: insts,
      locals: locals,
      captures: captures,
    });
    ChunkID(self.chunks.len() - 1)
  }

  pub fn codegen_func<'expr, T>(
    &mut self,
    env: &HashMap<&str, usize>,
    captures: usize,
    body: &Expr<'str, 'expr, T>,
  ) -> ChunkID  {
    let mut insts = vec![];
    self.codegen_expr(env, body, &mut insts);
    insts.push(Inst::Return);
    self.chunks.push(Chunk{
      insts: insts,
      locals: 1 + captures,
      captures: captures,
    });
    ChunkID(self.chunks.len() - 1)
  }

  pub fn codegen_expr<'expr, T>(
    &mut self,
    env: &HashMap<&str, usize>,
    expr: &Expr<'str, 'expr, T>,
    insts: &mut Vec<Inst>,
  ) {
    match expr.1 {
      ExprF::Lit(ref lit) =>
        self.codegen_literal(lit, insts),
      ExprF::Var("stdout#") => // TODO: Move to feldspar::builtin
        insts.push(Inst::NewI32(1)),
      ExprF::Var("to_utf8#") => { // TODO: Move to feldspar::builtin
        let chunk_id = self.codegen_func_bytecode(1, 0, vec![
          Inst::GetLocal(0),
          Inst::Return,
        ]);
        insts.push(Inst::NewFunc(chunk_id));
      },
      ExprF::Var("write#") => { // TODO: Move to feldspar::builtin
        let action_chunk_id = self.codegen_func_bytecode(3, 2, vec![
          Inst::GetLocal(1), // handle
          Inst::GetLocal(2), // bytes
          Inst::Write,
          Inst::Return,
        ]);
        let curry2_chunk_id = self.codegen_func_bytecode(2, 1, vec![
          Inst::GetLocal(0), // bytes
          Inst::GetLocal(1), // handle
          Inst::NewFunc(action_chunk_id),
          Inst::Return,
        ]);
        let curry1_chunk_id = self.codegen_func_bytecode(1, 0, vec![
          Inst::GetLocal(0), // handle
          Inst::NewFunc(curry2_chunk_id),
          Inst::Return,
        ]);
        insts.push(Inst::NewFunc(curry1_chunk_id));
      },
      ExprF::Var(name) => {
        let offset = env[name];
        insts.push(Inst::GetLocal(offset));
      },
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

  pub fn codegen_literal(
    &mut self,
    lit: &Literal<'str>,
    insts: &mut Vec<Inst>,
  ) {
    match *lit {
      Literal::Bool(value) =>
        insts.push(Inst::NewI32(value as i32)),
      Literal::Str(value) => {
        self.strs.push(value);
        let id = StrID(self.strs.len() - 1);
        insts.push(Inst::NewStr(id));
      },
      _ => panic!("codegen_literal: NYI"),
    }
  }
}
