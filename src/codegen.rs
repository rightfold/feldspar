use bytecode::{Chunk, Inst};
use std::collections::HashMap;
use syntax::{Expr, ExprF, Literal};

pub struct Codegen {
  pub chunks: Vec<Chunk>,
}

impl Codegen {
  pub fn new() -> Self {
    Codegen{
      chunks: vec![],
    }
  }

  pub fn codegen_func<T>(
    &mut self,
    env: &HashMap<&str, usize>,
    captures: usize,
    body: &Expr<T>,
  ) -> usize {
    let mut insts = vec![];
    self.codegen_expr(env, body, &mut insts);
    self.chunks.push(Chunk{
      insts: insts,
      locals: 1 + captures,
      captures: captures,
    });
    self.chunks.len() - 1
  }

  pub fn codegen_expr<T>(
    &mut self,
    env: &HashMap<&str, usize>,
    expr: &Expr<T>,
    insts: &mut Vec<Inst>,
  ) {
    match expr.1 {
      ExprF::Lit(ref lit) =>
        self.codegen_literal(lit, insts),
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
    }
  }

  pub fn codegen_literal(&self, lit: &Literal, insts: &mut Vec<Inst>) {
    match *lit {
      Literal::Bool(value) =>
        insts.push(Inst::NewBool(value)),
      _ => panic!("codegen_literal: NYI"),
    }
  }
}
