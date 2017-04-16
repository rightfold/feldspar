use bytecode::{Chunk, Inst};
use std::collections::HashMap;
use syntax::{Expr, ExprF, Literal};

pub struct Codegen<'str> {
  pub chunks: Vec<Chunk<'str>>,
}

impl<'str> Codegen<'str> {
  pub fn new() -> Self {
    Codegen{
      chunks: vec![],
    }
  }

  pub fn codegen_func<'expr, T>(
    &mut self,
    env: &HashMap<&str, usize>,
    captures: usize,
    body: &Expr<'str, 'expr, T>,
  ) -> usize {
    let mut insts = vec![];
    self.codegen_expr(env, body, &mut insts);
    insts.push(Inst::Return);
    self.chunks.push(Chunk{
      insts: insts,
      locals: 1 + captures,
      captures: captures,
    });
    self.chunks.len() - 1
  }

  pub fn codegen_expr<'expr, T>(
    &mut self,
    env: &HashMap<&str, usize>,
    expr: &Expr<'str, 'expr, T>,
    insts: &mut Vec<Inst<'str>>,
  ) {
    match expr.1 {
      ExprF::Lit(ref lit) =>
        self.codegen_literal(lit, insts),
      ExprF::Var("stdout#") =>
        insts.push(Inst::NewI32(1)),
      ExprF::Var("to_utf8#") =>
        insts.push(Inst::NewI32(1)),
      ExprF::Var("write#") =>
        insts.push(Inst::NewI32(1)),
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

  pub fn codegen_literal(
    &self,
    lit: &Literal<'str>,
    insts: &mut Vec<Inst<'str>>,
  ) {
    match *lit {
      Literal::Bool(value) =>
        insts.push(Inst::NewI32(value as i32)),
      Literal::Str(value) =>
        insts.push(Inst::NewStr(value)),
      _ => panic!("codegen_literal: NYI"),
    }
  }
}
