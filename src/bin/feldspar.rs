extern crate feldspar;
extern crate typed_arena;

use feldspar::bytecode::Inst;
use feldspar::check::Check;
use feldspar::check;
use feldspar::codegen::Codegen;
use feldspar::lex::Lexer;
use feldspar::parse;
use feldspar::builtin;
use feldspar::thread::Thread;
use feldspar::value::GC;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{Read, Write};
use std::io;
use std::process;
use typed_arena::Arena;

struct AnyError(String);

impl From<io::Error> for AnyError {
  fn from(other: io::Error) -> Self {
    AnyError(other.description().to_string())
  }
}

impl From<parse::Error> for AnyError {
  fn from(other: parse::Error) -> Self {
    AnyError(other.1.to_string())
  }
}

fn main() {
  unsafe {
    main_();
  }
}

unsafe fn main_() {
  if let Err(err) = main__() {
    let _ = writeln!(io::stderr(), "feldspar: {}", err.0);
    process::exit(1);
  }
}

unsafe fn main__() -> Result<(), AnyError>{
  let args: Vec<_> = env::args().collect();
  if args.len() < 2 {
    usage(&mut io::stderr())?;
    process::exit(1);
  }

  let mut source = String::new();
  File::open(&args[1])?.read_to_string(&mut source)?;

  let expr_arena = Arena::new();
  let mut lexer = Lexer::new(&source);
  let expr = parse::read_expr(&expr_arena, &mut lexer)?;
  println!("{:?}", expr);

  let type_arena = Arena::new();
  let mut check = Check::new(&type_arena);
  let ty = check.infer(&builtin::env(&type_arena), &expr).map_err(|err| {
    AnyError(match err {
      check::Error::Unify(a, b) =>
        "cannot unify type\n  ".to_string() +
        &a.pretty_string(&|t| check.purge(t)) +
        "\nwith type\n  " +
        &b.pretty_string(&|t| check.purge(t)),
      check::Error::Var(name) =>
        "cannot find variable\n  ".to_string() +
        name,
    })
  })?;
  println!("{}", ty.pretty_string(&|t| check.purge(t)));

  let mut codegen = Codegen::new();
  let mut insts = vec![];
  codegen.codegen_expr(&HashMap::new(), &expr, &mut insts);
  insts.push(Inst::New(0, 0));
  insts.push(Inst::Call);
  insts.push(Inst::Return);

  let gc = GC::new();
  let mut thread = Thread::new(
    &gc,
    |id| &codegen.strs[id.0],
    |id| &codegen.chunks[id.0],
    &insts,
    0,
  );

  thread.resume();

  Ok(())
}

fn usage<W>(w: &mut W) -> io::Result<()> where W: Write {
  writeln!(w, "usage: feldspar <src> [arg1 [arg2 [...]]]")
}
