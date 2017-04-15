extern crate feldspar;
extern crate typed_arena;

use feldspar::bytecode::Inst;
use feldspar::check::Check;
use feldspar::check;
use feldspar::codegen::Codegen;
use feldspar::lex::Lexer;
use feldspar::parse;
use feldspar::thread::Thread;
use feldspar::value::GC;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::io;
use std::process;
use typed_arena::Arena;

#[derive(Debug)]
enum Error {
  CheckError(check::Error),
  IOError(io::Error),
  ParseError(parse::Error),
}

impl From<check::Error> for Error {
  fn from(other: check::Error) -> Self {
    Error::CheckError(other)
  }
}

impl From<io::Error> for Error {
  fn from(other: io::Error) -> Self {
    Error::IOError(other)
  }
}

impl From<parse::Error> for Error {
  fn from(other: parse::Error) -> Self {
    Error::ParseError(other)
  }
}

fn main() {
  if let Err(err) = main_() {
    let _ = writeln!(io::stderr(), "feldspar: {:?}", err);
    process::exit(1);
  }
}

fn main_() -> Result<(), Error> {
  let args: Vec<_> = env::args().collect();
  if args.len() < 2 {
    try!(usage(&mut io::stderr()));
    process::exit(1);
  }

  let mut source = String::new();
  try!(try!(File::open(&args[1])).read_to_string(&mut source));

  let expr_arena = Arena::new();
  let mut lexer = Lexer::new(&source);
  let expr = try!(parse::read_expr(&expr_arena, &mut lexer));

  let type_arena = Arena::new();
  let mut check = Check::new(&type_arena);
  let ty = try!(check.infer(&HashMap::new(), &expr));
  let mut pretty = String::new();
  ty.pretty(&|t| check.purge(t), &mut pretty).unwrap();
  println!("{}", pretty);

  let mut codegen = Codegen::new();
  let mut insts = vec![];
  codegen.codegen_expr(&HashMap::new(), &expr, &mut insts);
  insts.push(Inst::Return);

  let gc = GC::new();
  let mut thread = Thread::new(
    &gc,
    |id| &codegen.chunks[id],
    &insts,
    0,
  );

  thread.resume();

  Ok(())
}

fn usage<W>(w: &mut W) -> io::Result<()> where W: Write {
  writeln!(w, "usage: feldspar <src> [arg1 [arg2 [...]]]")
}
