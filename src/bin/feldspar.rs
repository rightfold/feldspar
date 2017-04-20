extern crate feldspar;
extern crate nom;
extern crate typed_arena;

use feldspar::bytecode::Inst;
use feldspar::check::Check;
use feldspar::check;
use feldspar::codegen::Codegen;
use feldspar::parse;
use feldspar::thread::Thread;
use feldspar::value::GC;
use nom::IError;
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

impl From<IError> for AnyError {
  fn from(other: IError) -> Self {
    AnyError(format!("{:?}", other))
  }
}

fn main() {
  if let Err(err) = main_() {
    let _ = writeln!(io::stderr(), "feldspar: {}", err.0);
    process::exit(1);
  }
}

fn main_() -> Result<(), AnyError>{
  let args: Vec<_> = env::args().collect();
  if args.len() < 2 {
    usage(&mut io::stderr())?;
    process::exit(1);
  }

  let mut source = String::new();
  File::open(&args[1])?.read_to_string(&mut source)?;

  let (codegen, insts) = {
    let expr_arena = Arena::new();

    let iresult = parse::expr::level_1(&source, &expr_arena);
    println!("{:?}", iresult);
    let expr = iresult.to_full_result()?;
    println!("{:?}", expr);

    let type_arena = Arena::new();
    let mut check = Check::new(&type_arena);
    let ty = check.infer(&HashMap::new(), &expr).map_err(|err| {
      AnyError(match err {
        check::Error::Unify(a, b) =>
          "cannot unify type\n  ".to_string() +
          &a.pretty_string(&|t| check.purge(t)) +
          "\nwith type\n  " +
          &b.pretty_string(&|t| check.purge(t)),
        check::Error::Var(ref name) =>
          "cannot find variable\n  ".to_string() +
          name,
        check::Error::RankN =>
          "higher-rank types are not yet supported".to_string()
      })
    })?;
    println!("{}", ty.pretty_string(&|t| check.purge(t)));

    let mut codegen = Codegen::new();
    let mut insts = vec![];
    codegen.codegen_expr(&HashMap::new(), &expr, &mut insts);
    insts.push(Inst::NewTuple(0));
    insts.push(Inst::Call);
    insts.push(Inst::Return);

    (codegen, insts)
  };

  let gc = GC::new();
  let mut thread = Thread::new(
    &gc,
    |id| &codegen.str(id),
    |id| &codegen.chunk(id),
    &insts,
    0,
  );

  thread.resume();

  Ok(())
}

fn usage<W>(w: &mut W) -> io::Result<()> where W: Write {
  writeln!(w, "usage: feldspar <src> [arg1 [arg2 [...]]]")
}
