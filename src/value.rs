use bytecode::ChunkID;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub type Root = Rc<Layout>;

#[derive(Debug)]
pub enum Layout {
  I32(i32),
  Bytes(Vec<u8>),
  Str(String),
  Tuple0,
  Tuple1(Weak<Layout>),
  Tuple2(Weak<Layout>, Weak<Layout>),
  TupleN(Vec<Weak<Layout>>),
  Closure(ChunkID, Vec<Weak<Layout>>),
}

impl Layout {
  pub fn i32(&self) -> Option<i32> {
    match self {
      &Layout::I32(value) => Some(value),
      _ => None,
    }
  }

  pub fn bytes(&self) -> Option<&[u8]> {
    match self {
      &Layout::Bytes(ref bytes) => Some(bytes),
      &Layout::Str(ref string) => Some(string.as_bytes()),
      _ => None,
    }
  }

  pub fn str(&self) -> Option<&str> {
    match self {
      &Layout::Str(ref string) => Some(string),
      _ => None,
    }
  }

  pub fn tuple_elem(&self, offset: usize) -> Option<Root> {
    match (offset, self) {
      (_, &Layout::Tuple0)                => None,
      (0, &Layout::Tuple1(ref elem0))     => elem0.upgrade(),
      (_, &Layout::Tuple1(_))             => None,
      (0, &Layout::Tuple2(ref elem0, _))  => elem0.upgrade(),
      (1, &Layout::Tuple2(_, ref elem1))  => elem1.upgrade(),
      (_, &Layout::Tuple2(_, _))          => None,
      (n, &Layout::TupleN(ref elems))     =>
        elems.get(n).and_then(Weak::upgrade),
      _ => None,
    }
  }

  pub fn closure_chunk(&self) -> Option<ChunkID> {
    match self {
      &Layout::Closure(chunk, _) => Some(chunk),
      _ => None,
    }
  }

  pub fn closure_capture(&self, offset: usize) -> Option<Root> {
    match (offset, self) {
      (n, &Layout::Closure(_, ref captures)) =>
        captures.get(n).and_then(Weak::upgrade),
      _ => None,
    }
  }
}

pub struct GC {
  values: RefCell<Vec<Rc<Layout>>>,
}

impl GC {
  pub fn new() -> Self {
    GC{values: RefCell::new(vec![])}
  }

  pub fn alloc(&self, layout: Layout) -> Root {
    let rc = Rc::new(layout);
    self.values.borrow_mut().push(rc.clone());
    rc
  }

  pub fn alloc_i32(&self, value: i32) -> Root {
    self.alloc(Layout::I32(value))
  }

  pub fn alloc_str(&self, str: String) -> Root {
    self.alloc(Layout::Str(str))
  }

  pub fn alloc_tuple(&self, elems: &[Root]) -> Root {
    match elems.len() {
      0 => self.alloc(Layout::Tuple0),
      1 => self.alloc(Layout::Tuple1(Rc::downgrade(&elems[0]))),
      2 => self.alloc(Layout::Tuple2(Rc::downgrade(&elems[0]),
                                     Rc::downgrade(&elems[1]))),
      _ => {
        let weaks = elems.iter().map(Rc::downgrade).collect();
        self.alloc(Layout::TupleN(weaks))
      },
    }
  }

  pub fn alloc_closure(&self, chunk: ChunkID, captures: &[Root]) -> Root {
    let weaks = captures.iter().map(Rc::downgrade).collect();
    self.alloc(Layout::Closure(chunk, weaks))
  }
}
