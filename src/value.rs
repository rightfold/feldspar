use bytecode::ChunkID;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Root<'a> {
  gc: PhantomData<&'a ()>,
  layout: Rc<Layout>,
}

#[derive(Clone, Debug)]
struct Weak {
  layout: rc::Weak<Layout>,
}

#[derive(Debug)]
enum Layout {
  I32(i32),
  Bytes(Vec<u8>),
  Str(String),
  Tuple0,
  Tuple1(Weak),
  Tuple2(Weak, Weak),
  TupleN(Vec<Weak>),
  Closure(ChunkID, Vec<Weak>),
}

impl<'a> Root<'a> {
  fn upgrade(weak: &Weak) -> Option<Self> {
    weak.layout.upgrade().map(|rc| Root{gc: PhantomData, layout: rc})
  }

  fn downgrade(&self) -> Weak {
    Weak{layout: Rc::downgrade(&self.layout)}
  }

  pub fn i32(&self) -> Option<i32> {
    match self.layout.as_ref() {
      &Layout::I32(value) => Some(value),
      _ => None,
    }
  }

  pub fn bytes(&self) -> Option<&[u8]> {
    match self.layout.as_ref() {
      &Layout::Bytes(ref bytes) => Some(bytes),
      &Layout::Str(ref string) => Some(string.as_bytes()),
      _ => None,
    }
  }

  pub fn str(&self) -> Option<&str> {
    match self.layout.as_ref() {
      &Layout::Str(ref string) => Some(string),
      _ => None,
    }
  }

  pub fn tuple_elem(&self, offset: usize) -> Option<Root<'a>> {
    match (offset, self.layout.as_ref()) {
      (_, &Layout::Tuple0)                => None,
      (0, &Layout::Tuple1(ref elem0))     => Self::upgrade(elem0),
      (_, &Layout::Tuple1(_))             => None,
      (0, &Layout::Tuple2(ref elem0, _))  => Self::upgrade(elem0),
      (1, &Layout::Tuple2(_, ref elem1))  => Self::upgrade(elem1),
      (_, &Layout::Tuple2(_, _))          => None,
      (n, &Layout::TupleN(ref elems))     =>
        elems.get(n).and_then(Self::upgrade),
      _ => None,
    }
  }

  pub fn closure_chunk(&self) -> Option<ChunkID> {
    match self.layout.as_ref() {
      &Layout::Closure(chunk, _) => Some(chunk),
      _ => None,
    }
  }

  pub fn closure_capture(&self, offset: usize) -> Option<Root<'a>> {
    match (offset, self.layout.as_ref()) {
      (n, &Layout::Closure(_, ref captures)) =>
        captures.get(n).and_then(Self::upgrade),
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

  fn alloc<'a>(&'a self, layout: Layout) -> Root<'a> {
    let rc = Rc::new(layout);
    self.values.borrow_mut().push(rc.clone());
    Root{gc: PhantomData, layout: rc}
  }

  pub fn alloc_i32<'a>(&'a self, value: i32) -> Root<'a> {
    self.alloc(Layout::I32(value))
  }

  pub fn alloc_str<'a>(&'a self, str: String) -> Root<'a> {
    self.alloc(Layout::Str(str))
  }

  pub fn alloc_tuple<'a>(&'a self, elems: &[Root<'a>]) -> Root<'a> {
    match elems.len() {
      0 => self.alloc(Layout::Tuple0),
      1 => self.alloc(Layout::Tuple1(Root::downgrade(&elems[0]))),
      2 => self.alloc(Layout::Tuple2(Root::downgrade(&elems[0]),
                                     Root::downgrade(&elems[1]))),
      _ => {
        let weaks = elems.iter().map(Root::downgrade).collect();
        self.alloc(Layout::TupleN(weaks))
      },
    }
  }

  pub fn alloc_closure<'a>(&'a self, chunk: ChunkID, captures: &[Root<'a>])
    -> Root<'a> {
    let weaks = captures.iter().map(Root::downgrade).collect();
    self.alloc(Layout::Closure(chunk, weaks))
  }
}
