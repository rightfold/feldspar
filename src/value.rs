use libc::{calloc, c_void, memcpy};
use std::mem::{size_of, transmute};
use std::ptr;

fn ptr_size() -> usize {
  size_of::<*mut c_void>()
}

pub struct Ref<'a>(&'a GC, *mut c_void);

impl<'a> Ref<'a> {
  pub fn pointer(&self, offset: u16) -> Option<Ref<'a>> {
    if offset >= self.pointer_count() {
      None
    } else {
      let mut result = ptr::null_mut();
      unsafe {
        memcpy(
          transmute(&mut result),
          self.1.offset((4 + offset as usize * ptr_size()) as isize),
          ptr_size(),
        );
      }
      if result.is_null() {
        None
      } else {
        // FIXME: Tell GC to retain root.
        Some(Ref(self.0, result))
      }
    }
  }

  pub fn pointer_count(&self) -> u16 {
    let mut result = 0u16;
    unsafe { memcpy(transmute(&mut result), self.1.offset(0), 2) };
    result
  }

  pub fn auxiliary_count(&self) -> u16 {
    let mut result = 0u16;
    unsafe { memcpy(transmute(&mut result), self.1.offset(2), 2) };
    result
  }
}

impl<'a> Clone for Ref<'a> {
  fn clone(&self) -> Self {
    // FIXME: Tell GC to retain root.
    Ref(self.0, self.1)
  }
}

impl<'a> Drop for Ref<'a> {
  fn drop(&mut self) {
    // FIXME: Tell GC to release root.
  }
}

pub struct GC {
}

impl GC {
  pub fn new() -> Self {
    GC{}
  }

  pub fn alloc<'a>(&'a self, np: u16, nb: u16) -> Ref<'a> {
    unsafe {
      // FIXME: Tell GC to retain root.
      let size = 4 + ptr_size() * np as usize + nb as usize;
      let data = calloc(1, size);
      memcpy(data.offset(0), transmute(&np), 2);
      memcpy(data.offset(2), transmute(&nb), 2);
      Ref(self, data)
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_alloc() {
    let gc = GC::new();
    for pointer_count in 0 .. 10 {
      for auxiliary_count in 0 .. 10 {
        let r = gc.alloc(pointer_count, auxiliary_count);
        assert_eq!(r.pointer_count(),   pointer_count);
        assert_eq!(r.auxiliary_count(), auxiliary_count);
        for offset in 0 .. 10 {
          assert!(r.pointer(offset).is_none());
        }
      }
    }
  }
}
