use libc::c_void;
use std::mem;
use std::slice;

extern {
  fn fs_alloc(ptr_count: usize, aux_count: usize) -> *mut c_void;
  fn fs_dealloc(lay: *mut c_void);
  fn fs_ptr_count(lay: *mut c_void) -> usize;
  fn fs_aux_count(lay: *mut c_void) -> usize;
  fn fs_get_ptr(lay: *mut c_void, offset: usize) -> *mut c_void;
  fn fs_set_ptr(lay: *mut c_void, offset: usize, ptr: *mut c_void) -> *mut c_void;
  fn fs_aux(lay: *mut c_void) -> *mut u8;
}

pub struct Ref<'a> {
  gc: &'a GC,
  lay: *mut c_void,
}

impl<'a> Ref<'a> {
  pub fn get_ptr(&self, offset: usize) -> Option<Ref<'a>> {
    if offset >= self.ptr_count() {
      None
    } else {
      let lay = unsafe { fs_get_ptr(self.lay, offset) };
      if lay.is_null() {
        None
      } else {
        // FIXME: Tell GC to retain root.
        Some(Ref{gc: self.gc, lay: lay})
      }
    }
  }

  pub fn set_ptr(&self, offset: usize, ptr: &Ref<'a>) {
    // FIXME: Check that GCs are the same.
    if offset < self.ptr_count() {
      unsafe { fs_set_ptr(self.lay, offset, ptr.lay) };
    }
  }

  pub fn aux(&self) -> &mut [u8] {
    unsafe {
      slice::from_raw_parts_mut(fs_aux(self.lay), self.aux_count())
    }
  }

  unsafe fn aux_any<T>(&self) -> &mut T {
    let aux = self.aux();
    if aux.len() != mem::size_of::<T>() {
      panic!("Ref::aux_any: invalid aux size");
    }
    mem::transmute::<*const u8, &mut T>(aux.as_ptr())
  }

  pub fn aux_i32(&self) -> &mut i32 {
    unsafe { self.aux_any() }
  }

  pub fn aux_usize(&self) -> &mut usize {
    unsafe { self.aux_any() }
  }

  pub fn ptr_count(&self) -> usize {
    unsafe { fs_ptr_count(self.lay) }
  }

  pub fn aux_count(&self) -> usize {
    unsafe { fs_aux_count(self.lay) }
  }
}

impl<'a> Clone for Ref<'a> {
  fn clone(&self) -> Self {
    // FIXME: Tell GC to retain root.
    Ref{gc: self.gc, lay: self.lay}
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

  pub fn alloc<'a>(&'a self, ptr_count: usize, aux_count: usize) -> Ref<'a> {
    unsafe {
      // FIXME: Tell GC to retain root.
      let lay = fs_alloc(ptr_count, aux_count);
      Ref{gc: self, lay: lay}
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_alloc() {
    let gc = GC::new();
    for ptr_count in 0 .. 10 {
      for aux_count in 0 .. 10 {
        let r = gc.alloc(ptr_count, aux_count);
        assert_eq!(r.ptr_count(), ptr_count);
        assert_eq!(r.aux_count(), aux_count);
        for offset in 0 .. 10 {
          assert!(r.get_ptr(offset).is_none());
        }
      }
    }
  }
}
