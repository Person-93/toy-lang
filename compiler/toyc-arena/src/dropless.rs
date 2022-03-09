use crate::{impl_for_base, ArenaBase, HUGE_PAGE, PAGE};
use alloc::{
  alloc::{alloc, Layout},
  boxed::Box,
  vec::Vec,
};
use core::{
  cell::{Cell, RefCell},
  cmp, mem, ptr,
  ptr::NonNull,
  slice,
};

pub struct DroplessArena {
  /// A pointer to the next byte to allocate
  ptr: Cell<*mut u8>,
  /// A pointer to the end of the current page
  end: Cell<*mut u8>,
  /// The size the current page
  page_size: Cell<usize>,
  /// A vector of byte slices to hold the actual data
  pages: RefCell<Vec<Box<[u8]>>>,
}

impl_for_base!(DroplessArena);
impl<'arena, T: 'arena> ArenaBase<'arena, T> for DroplessArena {
  #[inline]
  fn alloc(&self, object: T) -> &'arena mut T {
    assert!(!mem::needs_drop::<T>());

    unsafe {
      if mem::size_of::<T>() == 0 {
        NonNull::dangling().as_mut()
      } else {
        let ptr = if self.ptr.get().is_null() {
          self.grow(mem::size_of::<T>());
          self.align::<T>()
        } else {
          let ptr = self.align::<T>().add(1);

          if ptr as usize >= self.end.get() as usize {
            self.grow(mem::size_of::<T>());
            self.align::<T>()
          } else {
            ptr
          }
        };

        ptr.write(object);
        self.ptr.set(ptr.add(1) as _);
        &mut *ptr
      }
    }
  }

  //noinspection DuplicatedCode
  unsafe fn alloc_uninit_slice(&self, len: usize) -> *mut T {
    if mem::size_of::<T>() == 0 {
      return NonNull::dangling().as_mut();
    }

    if self.end.get().is_null() {
      self.grow(mem::size_of::<T>() * len);
      return self.align::<T>();
    }

    let ptr = self.align::<T>();
    if ((self.end.get() as *mut T).offset_from(ptr) as usize) < len {
      self.grow(len);
      self.align::<T>()
    } else {
      self.ptr.set(ptr.add(len) as _);
      ptr
    }
  }
}

impl DroplessArena {
  pub const fn new() -> DroplessArena {
    DroplessArena {
      ptr: Cell::new(ptr::null_mut()),
      end: Cell::new(ptr::null_mut()),
      page_size: Cell::new(0),
      pages: RefCell::new(Vec::new()),
    }
  }

  #[inline]
  fn align<T>(&self) -> *mut T {
    assert_ne!(mem::size_of::<T>(), 0);

    let ptr = self.ptr.get();
    let alignment = mem::align_of::<T>();
    let distance = ptr.align_offset(alignment);
    let ptr = ptr.wrapping_add(distance);
    assert!(ptr as usize >= self.ptr.get() as usize);
    self.ptr.set(ptr);
    ptr as _
  }

  #[cold]
  #[inline(never)]
  fn grow(&self, additional: usize) {
    unsafe {
      let new_page_size =
        cmp::max(additional, cmp::min(self.page_size.get() * 2, HUGE_PAGE));
      let new_page_size = cmp::max(new_page_size, PAGE);
      self.page_size.set(new_page_size);
      let mut pages = self.pages.borrow_mut();
      pages.push(Box::from_raw(slice::from_raw_parts_mut(
        alloc(Layout::from_size_align(new_page_size, 1).unwrap()),
        new_page_size,
      )));
      let last = pages.last_mut().unwrap();
      let ptr = last.as_mut().as_mut_ptr();
      self.ptr.set(ptr);
      self.end.set(ptr.add(last.len()));
    }
  }
}

impl Default for DroplessArena {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::DroplessArena;
  use crate::Arena;

  #[test]
  fn test_returns_ref_to_param() {
    let arena = DroplessArena::default();
    let input = 42;
    let output = arena.alloc(input);
    assert_eq!(input, *output);
  }

  #[test]
  fn test_returns_slice() {
    let arena = DroplessArena::default();
    let slice = arena.alloc_iter([1, 2, 3, 4, 5]);
    assert_eq!(slice.len(), 5);
    assert_eq!(slice, &[1, 2, 3, 4, 5]);
  }

  #[test]
  fn test_unused_no_alloc() {
    let arena = DroplessArena::default();
    assert!(arena.pages.borrow().is_empty());
  }

  #[test]
  fn test_zero_sized_no_alloc() {
    let arena = DroplessArena::default();
    for _ in 0..5000 {
      arena.alloc(());
    }
    assert!(arena.pages.borrow().is_empty());
  }
}
