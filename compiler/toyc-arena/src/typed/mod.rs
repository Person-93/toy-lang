mod iter;

use crate::{private::Sealed, Arena, HUGE_PAGE, PAGE};
use alloc::{alloc::alloc, alloc::Layout, boxed::Box, vec::Vec};
use core::{
  cell::{Cell, RefCell},
  cmp,
  marker::PhantomData,
  mem, ptr,
  ptr::NonNull,
  slice,
};

pub struct TypedArena<T> {
  /// A pointer to the next object to be allocated.
  /// If `T` is a ZST it is just the count of "allocated" items.
  ptr: Cell<*mut T>,
  /// A pointer to the end of the current chunk's allocated space.
  end: Cell<*mut T>,
  /// A vector of all chunks currently allocated.
  chunks: RefCell<Vec<ArenaChunk<T>>>,
}

impl<T> Arena<T> for TypedArena<T> {
  #[inline]
  fn alloc(&self, object: T) -> &mut T {
    if mem::size_of::<T>() == 0 {
      return unsafe {
        self
          .ptr
          .set((self.ptr.get() as *mut u8).offset(1) as *mut T);
        mem::forget(object);
        NonNull::dangling().as_mut()
      };
    }

    if self.ptr == self.end {
      self.grow(1);
    }

    unsafe {
      let ptr = self.ptr.get();
      self.ptr.set(ptr.offset(1));
      ptr::write(ptr, object);
      &mut *ptr
    }
  }
}

impl<T> Sealed<T> for TypedArena<T> {
  //noinspection DuplicatedCode
  unsafe fn alloc_uninit_slice(&self, len: usize) -> *mut T {
    if mem::size_of::<T>() == 0 {
      self.ptr.set((self.ptr.get() as usize + len) as _);
      NonNull::dangling().as_ptr()
    } else {
      let ptr = self.ptr.get();
      if (self.end.get().offset_from(ptr) as usize) < len {
        self.grow(len);
        self.ptr.replace(self.ptr.get().add(len))
      } else {
        self.ptr.set(ptr.add(len));
        ptr
      }
    }
  }
}

impl<T> TypedArena<T> {
  pub const fn new() -> TypedArena<T> {
    TypedArena {
      ptr: Cell::new(ptr::null_mut()),
      end: Cell::new(ptr::null_mut()),
      chunks: RefCell::new(Vec::new()),
    }
  }

  pub fn iter(&self) -> iter::ArenaIter<T> {
    unsafe {
      if let Some(last_chunk) = self.chunks.borrow_mut().last_mut() {
        // set the entries count in the last chunk to make iteration simpler
        last_chunk.entries =
          self.ptr.get().offset_from(last_chunk.start()) as usize;
      }
      iter::ArenaIter::new(self)
    }
  }

  #[cold]
  #[inline(never)]
  fn grow(&self, additional: usize) {
    assert_ne!(mem::size_of::<T>(), 0);
    unsafe {
      let obj_size = mem::size_of::<T>();
      let mut chunks = self.chunks.borrow_mut();

      let mut new_chunk_size;

      if let Some(last_chunk) = chunks.last_mut() {
        // finalized info of last chunk
        last_chunk.entries =
          self.ptr.get().offset_from(last_chunk.start()) as usize;

        new_chunk_size =
          cmp::min(HUGE_PAGE / obj_size, last_chunk.storage.len() * 2);
      } else {
        new_chunk_size = PAGE / obj_size;
      }
      new_chunk_size = cmp::max(additional, new_chunk_size);

      let mut chunk = ArenaChunk::new(new_chunk_size);
      self.ptr.set(chunk.start());
      self.end.set(chunk.end());
      chunks.push(chunk);
    }
  }
}

impl<T> Default for TypedArena<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T> Drop for TypedArena<T> {
  fn drop(&mut self) {
    unsafe {
      if mem::size_of::<T>() == 0 {
        for _ in 0..self.ptr.get() as usize {
          NonNull::<T>::dangling().as_ptr().drop_in_place();
        }
      } else {
        let mut chunks = self.chunks.borrow_mut();

        // the entries count hasn't been set for the last chunk yet because
        // we only update the old one when we move on to the next one
        if let Some(last_chunk) = chunks.last_mut() {
          last_chunk.entries =
            self.ptr.get().offset_from(last_chunk.start()) as usize;
        }

        for chunk in chunks.iter_mut() {
          chunk.destroy();
        }
      }
    }
  }
}

struct ArenaChunk<T> {
  storage: Box<[u8]>,
  entries: usize,
  _marker: PhantomData<T>,
}

impl<T> ArenaChunk<T> {
  #[inline]
  unsafe fn new(capacity: usize) -> ArenaChunk<T> {
    assert_ne!(mem::size_of::<T>(), 0);
    ArenaChunk {
      storage: Box::from_raw(slice::from_raw_parts_mut(
        alloc(
          Layout::from_size_align(
            mem::size_of::<T>() * capacity,
            mem::align_of::<T>(),
          )
          .unwrap(),
        ),
        capacity,
      )),
      entries: 0,
      _marker: PhantomData,
    }
  }

  #[inline]
  unsafe fn destroy(&mut self) {
    if mem::needs_drop::<T>() {
      ptr::drop_in_place(ptr::slice_from_raw_parts_mut(
        self.start(),
        self.entries,
      ))
    }
  }

  /// Returns a pointer to the first allocated object
  #[inline]
  fn start(&mut self) -> *mut T {
    self.storage.as_mut_ptr() as *mut T
  }

  /// Returns a pointer to the end of the allocated space
  #[inline]
  fn end(&mut self) -> *mut T {
    unsafe { self.start().add(self.storage.len()) }
  }
}

#[cfg(test)]
//noinspection DuplicatedCode
mod tests {
  use super::*;
  use alloc::{string::String, vec::Vec};

  #[test]
  fn test_returns_ref_to_param() {
    let arena = TypedArena::default();
    let output = arena.alloc(String::from("hello world"));
    assert_eq!("hello world", output);
  }

  #[test]
  fn test_returns_slice() {
    let arena = TypedArena::default();
    let slice = arena.alloc_iter([1, 2, 3, 4, 5]);
    assert_eq!(slice.len(), 5);
    assert_eq!(slice, &[1, 2, 3, 4, 5]);
  }

  #[test]
  fn test_unused_no_alloc() {
    let arena: TypedArena<u128> = TypedArena::default();
    assert!(arena.chunks.borrow().is_empty());
  }

  #[test]
  fn test_zero_sized_no_alloc() {
    let arena = TypedArena::default();
    for _ in 0..100000 {
      arena.alloc(());
    }
    assert!(arena.chunks.borrow().is_empty());
  }

  #[test]
  fn test_drop_count() {
    let counter = Cell::new(0);
    {
      let arena = TypedArena::default();
      for _ in 0..100 {
        arena.alloc(DropCounter { counter: &counter });
      }
      assert_eq!(counter.get(), 0);
    }
    assert_eq!(counter.get(), 100);
  }

  #[test]
  fn test_drop_iter() {
    let counter = Cell::new(0);
    {
      let arena = TypedArena::default();
      let mut vec = Vec::with_capacity(100);
      for _ in 0..100 {
        vec.push(DropCounter { counter: &counter });
      }
      arena.alloc_iter(vec);
      assert_eq!(counter.get(), 0);
    }
    assert_eq!(counter.get(), 100);
  }

  struct DropCounter<'a> {
    counter: &'a Cell<u32>,
  }

  impl Drop for DropCounter<'_> {
    fn drop(&mut self) {
      self.counter.set(self.counter.get() + 1);
    }
  }

  #[test]
  fn test_drop_zst() {
    DROP_COUNTER.with(|counter| counter.set(0));
    {
      let arena = TypedArena::default();
      for _ in 0..100 {
        arena.alloc(DroppableZst);
      }
      assert_eq!(DROP_COUNTER.with(|counter| counter.get()), 0);
    }
    assert_eq!(DROP_COUNTER.with(|counter| counter.get()), 100);
  }

  #[test]
  fn test_drop_zst_iter() {
    let counter = Cell::new(0);
    {
      let arena = TypedArena::default();
      let mut vec = Vec::with_capacity(100);
      for _ in 0..100 {
        vec.push(DropCounter { counter: &counter });
      }
      arena.alloc_iter(vec);
      assert_eq!(counter.get(), 0);
    }
    assert_eq!(counter.get(), 100);
  }

  struct DroppableZst;

  impl Drop for DroppableZst {
    fn drop(&mut self) {
      DROP_COUNTER.with(|counter| counter.set(counter.get() + 1));
    }
  }

  thread_local! {
    static DROP_COUNTER: Cell<u32> = Cell::new(0);
  }
}
