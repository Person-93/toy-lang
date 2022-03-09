use super::TypedArena;
use core::{
  iter::FusedIterator,
  mem,
  ptr::{self, NonNull},
};

pub struct ArenaIter<'a, T> {
  arena: &'a TypedArena<T>,
  chunk_idx: usize,
  /// for ZSTs this field is repurposed to hold the current **index**
  current: *const T,
  /// for ZSTs this field is repurposed to hold the count
  chunk_end: *const T,
}

impl<T> ArenaIter<'_, T> {
  pub(super) fn new(arena: &TypedArena<T>) -> ArenaIter<'_, T> {
    if mem::size_of::<T>() == 0 {
      ArenaIter {
        arena,
        chunk_idx: 0,
        current: 0 as _,
        chunk_end: arena.ptr.get(),
      }
    } else {
      let (current, end) = match arena.chunks.borrow_mut().first_mut() {
        Some(chunk) => (chunk.start(), chunk.end()),
        None => (ptr::null_mut(), ptr::null_mut()),
      };
      ArenaIter {
        arena,
        chunk_idx: 0,
        current,
        chunk_end: end,
      }
    }
  }

  #[must_use]
  fn next_chunk(&mut self) -> bool {
    self.chunk_idx += 1;
    let mut chunks = self.arena.chunks.borrow_mut();
    let len = chunks.len();
    match chunks.get_mut(self.chunk_idx) {
      Some(next_chunk) => {
        unsafe {
          self.current = next_chunk.start();
          self.chunk_end = if self.chunk_idx == len - 1 {
            self.arena.ptr.get()
          } else {
            next_chunk.start().add(next_chunk.entries)
          };
        }
        true
      }
      None => false,
    }
  }
}

impl<'a, T> Iterator for ArenaIter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    unsafe {
      if mem::size_of::<T>() == 0 {
        let current_idx = self.current as usize;
        let count = self.chunk_end as usize;
        (current_idx < count).then(|| NonNull::dangling().as_ref())
      } else if self.current.is_null() {
        None
      } else {
        let current = self.current;
        self.current = self.current.add(1);
        if self.current == self.chunk_end {
          if self.next_chunk() {
            let current = self.current;
            self.current = self.current.add(1);
            Some(&*current)
          } else {
            None
          }
        } else {
          Some(&*current)
        }
      }
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    unsafe {
      if mem::size_of::<T>() == 0 {
        let current = self.current as usize;
        let count = self.chunk_end as usize;
        let remaining = count - current;
        (remaining, Some(remaining))
      } else {
        (self.chunk_end.offset_from(self.current) as usize, None)
      }
    }
  }

  fn count(self) -> usize
  where
    Self: Sized,
  {
    unsafe {
      if mem::size_of::<T>() == 0 {
        let current = self.current as usize;
        let total = self.chunk_end as usize;
        total - current
      } else {
        self
          .arena
          .chunks
          .borrow()
          .iter()
          .skip(self.chunk_idx + 1)
          .fold(
            self.chunk_end.offset_from(self.current) as usize,
            |accum, chunk| accum + chunk.entries,
          )
      }
    }
  }

  fn last(self) -> Option<Self::Item>
  where
    Self: Sized,
  {
    if self.current.is_null() {
      None
    } else {
      Some(unsafe { &*self.arena.ptr.get().offset(-1) })
    }
  }

  fn nth(&mut self, n: usize) -> Option<Self::Item> {
    unsafe {
      if mem::size_of::<T>() == 0 {
        self.current = (self.current as usize + n) as _;
        let current = self.current as usize;
        let total = self.chunk_end as usize;
        (current < total).then(|| NonNull::dangling().as_ref())
      } else if self.current.is_null() {
        None
      } else {
        self.current = self.current.add(n);
        let mut past_end = self.current.offset_from(self.chunk_end);
        while past_end >= 0 {
          if !self.next_chunk() {
            return None;
          }
          self.current = self.current.add(past_end as usize);
          past_end = self.current.offset_from(self.chunk_end);
        }
        let current = self.current;
        self.current = self.current.add(1);
        Some(&*(current))
      }
    }
  }
}

impl<T> FusedIterator for ArenaIter<'_, T> {}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::Arena;

  #[test]
  fn test_next() {
    let arena = TypedArena::new();
    arena.alloc(10);
    arena.alloc(20);
    arena.alloc(30);
    let mut iter = arena.iter().copied();
    assert_eq!(10, iter.next().unwrap());
    assert_eq!(20, iter.next().unwrap());
    assert_eq!(30, iter.next().unwrap());
  }

  #[test]
  fn test_count() {
    let arena = TypedArena::new();
    for _ in 0..2000 {
      arena.alloc(0);
    }
    assert_eq!(arena.iter().count(), 2000);
  }

  #[test]
  fn test_count_zst() {
    let arena = TypedArena::new();
    for _ in 0..5000 {
      arena.alloc(());
    }
    assert_eq!(arena.iter().count(), 5000);
  }

  #[test]
  fn test_nth() {
    let arena = TypedArena::new();
    for i in 0..2000 {
      arena.alloc(i);
    }
    let mut iter = arena.iter().copied();
    assert_eq!(iter.nth(420).unwrap(), 420);
    assert_eq!(iter.next().unwrap(), 421);
  }
}
