#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;
extern crate alloc;

pub use self::{dropless::DroplessArena, typed::TypedArena};
use alloc::boxed::Box;
use core::{ptr, slice};
use smallvec::SmallVec;

mod dropless;
mod typed;

pub trait Arena<T>: Default + private::Sealed<T> {
  #[allow(clippy::mut_from_ref)]
  fn alloc(&self, object: T) -> &mut T;

  #[allow(clippy::mut_from_ref)]
  fn alloc_iter<I: IntoIterator<Item = T>>(&self, iterable: I) -> &mut [T] {
    unsafe {
      let buffer =
        Box::leak(SmallVec::<[_; 8]>::from_iter(iterable).into_boxed_slice());
      let ptr = self.alloc_uninit_slice(buffer.len());
      ptr::copy(buffer as *mut [T] as *mut T, ptr, buffer.len());
      &mut *slice::from_raw_parts_mut(ptr, buffer.len())
    }
  }
}

mod private {
  pub trait Sealed<T> {
    /// # Safety
    /// The memory must be initialized before the arena is dropped.
    #[allow(clippy::mut_from_ref)]
    unsafe fn alloc_uninit_slice(&self, len: usize) -> *mut T;
  }
}

// The arenas start with PAGE-sized chunks, and then each new chunk is twice as
// big as its predecessor, up until we reach HUGE_PAGE-sized chunks, whereupon
// we stop growing. This scales well, from arenas that are barely used up to
// arenas that are used for 100s of MiBs. Note also that the chosen sizes match
// the usual sizes of pages and huge pages on Linux.
const PAGE: usize = 4096;
const HUGE_PAGE: usize = 2 * 1024 * 1024;

#[macro_export]
macro_rules! declare_arena {
    (
      $(#[$attr:meta])*
      $visibility: vis struct $arena:ident<'a> {
        $($alias:ident: $typed:ty),*,
      }
      $($anon:ty)*
  ) => {
    $(#[$attr])*
    #[derive(Default)]
    $visibility struct $arena<'a> {
      $($alias: $crate::TypedArena<$typed>),*,
      __anon: $crate::DroplessArena,
    }

    #[allow(clippy::mut_from_ref)]
    const _: () = {
      use $crate::Arena as _;

      impl<'a> $arena<'a> {
        fn alloc<T: Dispatch<'a>>(&self, object: T) -> &mut T {
          object.dispatch_one(self)
        }

        fn alloc_iter<I>(&self, iter: I) -> &mut [I::Item]
        where
          I: IntoIterator,
          <I as IntoIterator>::Item: Dispatch<'a>
        {
          <<I as IntoIterator>::Item as Dispatch<'a>>::dispatch_iter(iter, self)
        }
      }

      $visibility trait Dispatch<'a>: Sized {
        fn dispatch_one<'b>(self, arena: &'b $arena<'a>) -> &'b mut Self;

        fn dispatch_iter<'b, I>(iter: I, arena: &'b $arena<'a>) -> &'b mut[Self]
        where
          I: IntoIterator<Item = Self>;
      }

      $(
        #[automatically_derived]
        impl<'a> Dispatch<'a> for $typed {
          fn dispatch_one<'b>(self, arena: &'b $arena<'a>) -> &'b mut Self {
            arena.$alias.alloc(self)
          }

          fn dispatch_iter<'b, I>(iter: I, arena: &'b $arena<'a>) -> &'b mut[Self]
            where
              I: IntoIterator<Item = Self>,
            {
              arena.$alias.alloc_iter(iter)
            }
          }
        )*

        $(
          #[automatically_derived]
          impl<'a> Dispatch<'a> for $anon {
            fn dispatch_one<'b>(self, arena: &'b $arena<'a>) -> &'b mut Self {
              arena.__anon.alloc(self)
            }

            fn dispatch_iter<'b, I>(iter: I, arena: &'b $arena<'a>) -> &'b mut[Self]
            where
              I: IntoIterator<Item = Self>,
            {
              arena.__anon.alloc_iter(iter)
            }
          }
        )*
      };
    };
}

#[cfg(test)]
//noinspection DuplicatedCode
mod tests {
  use super::*;
  use alloc::vec::Vec;
  use core::cell::Cell;

  declare_arena! {
    struct Arena<'a> {
      d: DropCounter<'a>,
    }
    i128
  }

  #[test]
  fn test_macro_drop() {
    let drop_count = Cell::new(0);
    {
      let arena = Arena::default();
      for _ in 0..100 {
        arena.alloc(DropCounter(&drop_count));
      }
    }
    assert_eq!(drop_count.get(), 100);
  }

  #[test]
  fn test_macro_drop_iter() {
    let drop_count = Cell::new(0);
    {
      let arena = Arena::default();
      let mut vec = Vec::with_capacity(100);
      for _ in 0..100 {
        vec.push(DropCounter(&drop_count));
      }
      arena.alloc_iter(vec);
    }
    assert_eq!(drop_count.get(), 100);
  }

  struct DropCounter<'a>(&'a Cell<u32>);

  impl Drop for DropCounter<'_> {
    fn drop(&mut self) {
      self.0.set(self.0.get() + 1);
    }
  }
}
