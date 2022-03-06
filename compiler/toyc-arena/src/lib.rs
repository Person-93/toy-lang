#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;
extern crate alloc;

pub use self::{dropless::DroplessArena, typed::TypedArena};

mod dropless;
mod typed;

pub trait Arena<'arena, T> {
  #[allow(clippy::mut_from_ref)]
  fn alloc(&self, object: T) -> &'arena mut T;

  #[allow(clippy::mut_from_ref)]
  fn alloc_iter<I>(&self, iterable: I) -> &'arena mut [T]
  where
    I: IntoIterator<Item = T>,
    <I as IntoIterator>::Item: 'arena;
}

trait ArenaBase<'arena, T: 'arena>: Default {
  #[allow(clippy::mut_from_ref)]
  fn alloc(&self, object: T) -> &'arena mut T;

  /// # Safety
  /// The memory must be initialized before the arena is dropped.
  #[allow(clippy::mut_from_ref)]
  unsafe fn alloc_uninit_slice(&self, len: usize) -> *mut T;
}

macro_rules! impl_for_base {
  ($ty:ty) => {
    const _: () = {
      use crate::{Arena, ArenaBase};
      use ::smallvec::SmallVec;

      impl<'arena, T: 'arena> Arena<'arena, T> for $ty {
        fn alloc(&self, object: T) -> &'arena mut T {
          ArenaBase::alloc(self, object)
        }

        fn alloc_iter<I>(&self, iterable: I) -> &'arena mut [T]
        where
          I: IntoIterator<Item = T>,
          <I as IntoIterator>::Item: 'arena,
        {
          unsafe {
            let buffer = Box::leak(
              SmallVec::<[_; 8]>::from_iter(iterable).into_boxed_slice(),
            );
            let ptr = self.alloc_uninit_slice(buffer.len());
            ptr::copy(buffer as *mut [T] as *mut T, ptr, buffer.len());
            &mut *slice::from_raw_parts_mut(ptr, buffer.len())
          }
        }
      }
    };
  };
}
pub(crate) use impl_for_base;

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
      $visibility:vis struct $arena:ident<$lifetime:lifetime> {
        $(
          $(#[$field_attr:meta])*
          $field_vis:vis $field:ident: $field_ty:ty
        ),+,
      }
      Typed {
        $($typed_vis:vis $alias:ident: $typed:ty),*,
      }
      $($dropless:ident { $($anon:ty)* })*
  ) => {
    $(#[$attr])*
    $visibility struct $arena<$lifetime> {
      $(
        $(#[$field_attr])*
        $field_vis $field: $field_ty
      ),+,

      $($alias: $crate::TypedArena<$typed>),*,
      $($dropless: $crate::DroplessArena),*
    }

    #[allow(dead_code)]
    impl<$lifetime> $arena<$lifetime> {
      $(
        $typed_vis fn $alias(
          &$lifetime self
        ) -> impl Iterator<Item = &$lifetime $typed> + $lifetime {
          self.$alias.iter()
        }
      )*
    }

    #[allow(clippy::mut_from_ref)]
    const _: () = {
      use $crate::Arena as _;

      impl<$lifetime, __T> $crate::Arena<$lifetime, __T> for $arena<$lifetime>
      where
        __T: Dispatch<$lifetime> + $lifetime
      {
        fn alloc(&self, object: __T) -> &$lifetime mut __T {
          object.dispatch_one(self)
        }

        fn alloc_iter<I>(&self, iter: I) -> &$lifetime mut [I::Item]
        where
          I: IntoIterator<Item = __T>,
          <I as IntoIterator>::Item: Dispatch<$lifetime> + $lifetime
        {
          <<I as IntoIterator>::Item as Dispatch<$lifetime>>::dispatch_iter(iter, self)
        }
      }

      $visibility trait Dispatch<$lifetime>: Sized {
        fn dispatch_one(self, arena: &$arena<$lifetime>) -> &$lifetime mut Self;

        fn dispatch_iter<I>(iter: I, arena: &$arena<$lifetime>) -> &$lifetime mut[Self]
        where
          I: IntoIterator<Item = Self>,
          <I as IntoIterator>::Item: $lifetime;
      }

      $(
        #[automatically_derived]
        impl<$lifetime> Dispatch<$lifetime> for $typed {
          fn dispatch_one(
            self,
            arena: &$arena<$lifetime>,
          ) -> &$lifetime mut Self {
            arena.$alias.alloc(self)
          }

          fn dispatch_iter<I>(
            iter: I,
            arena: &$arena<$lifetime>,
          ) -> &$lifetime mut[Self]
            where
              I: IntoIterator<Item = Self>,
              <I as IntoIterator>::Item: $lifetime,
            {
              arena.$alias.alloc_iter(iter)
            }
          }
        )*

        $($(
          #[automatically_derived]
          impl<$lifetime> Dispatch<$lifetime> for $anon {
            fn dispatch_one(
              self,
              arena: &$arena<$lifetime>,
            ) -> &$lifetime mut Self {
              arena.$dropless.alloc(self)
            }

            fn dispatch_iter<I>(
              iter: I,
              arena: &$arena<$lifetime>,
            ) -> &$lifetime mut[Self]
            where
              I: IntoIterator<Item = Self>,
              <I as IntoIterator>::Item: $lifetime,
            {
              arena.$dropless.alloc_iter(iter)
            }
          }
        )*)*
      };
    };
}

#[cfg(test)]
//noinspection DuplicatedCode
mod tests {
  use super::Arena as _;
  use alloc::vec::Vec;
  use core::cell::Cell;

  declare_arena! {
    #[derive(Default)]
    struct Arena<'a> { _m: (), }

    Typed { d: DropCounter<'a>, }

    dropless { i128 }
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