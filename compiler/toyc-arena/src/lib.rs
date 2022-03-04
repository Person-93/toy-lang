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
