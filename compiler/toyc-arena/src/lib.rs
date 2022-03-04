#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;
extern crate alloc;

pub use self::{dropless::DroplessArena, typed::TypedArena};

mod dropless;
mod typed;

pub trait Arena<T>: Default {
  #[allow(clippy::mut_from_ref)]
  fn alloc(&self, object: T) -> &mut T;
}

// The arenas start with PAGE-sized chunks, and then each new chunk is twice as
// big as its predecessor, up until we reach HUGE_PAGE-sized chunks, whereupon
// we stop growing. This scales well, from arenas that are barely used up to
// arenas that are used for 100s of MiBs. Note also that the chosen sizes match
// the usual sizes of pages and huge pages on Linux.
const PAGE: usize = 4096;
const HUGE_PAGE: usize = 2 * 1024 * 1024;
