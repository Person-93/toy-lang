#![forbid(unsafe_code)]
#![no_std]

extern crate alloc;

pub use self::span::*;

mod span;
pub mod symbol;
