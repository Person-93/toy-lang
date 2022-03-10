#![forbid(unsafe_code)]
#![no_std]

extern crate alloc;

pub use self::span::*;

pub mod source;
mod span;
pub mod symbol;
