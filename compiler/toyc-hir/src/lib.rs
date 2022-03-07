#![forbid(unsafe_code)]
// TODO make toyc-hir no_std?

pub use self::{hir::*, hir_id::*};

mod hir;
mod hir_id;
