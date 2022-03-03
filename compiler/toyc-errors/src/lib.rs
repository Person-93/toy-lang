pub use self::{diagnostic::Diagnostic, diagnostic_builder::DiagnosticBuilder, handler::Handler};
use serde::Serialize;
use std::fmt::{self, Display, Formatter};

mod diagnostic;
mod diagnostic_builder;
pub mod emitter;
mod handler;

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize)]
pub enum Level {
  Warning,
  Error,
  Fatal,
  Bug,
}

impl Display for Level {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Level::Warning => write!(f, "warning"),
      Level::Error | Level::Fatal => write!(f, "error"),
      Level::Bug => write!(f, "error: internal compiler error"),
    }
  }
}
