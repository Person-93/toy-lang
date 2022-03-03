use crate::{emitter::Emitter, Diagnostic, DiagnosticBuilder, Level};
use std::cell::RefCell;

pub struct Handler {
  emitter: Box<RefCell<dyn Emitter>>,
}

impl Handler {
  pub fn emit_diagnostic(&self, diagnostic: &Diagnostic) {
    // TODO avoid emitting duplicate diagnostics
    self.emitter.borrow_mut().emit_diagnostic(diagnostic)
  }

  pub fn fatal(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Fatal, message)
  }

  pub fn error(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Error, message)
  }

  pub fn warn(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Warning, message)
  }
}
