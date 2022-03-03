use crate::{Diagnostic, Handler, Level};
use std::{
  ops::{Deref, DerefMut},
  thread::panicking,
};

pub struct DiagnosticBuilder<'a> {
  diagnostic: Diagnostic,
  state: DiagnosticBuilderState<'a>,
}

enum DiagnosticBuilderState<'a> {
  Emittable(&'a Handler),
  Handled,
}

impl<'a> DiagnosticBuilder<'a> {
  pub(crate) fn new(handler: &'a Handler, level: Level, message: impl Into<String>) -> Self {
    Self {
      diagnostic: Diagnostic::new(level, message),
      state: DiagnosticBuilderState::Emittable(handler),
    }
  }

  pub fn emit(mut self) {
    match self.state {
      DiagnosticBuilderState::Emittable(handler) => {
        handler.emit_diagnostic(&self.diagnostic);
        self.state = DiagnosticBuilderState::Handled;
      }
      DiagnosticBuilderState::Handled => {}
    }
  }

  pub fn cancel(mut self) {
    self.state = DiagnosticBuilderState::Handled;
  }
}

impl Deref for DiagnosticBuilder<'_> {
  type Target = Diagnostic;

  fn deref(&self) -> &Self::Target {
    &self.diagnostic
  }
}

impl DerefMut for DiagnosticBuilder<'_> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.diagnostic
  }
}

impl Drop for DiagnosticBuilder<'_> {
  fn drop(&mut self) {
    match self.state {
      DiagnosticBuilderState::Emittable(handler) => {
        if !panicking() {
          handler.emit_diagnostic(&Diagnostic::new(
            Level::Bug,
            "A diagnostic was created but never handled",
          ));
          handler.emit_diagnostic(&self.diagnostic);
          panic!();
        }
      }
      DiagnosticBuilderState::Handled => {}
    }
  }
}
