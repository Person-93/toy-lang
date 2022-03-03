use crate::{Diagnostic, Handler, Level};
use std::{
  ops::{Deref, DerefMut},
  thread::panicking,
};
use toyc_span::Span;

#[must_use]
pub struct DiagnosticBuilder<'a> {
  diagnostic: Diagnostic,
  state: DiagnosticBuilderState<'a>,
}

enum DiagnosticBuilderState<'a> {
  Emittable(&'a Handler),
  Handled,
}

macro_rules! forward {
  (
    pub fn $n:ident(&self, $($name:ident: $ty:ty),* $(,)?) -> &Self
  ) => {
    pub fn $n(&self, $($name: $ty),*) -> &Self {
      self.diagnostic.$n($(name),*);
      self
    }
  };
  (
    pub fn $n:ident(&mut self, $($name:ident: $ty:ty),* $(,)?) -> &mut Self
  ) => {
    pub fn $n(&mut self, $($name: $ty),*) -> &mut Self {
        self.diagnostic.$n($($name),*);
        self
      }
  }
}

impl<'a> DiagnosticBuilder<'a> {
  pub(crate) fn new(
    handler: &'a Handler,
    level: Level,
    message: impl Into<String>,
  ) -> Self {
    Self {
      diagnostic: Diagnostic::new(level, message),
      state: DiagnosticBuilderState::Emittable(handler),
    }
  }

  pub fn emit(&mut self) {
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

  forward!(
    pub fn add_span(&mut self, span: Span, label: impl Into<String>) -> &mut Self
  );

  forward!(
    pub fn attach_note(&mut self, label: impl Into<String>) -> &mut Self
  );
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
