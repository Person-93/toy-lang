use crate::{emitter::Emitter, Diagnostic, DiagnosticBuilder, Level};
use std::{cell::RefCell, num::NonZeroI32};

pub struct Handler {
  inner: RefCell<HandlerInner>,
}

struct HandlerInner {
  emitter: Box<dyn Emitter>,
  err_count: usize,
  warn_count: usize,
}

impl Handler {
  pub fn new(emitter: Box<dyn Emitter>) -> Handler {
    Self {
      inner: RefCell::new(HandlerInner {
        emitter,
        err_count: 0,
        warn_count: 0,
      }),
    }
  }

  pub fn emit_diagnostic(&self, diagnostic: &Diagnostic) {
    // TODO avoid emitting duplicate diagnostics
    let mut inner = self.inner.borrow_mut();
    match diagnostic.level {
      Level::Note => {}
      Level::Warning => inner.warn_count += 1,
      Level::Error | Level::Fatal => inner.err_count += 1,
      Level::Bug => todo!(),
    }
    inner.emitter.emit_diagnostic(diagnostic);
  }

  pub fn note(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Note, message)
  }

  pub fn warn(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Warning, message)
  }

  pub fn error(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Error, message)
  }

  pub fn fatal(&self, message: impl Into<String>) -> DiagnosticBuilder {
    DiagnosticBuilder::new(self, Level::Fatal, message)
  }

  pub fn bug(&self, message: &str) -> ! {
    self.emit_diagnostic(&Diagnostic::new(Level::Bug, message));
    panic!();
  }

  pub fn err_exit(&self, code: NonZeroI32) -> ! {
    if self.inner.borrow().err_count == 0 {
      self.bug("attempted to exit with errors but no errors were reported");
    }
    self.print_error_report();
    std::process::exit(code.get());
  }

  /// Prints an error report and exits with code 101 if there were errors
  pub fn finish(self) {
    self.print_error_report();
    let mut inner = self.inner.borrow_mut();
    if inner.err_count > 0 {
      std::process::exit(101);
    } else {
      inner.emitter.emit_diagnostic(&Diagnostic::new(
        Level::Note,
        "all checks passed but generating the output file is not yet implemented",
      ));
    }
  }

  fn print_error_report(&self) {
    let mut inner = self.inner.borrow_mut();

    let warn_count = inner.warn_count;
    let err_count = inner.err_count;

    let warnings = || match warn_count {
      0 => String::new(),
      1 => "1 warning emitted".to_string(),
      count => format!("{count} warnings emitted"),
    };

    let errors = || match err_count {
      0 => String::new(),
      1 => "aborting due to previous error".to_string(),
      count => format!("aborting due to {count} previous errors"),
    };

    match (inner.warn_count == 0, inner.err_count == 0) {
      (true, true) => (),
      (true, false) => inner
        .emitter
        .emit_diagnostic(&Diagnostic::new(Level::Warning, warnings())),
      (false, true) => inner
        .emitter
        .emit_diagnostic(&Diagnostic::new(Level::Fatal, errors())),
      (false, false) => inner.emitter.emit_diagnostic(&Diagnostic::new(
        Level::Fatal,
        format!("{}; {}", errors(), warnings()),
      )),
    }
  }
}
