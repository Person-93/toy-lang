use crate::Diagnostic;
use std::io::Write;

pub trait Emitter {
  fn emit_diagnostic(&mut self, diagnostic: &Diagnostic);
}

pub struct JsonWriterEmitter<T: Write> {
  writer: T,
  pretty: bool,
}

impl<T: Write> JsonWriterEmitter<T> {
  pub fn new(writer: T, pretty: bool) -> Self {
    Self { writer, pretty }
  }
}

impl<T: Write> Emitter for JsonWriterEmitter<T> {
  fn emit_diagnostic(&mut self, diagnostic: &Diagnostic) {
    if self.pretty {
      serde_json::to_writer_pretty(&mut self.writer, diagnostic).unwrap();
    } else {
      serde_json::to_writer(&mut self.writer, diagnostic).unwrap();
    }
  }
}
