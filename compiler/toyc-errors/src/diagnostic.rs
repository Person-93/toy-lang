use crate::Level;
use serde::Serialize;
use toyc_span::Span;

#[derive(Clone, Debug, Serialize)]
pub struct Diagnostic {
  pub(crate) level: Level,
  pub(crate) message: String,
  pub(crate) spans: Vec<SpanLabel>,
  pub(crate) children: Vec<SubDiagnostic>,
}

#[derive(Clone, Debug, Serialize)]
pub struct SubDiagnostic {
  pub(crate) level: Level,
  pub(crate) label: String,
}

#[derive(Clone, Debug, Serialize)]
pub(crate) struct SpanLabel {
  span: Span,
  label: String,
}

impl Diagnostic {
  pub(crate) fn new(level: Level, message: impl Into<String>) -> Self {
    Self {
      message: message.into(),
      spans: Default::default(),
      children: Default::default(),
      level,
    }
  }

  pub fn add_span(
    &mut self,
    span: Span,
    label: impl Into<String>,
  ) -> &mut Self {
    self.spans.push(SpanLabel {
      span,
      label: label.into(),
    });
    self
  }

  pub fn add_spans(
    &mut self,
    spans: impl IntoIterator<Item = Span>,
    label: impl Into<String>,
  ) -> &mut Self {
    let label = label.into();
    for span in spans {
      self.add_span(span, &label);
    }
    self
  }

  pub fn attach_note(
    &mut self,
    level: Level,
    label: impl Into<String>,
  ) -> &mut Self {
    self.children.push(SubDiagnostic {
      level,
      label: label.into(),
    });
    self
  }
}
