use super::*;
use std::fmt::{Debug, Formatter};

use insta::{assert_snapshot, glob};
use logos::{Logos, Span};
use std::fs;

#[test]
fn snapshot() {
  glob!("toys/*.toy", |file| {
    let text = fs::read_to_string(file).unwrap();
    assert_snapshot!(Token::lexer(&text)
      .spanned()
      .map(|(token, span)| Output(token, span.clone(), &text[span]).to_string())
      .collect::<Vec<_>>()
      .join("\n"))
  });

  struct Output<'a>(Token, Span, &'a str);

  impl std::fmt::Display for Output<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
      if let Token::Error = self.0 {
        write!(f, "ERROR @ {:?}: \"{}\"", self.1, self.2)
      } else {
        write!(f, "{:?} @ {:?}", self.0, self.1)
      }
    }
  }
}
