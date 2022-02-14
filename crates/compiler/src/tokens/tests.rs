use super::*;
use std::fmt::Formatter;

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
      match &self.0 {
        Token::Error => write!(f, "INVALID @ {:?}: {}", self.1, self.2),
        Token::StrLit(lit) => write!(f, "STR_LIT @ {:?}: {lit}", self.1),
        Token::NumLit(lit) => write!(f, "NUM_LIT @ {:?}: {lit}", self.1),
        Token::Ident(ident) => write!(f, "IDENT @ {:?}: {ident}", self.1),
        _ => write!(f, "{:?} @ {:?}", self.0, self.1),
      }
    }
  }
}
