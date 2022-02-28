use anyhow::Result;
use logos::{Logos, Span};
use std::fmt::{self, Display, Formatter};
use test_util::{assert_snapshot, snapshots};
use toy_lang_syntax::tokens::Token;

fn main() -> Result<()> {
  snapshots(|text| {
    assert_snapshot!(Token::lexer(text)
      .spanned()
      .map(|(token, span)| Output(token, span.clone(), &text[span]).to_string())
      .collect::<Vec<_>>()
      .join("\n"));
  })
}

struct Output<'a>(Token, Span, &'a str);

impl Display for Output<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match &self.0 {
      Token::Error => write!(f, "INVALID @ {:?}: {}", self.1, self.2),
      Token::StrLit(lit) => write!(f, "StrLit @ {:?}: \"{lit}\"", self.1),
      Token::NumLit(lit) => write!(f, "NumLit @ {:?}: {lit}", self.1),
      Token::Ident(ident) => write!(f, "Ident @ {:?}: {ident}", self.1),
      _ => write!(f, "{:?} @ {:?}", self.0, self.1),
    }
  }
}
