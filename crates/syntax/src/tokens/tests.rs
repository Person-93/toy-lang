use super::*;
use anyhow::{Context, Result};
use insta::{assert_snapshot, with_settings};
use logos::{Logos, Span};
use std::{
  fmt::{self, Display, Formatter},
  fs,
};

#[test]
fn snapshot() -> Result<()> {
  let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/../../toys");
  for entry in fs::read_dir(dir).with_context(|| format!("missing toys directory: {dir}"))? {
    let entry = entry.context("failed reading directory entry")?;
    if !entry
      .file_type()
      .context("failed reading file type")?
      .is_file()
    {
      continue;
    }
    let path = entry.path();
    if let Some(ext) = path.extension() {
      if ext.to_str().unwrap() != "toy" {
        continue;
      }
    }

    let text = fs::read_to_string(&path)
      .context("failed reading file")?
      // strip '\r' so snapshots match on windows
      .replace('\r', "");
    with_settings!({snapshot_suffix => path.file_name().unwrap().to_str().unwrap()}, {
      assert_snapshot!(Token::lexer(&text)
        .spanned()
        .map(|(token, span)| Output(token, span.clone(), &text[span]).to_string())
        .collect::<Vec<_>>()
        .join("\n"))
    });
  }

  return Ok(());

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
}
