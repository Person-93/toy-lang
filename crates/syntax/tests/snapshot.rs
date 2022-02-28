use anyhow::{Context, Result};
use chumsky::{Parser, Stream};
use insta::{assert_debug_snapshot, with_settings};
use logos::Logos;
use std::fs;

fn main() -> Result<()> {
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
    let tokens = toy_lang_syntax::tokens::Token::lexer(&text).spanned();
    with_settings!({snapshot_suffix => path.file_name().unwrap().to_str().unwrap()}, {
      assert_debug_snapshot!(toy_lang_syntax::ast::parse::file().parse_recovery_verbose(Stream::from_iter(text.len()..text.len(), tokens)));
    });
  }

  Ok(())
}
