use anyhow::Result;
use chumsky::{Parser, Stream};
use logos::Logos;
use test_util::{assert_debug_snapshot, snapshots};
use toyc_ast::{ast, tokens::Token};

fn main() -> Result<()> {
  snapshots(|text| {
    let tokens = Token::lexer(text).spanned();
    assert_debug_snapshot!(
      ast::parse::file().parse_recovery(Stream::from_iter(text.len()..text.len(), tokens))
    );
  })
}
