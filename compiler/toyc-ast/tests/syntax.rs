use anyhow::Result;
use chumsky::Parser;
use test_util::{assert_debug_snapshot, snapshots};
use toyc_ast::{ast, tokens::TokenIter};

fn main() -> Result<()> {
  snapshots(|text| {
    let tokens = TokenIter::new(text).into_stream();
    assert_debug_snapshot!(ast::parse::file().parse_recovery(tokens));
  })
}
