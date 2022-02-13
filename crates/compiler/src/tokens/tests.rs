use super::*;

use insta::{assert_ron_snapshot, glob};
use logos::Logos;
use std::fs;

#[test]
fn snapshot() {
  glob!("toys/*.toy", |file| {
    let text = fs::read_to_string(file).unwrap();
    assert_ron_snapshot!((Token::lexer(&text).spanned().collect::<Vec<_>>()));
  })
}
