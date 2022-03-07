#![forbid(unsafe_code)]

pub mod ast;
pub mod error;
pub mod tokens;
pub mod visitor;

pub fn parse_single_file(
  text: &str,
  handler: &toyc_errors::Handler,
) -> Option<crate::ast::File> {
  use self::{ast::parse, tokens::TokenIter};
  use chumsky::Parser;

  let tokens = TokenIter::new(text);
  let (file, errors) = parse::file().parse_recovery(tokens.into_stream());
  for error in errors {
    handler
      .error("parsing failed")
      .add_span(error.span(), error.to_string())
      .emit();
  }
  file
}
