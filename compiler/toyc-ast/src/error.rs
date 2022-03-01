use crate::tokens::Token;
use chumsky::error::Simple;
use toyc_span::Span;

pub type ParseError = Simple<Token, Span>;
