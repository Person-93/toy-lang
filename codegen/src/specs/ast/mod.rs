use crate::Specs;
use anyhow::{Context, Result};
use proc_macro2::TokenStream;
use serde::Deserialize;
use std::fmt::{self, Display, Formatter};

mod collapsed;
mod lex;
mod parsed;
mod print;
mod raw;

pub fn generate<'ast>(text: &'ast str, specs: &'ast Specs<'ast>) -> Result<TokenStream> {
  Ok(
    raw::Ast::parse(text)
      .context("failed to lex AST description")?
      .transform(specs)
      .context("failed to parse AST description")?
      .transform()
      .context("failed to collapse AST description")?
      .print(specs),
  )
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Deserialize)]
pub struct Ident<'a>(&'a str);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Modifier {
  Repeat,
  Csv,
  OnePlus,
  CsvOnePlus,
  Optional,
}

impl Display for Modifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Modifier::Repeat => write!(f, "*"),
      Modifier::Csv => write!(f, ",*"),
      Modifier::OnePlus => write!(f, "+"),
      Modifier::CsvOnePlus => write!(f, ",+"),
      Modifier::Optional => write!(f, "?"),
    }
  }
}
