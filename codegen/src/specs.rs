use anyhow::Result;
use ast_description_lang::{Config, Ident};
use indexmap::IndexMap;
use proc_macro2::TokenStream;
use serde::Deserialize;
use std::fs;

#[derive(Debug, Deserialize)]
#[serde(bound(deserialize = "'de: 's"))]
pub struct Specs<'s> {
  #[serde(flatten)]
  inner: ast_description_lang::Specs<'s>,
  #[allow(dead_code)]
  operators: IndexMap<Ident<'s>, Operator>,
  #[allow(dead_code)]
  prefix_operators: IndexMap<Ident<'s>, &'s str>,
  #[allow(dead_code)]
  postfix_operators: IndexMap<Ident<'s>, &'s str>,
}

#[derive(Debug, Deserialize)]
pub struct Operator {
  #[allow(dead_code)]
  symbol: String,
  #[allow(dead_code)]
  precedence: i8,
}

impl Specs<'_> {
  pub fn generate_tokens_mod(&self, config: &Config<'_>) -> Result<TokenStream> {
    self.inner.generate_tokens_mod(config)
  }

  pub fn generate_ast_mod(&self, config: &Config<'_>) -> Result<TokenStream> {
    ast_description_lang::generate_ast_mod(
      &fs::read_to_string("specs/toy.ast")?,
      &self.inner,
      config,
    )
  }
}
