use anyhow::Result;
use ast_description_lang::{Delimiter, Ident, NamedSet};
use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::quote;
use serde::Deserialize;
use std::fs;

#[derive(Debug, Deserialize)]
#[serde(bound(deserialize = "'de: 's"))]
pub struct Specs<'s> {
  static_tokens: IndexMap<Ident<'s>, &'s str>,
  dynamic_tokens: IndexMap<Ident<'s>, &'s str>,
  #[allow(dead_code)]
  operators: IndexMap<Ident<'s>, Operator>,
  #[allow(dead_code)]
  prefix_operators: IndexMap<Ident<'s>, &'s str>,
  #[allow(dead_code)]
  postfix_operators: IndexMap<Ident<'s>, &'s str>,
  delimiters: NamedSet<'s, Delimiter<'s>>,
}

#[derive(Debug, Deserialize)]
pub struct Operator {
  #[allow(dead_code)]
  symbol: String,
  #[allow(dead_code)]
  precedence: i8,
}

impl<'s> ast_description_lang::Specs<'s> for Specs<'s> {
  fn delimiters(&self) -> &NamedSet<'s, Delimiter<'s>> {
    &self.delimiters
  }

  fn static_tokens(&self) -> &IndexMap<Ident<'s>, &'s str> {
    &self.static_tokens
  }

  fn dynamic_tokens(&self) -> &IndexMap<Ident<'s>, &'s str> {
    &self.dynamic_tokens
  }
}

impl Specs<'_> {
  pub fn generate_tokens_mod(&self) -> Result<TokenStream> {
    let enum_ = self.generate_tokens_enum();
    let fmt = self.generate_tokens_fmt();
    let parse = self.generate_tokens_parse();
    Ok(quote! {
      #![allow(dead_code)]
      #enum_
      #fmt
      #parse
    })
  }

  pub fn generate_ast_mod(&self) -> Result<TokenStream> {
    ast_description_lang::generate(&fs::read_to_string("specs/toy.ast")?, self)
  }

  fn generate_tokens_enum(&self) -> TokenStream {
    let dynamic_tokens = self.dynamic_tokens.iter().map(|token| {
      let name = token.0.as_type();
      let pattern = token.1;
      quote! {
        #[regex(#pattern, |lex| super::DynamicToken::new(lex))]
        #name(super::#name)
      }
    });

    let static_tokens = self.static_tokens.iter().map(|(name, keyword)| {
      let name = name.as_type();
      quote! {
        #[token(#keyword)]
        #name
      }
    });

    quote! {
      #[derive(Clone, Debug, logos::Logos, Eq, PartialEq, Hash)]
      #[cfg_attr(test, derive(serde::Serialize))]
      pub enum Token {
        #[regex(r"\s+", logos::skip)]
        #[error]
        Error,

        #(#dynamic_tokens),*,
        #(#static_tokens),*,
      }
    }
  }

  fn generate_tokens_fmt(&self) -> TokenStream {
    let dynamic_tokens = self.dynamic_tokens.iter().map(|(token, _)| {
      let token = token.as_type();
      quote! { Token::#token(value) => std::fmt::Display::fmt(value, f) }
    });

    let static_tokens = self.static_tokens.iter().map(|(token, symbol)| {
      let token = token.as_type();
      quote! { Token::#token => write!(f, "{}", #symbol) }
    });

    quote! {
      impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          #[allow(clippy::write_literal)]
          match self {
            Token::Error => write!(f, "INVALID_TOKEN"),
            #(#dynamic_tokens),*,
            #(#static_tokens),*,
          }
        }
      }
    }
  }

  fn generate_tokens_parse(&self) -> TokenStream {
    let static_tokens = self.static_tokens.iter().map(|(ident, _)| {
      let ty = ident.as_type();
      quote! {
        pub fn #ident() -> impl Parser<Token, (), Error = Error> + Clone {
          just(Token::#ty).ignored()
        }
      }
    });

    let dynamic_tokens = self.dynamic_tokens.iter().map(|(ident, _)| {
      let ty = ident.as_type();
      quote! {
        pub fn #ident() -> impl Parser<Token, super::super::#ty, Error = Error> {
          select!{ Token::#ty(value) => value }
        }
      }
    });

    quote! {
      pub mod parse {
        use chumsky::prelude::*;
        use super::{super::Error, Token};
        #(#static_tokens)*
        #(#dynamic_tokens)*
      }
    }
  }
}
