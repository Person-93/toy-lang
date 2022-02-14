use anyhow::{Context, Result};
use heck::{ToPascalCase, ToShoutySnakeCase};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Deserialize)]
pub struct Specs {
  static_tokens: BTreeMap<String, String>,
  dynamic_tokens: BTreeMap<String, String>,
  keywords: BTreeSet<String>,
  reserved: BTreeSet<String>,
  #[allow(dead_code)]
  operators: BTreeMap<String, Operator>,
  #[allow(dead_code)]
  prefix_operators: BTreeMap<String, String>,
  #[allow(dead_code)]
  postfix_operators: BTreeMap<String, String>,
  #[allow(dead_code)]
  delimiters: BTreeMap<String, Delimiter>,
}

#[derive(Debug, Deserialize)]
pub struct Operator {
  #[allow(dead_code)]
  symbol: String,
  #[allow(dead_code)]
  precedence: i8,
}

#[derive(Debug, Deserialize)]
pub struct Delimiter {
  #[allow(dead_code)]
  open: String,
  #[allow(dead_code)]
  close: String,
}

impl Specs {
  pub fn generate_keywords_mod(&self) -> Result<TokenStream> {
    let keywords = self
      .keywords
      .iter()
      .map(|name| -> Result<TokenStream> {
        Ok(expand_keyword(
          name,
          self.static_tokens.get(name).context("missing token")?,
        ))
      })
      .collect::<Result<Vec<_>>>()?;

    let reserved = self
      .reserved
      .iter()
      .map(|name| -> Result<TokenStream> {
        Ok(expand_keyword(
          name,
          self
            .static_tokens
            .get(name)
            .context("missing reserved keyword")?,
        ))
      })
      .collect::<Result<Vec<_>>>()?;

    Ok(quote! {
      use super::super::tokens::StaticToken;

      #(#keywords)
      *

      pub mod reserved {
        use super::StaticToken;

        #(#reserved)
        *
      }
    })
  }

  pub fn generate_tokens_mod(&self) -> Result<TokenStream> {
    let mut tokens = self.generate_tokens_enum();
    tokens.extend(self.generate_tokens_fmt());
    Ok(tokens)
  }

  fn generate_tokens_enum(&self) -> TokenStream {
    let dynamic_tokens = self.dynamic_tokens.iter().map(|(name, pattern)| {
      let name = name.to_pascal_case();
      let name = format_ident!("{name}");
      quote! {
        #[regex(#pattern, |lex| super::DynamicToken::new(lex))]
        #name(super::#name)
      }
    });

    let static_tokens = self.static_tokens.iter().map(|(name, keyword)| {
      let name = name.to_pascal_case();
      let name = format_ident!("{name}");
      quote! {
        #[token(#keyword)]
        #name
      }
    });

    quote! {
      #[derive(Clone, Debug, logos::Logos, Eq, PartialEq)]
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
    let dynamic_tokens = self.dynamic_tokens.iter().map(|(name, _)| {
      let name = name.to_pascal_case();
      let name = format_ident!("{name}");
      quote! { Token::#name(value) => std::fmt::Display::fmt(value, f) }
    });

    let static_tokens = self.static_tokens.iter().map(|(name, symbol)| {
      let name = name.to_pascal_case();
      let name = format_ident!("{name}");
      quote! { Token::#name => write!(f, "{}", #symbol) }
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
}

fn expand_keyword(name: &str, keyword: &str) -> TokenStream {
  let ident = format_ident!("{}", name.to_pascal_case());
  let name = name.to_shouty_snake_case();
  quote! {
    #[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
    #[cfg_attr(test, derive(serde::Serialize))]
    pub struct #ident;

    impl StaticToken for #ident {
      fn name() -> &'static str { #name }

      fn symbol() -> &'static str { #keyword }
    }

    impl std::fmt::Display for #ident {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, #keyword)
      }
    }
  }
}
