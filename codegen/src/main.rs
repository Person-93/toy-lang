mod specs;

use crate::specs::Specs;
use anyhow::Result;
use heck::{ToPascalCase, ToShoutySnakeCase};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::path::{Path, PathBuf};

fn main() -> Result<()> {
  let specs = std::fs::read_to_string("specs.toml")?;
  let specs: specs::Specs = toml::de::from_str(&specs)?;

  std::fs::write(
    make_path_name("tokens"),
    specs.generate_tokens_mod().to_string(),
  )?;

  std::fs::write(
    make_path_name("keywords"),
    specs.generate_keywords_mod().to_string(),
  )?;

  Ok(())
}

fn make_path_name(mod_name: &str) -> PathBuf {
  Path::new("./crates/compiler/src/")
    .join(mod_name)
    .join("generated.rs")
}

impl Specs {
  fn generate_keywords_mod(&self) -> TokenStream {
    let keywords = self
      .keywords
      .iter()
      .map(|(name, keyword)| expand_keyword(name, keyword));

    let reserved = self
      .reserved
      .iter()
      .map(|(name, keyword)| expand_keyword(name, keyword));

    quote! {
      use super::super::tokens::StaticToken;

      #(#keywords)
      *

      pub mod reserved {
        use super::StaticToken;

        #(#reserved)
        *
      }
    }
  }

  fn generate_tokens_mod(&self) -> TokenStream {
    TokenStream::from_iter(
      self
        .generate_tokens_enum()
        .into_iter()
        .chain(self.generate_tokens_fmt().into_iter()),
    )
  }

  fn generate_tokens_enum(&self) -> TokenStream {
    let keywords = self.keywords.iter().map(|(name, keyword)| {
      let ident = format_ident!("{}", name.to_pascal_case());
      quote! {
        #[token(#keyword)]
        #ident
      }
    });

    let operators = self.operators.iter().map(|(name, operator)| {
      let symbol = &operator.symbol;
      let ident = format_ident!("{}", name.to_pascal_case());
      quote! {
        #[token(#symbol)]
        #ident
      }
    });

    quote! {
      #[derive(Clone, Debug, logos::Logos, Eq, PartialEq)]
      #[cfg_attr(test, derive(serde::Serialize, serde::Deserialize))]
      pub enum Token {
        #[regex(r"\s+", logos::skip)]
        #[error]
        Error,

        #(#keywords),*,
        #(#operators),*,

        #[regex(r#""([^"]|(\\"))*""#, |lex| super::StrLit(lex.slice().to_owned()))]
        StrLit(super::StrLit),

        #[regex(r#"(0(x|b))?\d+(\.\d+)((u|i|f)\d+)"#, super::num_lit)]
        NumLit(super::NumLit),
      }
    }
  }

  fn generate_tokens_fmt(&self) -> TokenStream {
    let keywords = self.keywords.iter().map(|(name, kw)| {
      let ident = format_ident!("{}", name.to_pascal_case());
      quote! { Token::#ident => write!(f, #kw) }
    });

    let operators = self.operators.iter().map(|(name, operator)| {
      let ident = format_ident!("{}", name.to_pascal_case());
      let symbol = &operator.symbol;
      quote! { Token::#ident => write!(f, #symbol) }
    });

    quote! {
      impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          match self {
            Token::Error => write!(f, "INVALID_TOKEN"),
            Token::StrLit(lit) => std::fmt::Display::fmt(lit, f),
            Token::NumLit(lit) => std::fmt::Display::fmt(lit, f),
            #(#operators),*,
            #(#keywords),*,
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
    pub struct #ident;

    impl StaticToken for #ident {
      fn name() -> &'static str {
        #name
      }

      fn symbol() -> &'static str {
        #keyword
      }
    }
  }
}
