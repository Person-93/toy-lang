use self::tokens::{DynamicToken, StaticToken, Token, TokenSet};
use crate::collections::{NamedItem, NamedSet, Unnamed};
use anyhow::{Context, Result};
use heck::{ToPascalCase, ToShoutySnakeCase};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use serde::Deserialize;
use std::{
  collections::{BTreeMap, BTreeSet},
  fs,
};
use type_utils::TypeUtils;

mod ast;
mod tokens;

#[derive(Debug, Deserialize)]
#[serde(bound(deserialize = "'de: 's"))]
pub struct Specs<'s> {
  static_tokens: TokenSet<'s, StaticToken<'s>>,
  dynamic_tokens: TokenSet<'s, DynamicToken<'s>>,
  keywords: BTreeSet<String>,
  reserved: BTreeSet<String>,
  #[allow(dead_code)]
  operators: BTreeMap<String, Operator>,
  #[allow(dead_code)]
  prefix_operators: BTreeMap<String, String>,
  #[allow(dead_code)]
  postfix_operators: BTreeMap<String, String>,
  delimiters: NamedSet<'s, Delimiter<'s>>,
}

#[derive(Debug, Deserialize)]
pub struct Operator {
  #[allow(dead_code)]
  symbol: String,
  #[allow(dead_code)]
  precedence: i8,
}

#[derive(Debug, TypeUtils, Deserialize)]
#[tu_derive(Deserialize)]
#[omit(pub UnnamedDelimiter<'d> {name})]
pub struct Delimiter<'d> {
  name: &'d str,
  open: &'d str,
  close: &'d str,
}

impl Specs<'_> {
  pub fn generate_keywords_mod(&self) -> Result<TokenStream> {
    let keywords = self
      .keywords
      .iter()
      .map(|name| -> Result<TokenStream> {
        Ok(expand_keyword(
          name,
          self
            .static_tokens
            .get(name)
            .map(|t| t.symbol())
            .context("missing token")?,
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
            .map(|t| t.symbol())
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
    tokens.extend(self.generate_tokens_parse());
    Ok(tokens)
  }

  pub fn generate_ast_mod(&self) -> Result<TokenStream> {
    ast::generate(&fs::read_to_string("specs/toy.ast")?, self)
  }

  fn generate_tokens_enum(&self) -> TokenStream {
    let dynamic_tokens = self.dynamic_tokens.iter().map(|token| {
      let name = format_ident!("{}", token.name().to_pascal_case());
      let pattern = token.pattern();
      quote! {
        #[regex(#pattern, |lex| super::DynamicToken::new(lex))]
        #name(super::#name)
      }
    });

    let static_tokens = self.static_tokens.iter().map(|token| {
      let name = format_ident!("{}", token.name().to_pascal_case());
      let keyword = token.symbol();
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
    let dynamic_tokens = self.dynamic_tokens.iter().map(|token| {
      let name = format_ident!("{}", token.name().to_pascal_case());
      quote! { Token::#name(value) => std::fmt::Display::fmt(value, f) }
    });

    let static_tokens = self.static_tokens.iter().map(|token| {
      let name = format_ident!("{}", token.name().to_pascal_case());
      let symbol = token.symbol();
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

  fn generate_tokens_parse(&self) -> TokenStream {
    let static_tokens = self.static_tokens.iter().map(|token| {
      let ident = if is_rust_keyword(token.name()) {
        format_ident!("{}_", token.name())
      } else {
        format_ident!("{}", token.name())
      };
      let ty = format_ident!("{}", token.name().to_pascal_case());
      quote! {
        pub fn #ident() -> impl Parser<Token, (), Error = Error> + Clone {
          just(Token::#ty).ignored()
        }
      }
    });

    let dynamic_tokens = self.dynamic_tokens.iter().map(|token| {
      let ident = format_ident!("{}", token.name());
      let ty = format_ident!("{}", token.name().to_pascal_case());
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

impl<'d> NamedItem<'d> for Delimiter<'d> {
  type Unnamed = UnnamedDelimiter<'d>;

  fn name(&self) -> &'d str {
    self.name
  }

  fn dummy(name: &'d str) -> Self {
    Self {
      name,
      open: "",
      close: "",
    }
  }
}

impl<'d> Unnamed<'d> for UnnamedDelimiter<'d> {
  type Named = Delimiter<'d>;

  fn add_name(self, name: &'d str) -> Self::Named {
    Delimiter {
      name,
      open: self.open,
      close: self.close,
    }
  }
}

fn is_rust_keyword(word: &str) -> bool {
  #[rustfmt::skip]
  const KEYWORDS: &[&str] = &[
    // strict keywords
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
    "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
    "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
    "unsafe", "use", "where", "while",

    // reserved keywords
    "abstract", "become", "box", "do", "final", "macro", "override", "priv", "try", "typeof",
    "unsized", "virtual", "yield",
  ];

  KEYWORDS.contains(&word)
}
