use heck::{ToPascalCase, ToShoutySnakeCase};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Deserialize)]
pub struct Specs {
  keywords: HashMap<String, String>,
  reserved: HashMap<String, String>,
  operators: HashMap<String, Operator>,
  #[allow(dead_code)]
  prefix_operators: HashMap<String, String>,
  #[allow(dead_code)]
  postfix_operators: HashMap<String, String>,
  #[allow(dead_code)]
  punctuation: HashMap<String, String>,
  delimiters: HashMap<String, Delimiter>,
  overloaded: HashSet<String>,
}

#[derive(Debug, Deserialize)]
pub struct Operator {
  symbol: String,
  #[allow(dead_code)]
  precedence: i8,
}

#[derive(Debug, Deserialize)]
pub struct Delimiter {
  open: String,
  close: String,
}

impl Specs {
  pub fn generate_keywords_mod(&self) -> TokenStream {
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

  pub fn generate_tokens_mod(&self) -> TokenStream {
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

    let delimiters = self.filter_delimiters().map(|(ident, symbol)| {
      quote! {
        #[token(#symbol)]
        #ident
      }
    });

    let punctuation = self.punctuation.iter().map(|(name, symbol)| {
      let name = name.to_pascal_case();
      let ident = format_ident!("{name}");

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

        #[regex(r#""([^"]|(\\"))*""#, |lex| super::StrLit(lex.slice().to_owned()))]
        StrLit(super::StrLit),

        #[regex(r#"(0(x|b))?\d+(\.\d+)((u|i|f)\d+)"#, super::num_lit)]
        NumLit(super::NumLit),

        #[regex(r"[[:alpha:]_][[:alnum:]_]*", |lex| lex.slice().to_owned())]
        Ident(String),

        #(#keywords),*,
        #(#operators),*,
        #(#delimiters),*,
        #(#punctuation),*,
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

    let delimiters = self
      .filter_delimiters()
      .map(|(ident, symbol)| quote! { Token::#ident => write!(f, "{}", #symbol) });

    let punctuation = self.punctuation.iter().map(|(name, symbol)| {
      let name = name.to_pascal_case();
      let ident = format_ident!("{name}");

      quote! { Token::#ident => write!(f, #symbol) }
    });

    quote! {
      impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          #[allow(clippy::write_literal)]
          match self {
            Token::Error => write!(f, "INVALID_TOKEN"),
            Token::StrLit(lit) => std::fmt::Display::fmt(lit, f),
            Token::NumLit(lit) => std::fmt::Display::fmt(lit, f),
            Token::Ident(id) => std::fmt::Display::fmt(id, f),
            #(#operators),*,
            #(#keywords),*,
            #(#delimiters),*,
            #(#punctuation),*,
          }
        }
      }
    }
  }

  fn filter_delimiters(&self) -> impl Iterator<Item = (Ident, &str)> {
    self
      .delimiters
      .iter()
      .map(|(name, Delimiter { open, close })| {
        [
          (
            format_ident!("{}Open", name.to_pascal_case()),
            open.as_str(),
          ),
          (
            format_ident!("{}Close", name.to_pascal_case()),
            close.as_str(),
          ),
        ]
      })
      .flatten()
      .filter(|(_, symbol)| !self.overloaded.contains(*symbol))
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
