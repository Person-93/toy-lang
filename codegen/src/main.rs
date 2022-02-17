use crate::specs::Specs;
use anyhow::Result;
use proc_macro2::TokenStream;
use std::path::Path;

mod collections;
mod specs;

fn main() -> Result<()> {
  let rules = [
    Rule {
      name: "tokens",
      func: Specs::generate_tokens_mod,
    },
    Rule {
      name: "keywords",
      func: Specs::generate_keywords_mod,
    },
  ];

  let specs = std::fs::read_to_string("specs/specs.toml")?;
  let specs: specs::Specs = toml::de::from_str(&specs)?;

  rules.into_iter().try_for_each(|rule| rule.run(&specs))?;

  Ok(())
}

struct Rule<'r, 's> {
  name: &'static str,
  func: fn(&'r Specs<'s>) -> Result<TokenStream>,
}

impl<'r, 's> Rule<'r, 's> {
  fn run(self, specs: &'r Specs<'s>) -> Result<()> {
    let Self { name, func } = self;

    let name = Path::new("./crates/compiler/src/")
      .join(name)
      .join("generated.rs");

    std::fs::write(name, func(specs)?.to_string())?;

    Ok(())
  }
}
