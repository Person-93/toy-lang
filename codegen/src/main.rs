mod specs;

use anyhow::Result;

use crate::specs::Specs;
use proc_macro2::TokenStream;
use std::path::Path;

const RULES: &[Rule] = &[
  ("tokens", Specs::generate_tokens_mod),
  ("keywords", Specs::generate_keywords_mod),
];

type Rule = (&'static str, fn(&Specs) -> TokenStream);

fn main() -> Result<()> {
  let specs = std::fs::read_to_string("specs.toml")?;
  let specs: specs::Specs = toml::de::from_str(&specs)?;

  for (name, func) in RULES {
    let name = Path::new("./crates/compiler/src/")
      .join(name)
      .join("generated.rs");

    std::fs::write(name, func(&specs).to_string())?;
  }

  Ok(())
}
