use crate::specs::Specs;
use anyhow::{Context, Result};
use proc_macro2::TokenStream;
use std::{
  io::{self, Write},
  path::Path,
  process::{Command, Stdio},
};

mod specs;

fn main() -> Result<()> {
  let rules = [
    Rule {
      name: "tokens",
      func: Specs::generate_tokens_mod,
    },
    Rule {
      name: "ast",
      func: Specs::generate_ast_mod,
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

    let contents = func(specs)?.to_string();
    let rustfmt = Command::new("rustfmt")
      .stdin(Stdio::piped())
      .stdout(Stdio::piped())
      .stderr(Stdio::piped())
      .spawn()
      .context("failed to start rustfmt")?;
    rustfmt
      .stdin
      .as_ref()
      .unwrap()
      .write_all(contents.as_bytes())
      .context("failed to write generated code to rustfmt")?;
    let output = rustfmt
      .wait_with_output()
      .context("failed waiting for rustfmt")?;

    let output = if output.status.success() {
      &output.stdout
    } else {
      let cerr = io::stderr();
      let mut cerr = cerr.lock();
      cerr
        .write_fmt(format_args!("rustfmt exited with {}\n", output.status))
        .unwrap();
      cerr.write_all(&output.stderr).unwrap();
      contents.as_bytes()
    };

    std::fs::write(name, output)?;

    Ok(())
  }
}
