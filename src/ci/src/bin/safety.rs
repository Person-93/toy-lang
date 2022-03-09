#![forbid(unsafe_code)]

use anyhow::Result;
use cargo_metadata::MetadataCommand;
use std::{
  fs::File,
  io::{BufRead, BufReader},
};

fn main() -> Result<()> {
  let metadata = MetadataCommand::new().no_deps().exec()?;

  let mut missing_forbid = 0;

  for package in &metadata.packages {
    if package.name == "toyc-arena" {
      continue;
    }

    for target in &package.targets {
      if !target.kind.contains(&"bin".to_string())
        && !target.kind.contains(&"lib".to_string())
      {
        continue;
      }

      let mut first_line = String::new();
      BufReader::new(File::open(&target.src_path)?)
        .read_line(&mut first_line)?;
      if first_line.trim() != "#![forbid(unsafe_code)]" {
        missing_forbid += 1;
        eprintln!(
          "#![forbid(unsafe_code)] should be the first line of {}",
          target.src_path
        );
      }
    }
  }

  // no need to take the time to run destructors
  std::process::exit(missing_forbid.clamp(0, 1));
}
