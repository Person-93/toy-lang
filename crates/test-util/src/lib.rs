use anyhow::{Context, Result};
use insta::with_settings;
pub use insta::{assert_debug_snapshot, assert_snapshot};
use std::{fs, path::Path};

pub fn snapshots(f: fn(&str) -> ()) -> Result<()> {
  let root = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../.."));
  let dir = root.join("toys");
  for entry in
    fs::read_dir(&dir).with_context(|| format!("missing toys directory: {}", dir.display()))?
  {
    let entry = entry.context("failed reading directory entry")?;
    if !entry
      .file_type()
      .context("failed reading file type")?
      .is_file()
    {
      continue;
    }
    let path = entry.path();
    if let Some(ext) = path.extension() {
      if ext.to_str().unwrap() != "toy" {
        continue;
      }
    }

    let text = fs::read_to_string(&path)
      .context("failed reading file")?
      // strip '\r' so snapshots match on windows
      .replace('\r', "");

    with_settings!({
      snapshot_suffix => path.file_name().unwrap().to_str().unwrap(),
    }, {
      f(&text);
    });
  }

  Ok(())
}
