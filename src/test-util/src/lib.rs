pub use insta::{assert_debug_snapshot, assert_snapshot, glob, with_settings};
use std::fs;

pub fn snapshots(f: fn(&str) -> ()) {
  glob!("toys/*.toy", |path| {
    let text = fs::read_to_string(&path)
      .expect("failed reading file")
      // strip '\r' so snapshots match on windows
      .replace('\r', "");

    with_settings!({
      snapshot_suffix => path.file_name().unwrap().to_str().unwrap(),
    }, {
      f(&text);
    });
  });
}
