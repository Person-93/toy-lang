#![forbid(unsafe_code)]

use core::cell::RefCell;
use std::{
  fs,
  path::{Path, PathBuf},
};
use toyc_errors::Handler;
use toyc_span::source::{SourceMap, SrcRef};

#[non_exhaustive]
pub struct Session {
  pub job_name: String,
  pub root_source_file_path: Option<PathBuf>,
  pub handler: Handler,
  pub root_source: SrcRef,
  source_map: RefCell<SourceMap>,
}

impl Session {
  pub fn new(
    job_name: String,
    root_source_file: PathBuf,
    handler: Handler,
  ) -> Session {
    let mut source_map = SourceMap::default();
    let source_root = source_map.insert(
      root_source_file.to_str().unwrap(),
      fs::read_to_string(&root_source_file).unwrap(),
    );
    Session {
      job_name,
      root_source_file_path: Some(root_source_file),
      handler,
      root_source: source_root,
      source_map: RefCell::new(source_map),
    }
  }

  pub fn from_str(job_name: String, text: String, handler: Handler) -> Session {
    let mut source_map = SourceMap::default();
    let source_root = source_map.insert("", text);
    Session {
      job_name,
      root_source_file_path: None,
      handler,
      root_source: source_root,
      source_map: RefCell::new(source_map),
    }
  }

  pub fn load_source<P: AsRef<Path>>(&self, path: P) -> SrcRef {
    let path = path.as_ref();

    self
      .source_map
      .borrow_mut()
      .insert(path.to_str().unwrap(), fs::read_to_string(path).unwrap())
  }
}
