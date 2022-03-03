use std::path::PathBuf;
use toyc_errors::Handler;

#[non_exhaustive]
pub struct Session {
  pub job_name: String,
  pub root_source_file: Option<PathBuf>,
  pub handler: Handler,
}

impl Session {
  pub fn new(
    job_name: String,
    root_source_file: Option<PathBuf>,
    handler: Handler,
  ) -> Session {
    Session {
      job_name,
      root_source_file,
      handler,
    }
  }
}
