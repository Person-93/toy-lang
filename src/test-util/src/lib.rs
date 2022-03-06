use core::ops::{Deref, DerefMut};
pub use insta::{assert_debug_snapshot, assert_snapshot, glob, with_settings};
use parking_lot::{RwLock, RwLockReadGuard};
use std::{fs, rc::Rc};
use toyc_errors::emitter::Emitter;
use toyc_errors::{Diagnostic, Handler};
use toyc_session::Session;

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

pub struct TestSession {
  inner: Session,
  diagnostics: Rc<RwLock<Vec<Diagnostic>>>,
}

impl TestSession {
  pub fn new<T: ToString>(job_name: T) -> TestSession {
    let diagnostics = Rc::new(RwLock::default());
    TestSession {
      inner: Session::new(
        job_name.to_string(),
        None,
        Handler::new(Box::new(TestEmitter(diagnostics.clone()))),
      ),
      diagnostics,
    }
  }

  pub fn diagnostics(
    this: &TestSession,
  ) -> impl Deref<Target = [Diagnostic]> + '_ {
    RwLockReadGuard::map(this.diagnostics.read(), Vec::as_slice)
  }
}

impl Deref for TestSession {
  type Target = Session;

  fn deref(&self) -> &Self::Target {
    &self.inner
  }
}

impl DerefMut for TestSession {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.inner
  }
}

#[derive(Debug)]
struct TestEmitter(Rc<RwLock<Vec<Diagnostic>>>);

impl Emitter for TestEmitter {
  fn emit_diagnostic(&mut self, diagnostic: &Diagnostic) {
    self.0.write().push(diagnostic.clone());
  }
}
