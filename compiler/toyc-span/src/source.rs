use alloc::{rc::Rc, string::String};
use core::ops::Deref;
use hashbrown::HashMap;
use toyc_data_structures::fingerprint::{Fingerprint, Fingerprinter};

/// A hash of the source text and any additional identifier (usually the file path)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourceId(Fingerprint);

#[derive(Debug, Default)]
pub struct SourceMap(HashMap<SourceId, Rc<str>>);

impl SourceMap {
  pub fn insert<S: AsRef<str>>(&mut self, ident: S, text: String) -> SrcRef {
    let mut fingerprinter = Fingerprinter::new();
    fingerprinter.add(ident.as_ref());
    fingerprinter.add(&text);
    let id = SourceId(fingerprinter.finish());
    SrcRef(
      self
        .0
        .entry(id)
        .or_insert_with(|| Rc::from(text.into_boxed_str()))
        .clone(),
    )
  }

  pub fn get(&self, id: SourceId) -> Option<SrcRef> {
    self.0.get(&id).cloned().map(SrcRef)
  }
}

#[derive(Clone, Debug)]
pub struct SrcRef(Rc<str>);

impl Deref for SrcRef {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}
