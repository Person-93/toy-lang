use core::hash::{Hash, Hasher};
use serde::{Deserialize, Serialize};
use xxhash_rust::xxh64::Xxh64;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Fingerprint(u64, u64);

impl Fingerprint {
  pub fn new<T: Hash + ?Sized>(object: &T) -> Self {
    let mut fingerprinter = Fingerprinter::new();
    fingerprinter.add(object);
    fingerprinter.finish()
  }
}

pub struct Fingerprinter(Xxh64, Xxh64);

impl Default for Fingerprinter {
  fn default() -> Self {
    Self::new()
  }
}

impl Fingerprinter {
  pub const fn new() -> Self {
    Fingerprinter(Xxh64::new(0), Xxh64::new(u64::MAX / 2))
  }

  pub fn add<T: Hash + ?Sized>(&mut self, object: &T) {
    object.hash(&mut self.0);
    object.hash(&mut self.1);
  }

  pub fn finish(self) -> Fingerprint {
    Fingerprint(self.0.finish(), self.1.finish())
  }
}
