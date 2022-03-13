use super::{CanBeFingerprinted, Fingerprint, Fingerprinter};
use core::{
  hash::{Hash, Hasher},
  ops::Deref,
};

#[derive(Debug)]
pub struct Fingerprinted<'a, T: ?Sized>(&'a T, Fingerprint);

impl<T: ?Sized> Copy for Fingerprinted<'_, T> {}

impl<T: ?Sized> Clone for Fingerprinted<'_, T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T: ?Sized> Fingerprinted<'_, T> {
  pub fn get_fingerprint(&self) -> Fingerprint {
    self.1
  }
}

impl<T: ?Sized> Hash for Fingerprinted<'_, T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.1.hash(state);
  }
}

impl<T: ?Sized> Eq for Fingerprinted<'_, T> {}

impl<T: ?Sized> PartialEq for Fingerprinted<'_, T> {
  fn eq(&self, other: &Self) -> bool {
    self.1 == other.1
  }
}

impl<T: ?Sized> Deref for Fingerprinted<'_, T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    self.0
  }
}

impl<T> CanBeFingerprinted for Fingerprinted<'_, T> {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
    self.get_fingerprint().fingerprint(fingerprinter);
  }
}
