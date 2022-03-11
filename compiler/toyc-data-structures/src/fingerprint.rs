use core::{
  hash::{Hash, Hasher},
  marker::PhantomData,
  mem::Discriminant,
};
use serde::{Deserialize, Serialize};
use xxhash_rust::xxh64::Xxh64;

pub trait CanBeFingerprinted {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter);
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Fingerprint(u64, u64);

impl Fingerprint {
  pub fn new<T: CanBeFingerprinted + ?Sized>(object: &T) -> Self {
    let mut fingerprinter = Fingerprinter::new();
    object.fingerprint(&mut fingerprinter);
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

macro_rules! impl_finger_print {
    ($($ty:ident$(<$($lf:lifetime),*>)?)*) => {
      $(impl $(<$($lf),*>)? $crate::fingerprint::CanBeFingerprinted for $ty {
        #[inline(always)]
        fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
          fingerprinter.add(self);
        }
      })*
    };
}

impl_finger_print! {
  bool char str i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize
}

impl<T> CanBeFingerprinted for Discriminant<T> {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
    fingerprinter.add(self);
  }
}

impl<T> CanBeFingerprinted for PhantomData<T> {
  fn fingerprint(&self, _: &mut Fingerprinter) {}
}

impl CanBeFingerprinted for Fingerprint {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
    fingerprinter.add(&self.0);
    fingerprinter.add(&self.1);
  }
}

impl<T: CanBeFingerprinted> CanBeFingerprinted for [T] {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
    for item in self {
      item.fingerprint(fingerprinter);
    }
  }
}

impl<T: CanBeFingerprinted> CanBeFingerprinted for &T {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
    (**self).fingerprint(fingerprinter);
  }
}
