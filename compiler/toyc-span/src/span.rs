use core::{
  fmt::{self, Debug, Display, Formatter},
  ops::Range,
};
use serde::Serialize;
use toyc_macros::CanBeFingerprinted;

#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, CanBeFingerprinted,
)]
pub struct Span {
  pub lo: BytePos,
  pub hi: BytePos,
}

impl Span {
  pub const DUMMY: Span = Span {
    lo: BytePos(u32::MAX),
    hi: BytePos(u32::MAX),
  };

  pub const fn shrink_to_lo(mut self) -> Self {
    self.hi = self.lo;
    self
  }

  pub const fn shrink_to_hi(mut self) -> Self {
    self.lo = self.hi;
    self
  }

  pub const fn is_dummy(self) -> bool {
    self.lo.0 == u32::MAX && self.hi.0 == u32::MAX
  }

  pub const fn combine(self, other: Span) -> Span {
    Span {
      lo: min(self.lo, other.lo),
      hi: max(self.hi, other.hi),
    }
  }
}

impl Display for Span {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}..{}", self.lo, self.hi)
  }
}

impl chumsky::Span for Span {
  type Context = ();
  type Offset = BytePos;

  fn new(_: Self::Context, range: Range<Self::Offset>) -> Self {
    Self {
      lo: range.start,
      hi: range.end,
    }
  }

  fn context(&self) -> Self::Context {}

  fn start(&self) -> Self::Offset {
    self.lo
  }

  fn end(&self) -> Self::Offset {
    self.hi
  }
}

#[derive(
  Copy,
  Clone,
  Ord,
  PartialOrd,
  Eq,
  PartialEq,
  Hash,
  Serialize,
  CanBeFingerprinted,
)]
#[serde(transparent)]
pub struct BytePos(pub u32);

const fn min(a: BytePos, b: BytePos) -> BytePos {
  if a.0 < b.0 {
    a
  } else {
    b
  }
}

const fn max(a: BytePos, b: BytePos) -> BytePos {
  if a.0 > b.0 {
    a
  } else {
    b
  }
}

impl Display for BytePos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.0, f)
  }
}

impl Debug for BytePos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self.0, f)
  }
}
