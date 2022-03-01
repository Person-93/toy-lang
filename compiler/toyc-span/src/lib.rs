use std::{
  fmt::{self, Debug, Formatter},
  ops::Range,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Span {
  pub lo: BytePos,
  pub hi: BytePos,
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct BytePos(pub u32);

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

impl Debug for BytePos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self.0, f)
  }
}