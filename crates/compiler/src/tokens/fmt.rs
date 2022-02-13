use crate::tokens::{NumLitPrefix, NumLitType};
use std::fmt::{Display, Formatter};

impl Display for super::StrLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "\"{}\"", self.0)
  }
}

impl Display for super::BoolLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", if self.0 { "true" } else { "false" })
  }
}

impl Display for super::NumLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let super::NumLit {
      prefix,
      val,
      decimal,
      ty,
    } = self;

    let prefix = match prefix {
      None => "",
      Some(NumLitPrefix::Hex) => "0x",
      Some(NumLitPrefix::Binary) => "0b",
    };

    let decimal = match decimal {
      None => String::new(),
      Some(n) => format!("{n}"),
    };

    let ty = match ty {
      None => "",
      Some(NumLitType::Float) => "f",
      Some(NumLitType::Int) => "i",
      Some(NumLitType::Unsigned) => "u",
    };

    write!(f, "{prefix}{val}{decimal}{ty}")
  }
}
