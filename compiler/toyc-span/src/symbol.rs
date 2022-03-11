use crate::Span;
use alloc::{borrow::ToOwned, boxed::Box, string::ToString};
use core::{
  fmt::{self, Debug, Display, Formatter},
  ops::Deref,
};
use internment::Intern;
use toyc_data_structures::fingerprint::{CanBeFingerprinted, Fingerprinter};
use unicode_xid::UnicodeXID;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
  pub name: Symbol,
  pub span: Span,
}

impl Ident {
  pub fn from_string_and_span(
    name: &str,
    span: Span,
  ) -> Result<Ident, InvalidIdent> {
    let name = name.trim();
    if name.is_empty() {
      Err(InvalidIdent::Empty)
    } else {
      let mut chars = name.chars();
      let name = Symbol::new(name);
      if !chars.next().unwrap().is_xid_start() {
        return Err(InvalidIdent::Invalid(name));
      }
      for c in chars {
        if !c.is_xid_continue() {
          return Err(InvalidIdent::Invalid(name));
        }
      }
      Ok(Ident { name, span })
    }
  }

  pub fn synthesize_number(n: usize) -> Ident {
    Ident {
      name: Symbol::new(&n.to_string()),
      span: Span::DUMMY,
    }
  }
}

impl Display for Ident {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.name, f)
  }
}

impl Debug for Ident {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{self} @ {}", self.span)
  }
}

#[derive(Copy, Clone, Debug)]
pub enum InvalidIdent {
  Empty,
  Invalid(Symbol),
}

impl Display for InvalidIdent {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      InvalidIdent::Empty => write!(f, "identifier must not be empty"),
      InvalidIdent::Invalid(symbol) => write!(f, "invalid ident `{symbol}`"),
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NumLit {
  pub prefix: Option<NumLitPrefix>,
  pub val: i32,
  pub decimal: Option<u16>,
  pub ty: Option<NumLitType>,
  pub span: Span,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum NumLitPrefix {
  Binary,
  Hex,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum NumLitType {
  Int(u16),
  Unsigned(u16),
  Float(u16),
}

impl Display for NumLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let NumLit {
      prefix,
      val,
      decimal,
      ty,
      span: _,
    } = self;

    match prefix {
      None => (),
      Some(NumLitPrefix::Hex) => write!(f, "0x")?,
      Some(NumLitPrefix::Binary) => write!(f, "0b")?,
    };

    write!(f, "{val}")?;

    if let Some(decimal) = decimal {
      write!(f, ".{decimal}")?;
    }

    if let Some(ty) = ty {
      match ty {
        NumLitType::Int(bits) => write!(f, "i{bits}"),
        NumLitType::Unsigned(bits) => write!(f, "u{bits}"),
        NumLitType::Float(bits) => write!(f, "f{bits}"),
      }?;
    }

    Ok(())
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct StrLit {
  pub value: Symbol,
  pub span: Span,
}

impl StrLit {
  pub fn new(value: &str, span: Span) -> StrLit {
    StrLit {
      value: Symbol::new(value),
      span,
    }
  }
}

impl<T: AsRef<str> + ?Sized> PartialEq<T> for StrLit {
  fn eq(&self, other: &T) -> bool {
    *self.value == *other.as_ref()
  }
}

impl Display for StrLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.value, f)
  }
}

impl Debug for StrLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "\"{self}\"")
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct BoolLit {
  pub value: bool,
  pub span: Span,
}

impl Display for BoolLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if self.value {
      write!(f, "true")
    } else {
      write!(f, "false")
    }
  }
}

impl Debug for BoolLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{self}")
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Symbol(internment::Intern<&'static str>);

impl Symbol {
  pub fn new(s: &str) -> Symbol {
    Symbol(Intern::new(Box::leak(s.to_owned().into_boxed_str())))
  }
}

impl CanBeFingerprinted for Symbol {
  fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
    fingerprinter.add(self.0.as_ref());
  }
}

impl Display for Symbol {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(self.deref(), f)
  }
}

impl Debug for Symbol {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(self, f)
  }
}

impl Deref for Symbol {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    &**self.0
  }
}
