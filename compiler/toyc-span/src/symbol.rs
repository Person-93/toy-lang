use crate::Span;
use internment::Intern;
use std::{
  fmt::{self, Debug, Display, Formatter},
  ops::Deref,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
  pub name: Symbol,
  pub span: Span,
}

impl Ident {
  pub fn from_string_and_span(name: &str, span: Span) -> Ident {
    Ident {
      name: Symbol(Intern::new(name.to_owned())),
      span,
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

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct StrLit {
  pub value: Symbol,
  pub span: Span,
}

impl StrLit {
  pub fn new(value: &str, span: Span) -> StrLit {
    StrLit {
      value: Symbol(Intern::new(value.to_owned())),
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
pub struct Symbol(internment::Intern<String>);

impl Symbol {
  pub fn new(s: &str) -> Symbol {
    Symbol(Intern::new(s.to_owned()))
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
