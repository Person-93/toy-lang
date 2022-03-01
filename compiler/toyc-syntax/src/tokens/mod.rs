pub use self::generated::*;
use logos::Lexer;
use std::{num::ParseIntError, ops::Deref};

mod fmt;
mod generated;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ident(String);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StrLit(String);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NumLit {
  pub prefix: Option<NumLitPrefix>,
  pub val: i64,
  pub decimal: Option<u64>,
  pub ty: Option<NumLitType>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum NumLitPrefix {
  Binary,
  Hex,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum NumLitType {
  Int,
  Unsigned,
  Float,
}

impl StrLit {
  fn new(lex: &mut Lexer<Token>) -> Self {
    let slice = lex.slice();
    let slice = &slice[1..lex.slice().len() - 1];
    StrLit(String::from(slice))
  }

  pub fn take(self) -> String {
    self.0
  }
}

impl Ident {
  fn new(lex: &mut Lexer<Token>) -> Self {
    Ident(String::from(lex.slice()))
  }
}

impl AsRef<str> for Ident {
  #[inline]
  fn as_ref(&self) -> &str {
    &*self
  }
}

impl Deref for Ident {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl NumLit {
  fn new(lex: &mut Lexer<Token>) -> Result<Self, NumLitErr> {
    let slice = lex.slice();

    let prefix = if slice[0..=0] != *"0" {
      None
    } else {
      match &slice[1..=1] {
        "b" => Some(NumLitPrefix::Binary),
        "x" => Some(NumLitPrefix::Hex),
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => None,
        lit_type => return Err(NumLitErr::InvalidType(lit_type.chars().next().unwrap())),
      }
    };

    let dot_index = slice.find('.');

    let radix = match prefix {
      None => 10,
      Some(NumLitPrefix::Binary) => 2,
      Some(NumLitPrefix::Hex) => 16,
    };

    let (ty, ty_index) = match slice.find('i') {
      Some(i) => (Some(NumLitType::Int), Some(i)),
      None => match slice.find('u') {
        Some(i) => (Some(NumLitType::Unsigned), Some(i)),
        None => match slice.find('f') {
          Some(i) => (Some(NumLitType::Float), Some(i)),
          None => (None, None),
        },
      },
    };

    if let Some(ty) = &ty {
      if *ty != NumLitType::Float && dot_index.is_some() {
        return Err(NumLitErr::NonFloatWithPoint);
      }
    }

    let val_index: usize = if prefix.is_none() { 0 } else { 2 };
    let val = match (dot_index, ty_index) {
      (Some(end_index), _) | (None, Some(end_index)) => {
        i64::from_str_radix(&slice[val_index..end_index], radix)
      }
      (None, None) => i64::from_str_radix(&slice[val_index..], radix),
    }?;

    let decimal = match dot_index {
      Some(dot_index) => Some(u64::from_str_radix(&slice[dot_index + 1..], radix)?),
      None => None,
    };

    Ok(NumLit {
      prefix,
      val,
      decimal,
      ty,
    })
  }
}

pub enum NumLitErr {
  ParseInt(ParseIntError),
  InvalidType(char),
  NonFloatWithPoint,
}

impl From<ParseIntError> for NumLitErr {
  #[inline(always)]
  fn from(err: ParseIntError) -> Self {
    NumLitErr::ParseInt(err)
  }
}
