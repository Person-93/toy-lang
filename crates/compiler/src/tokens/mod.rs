pub use self::generated::Token;
use logos::Lexer;
#[cfg(test)]
use serde::{Deserialize, Serialize};
use std::num::ParseIntError;

mod fmt;
mod generated;
#[cfg(test)]
mod tests;

pub trait StaticToken {
  fn name() -> &'static str;
  fn symbol() -> &'static str;
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(Serialize, Deserialize))]
pub struct StrLit(String);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(Serialize, Deserialize))]
pub struct BoolLit(bool);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(Serialize, Deserialize))]
pub struct NumLit {
  prefix: Option<NumLitPrefix>,
  val: i64,
  decimal: Option<u64>,
  ty: Option<NumLitType>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(Serialize, Deserialize))]
enum NumLitPrefix {
  Binary,
  Hex,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(Serialize, Deserialize))]
enum NumLitType {
  Int,
  Unsigned,
  Float,
}

fn num_lit(lex: &mut Lexer<Token>) -> Result<NumLit, NumLitErr> {
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

enum NumLitErr {
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
