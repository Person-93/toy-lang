pub use self::generated::*;
use chumsky::Span as S;
use logos::{Lexer, Logos};
use std::{
  fmt::{self, Debug, Display, Formatter},
  num::ParseIntError,
};
pub(crate) use toyc_span::{
  symbol::{Ident, InvalidIdent, Symbol},
  BytePos, Span,
};

mod generated;

fn make_ident(lexer: &mut Lexer<Token>) -> Result<Ident, InvalidIdent> {
  let span = lexer.span();
  Ident::from_string_and_span(
    lexer.slice(),
    Span {
      lo: BytePos(span.start as u32),
      hi: BytePos(span.end as u32),
    },
  )
}

fn make_str_lit(lexer: &mut Lexer<Token>) -> StrLit {
  let slice = lexer.slice();
  let slice = &slice[1..lexer.slice().len() - 1];
  StrLit(Symbol::new(slice))
}

fn make_num_lit(lexer: &mut Lexer<Token>) -> Result<NumLit, NumLitErr> {
  let slice = lexer.slice();

  let prefix = if slice.len() < 3 || slice[0..=0] != *"0" {
    None
  } else {
    match &slice[1..=1] {
      "b" => Some(NumLitPrefix::Binary),
      "x" => Some(NumLitPrefix::Hex),
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => None,
      lit_type => {
        return Err(NumLitErr::InvalidType(lit_type.chars().next().unwrap()))
      }
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
      i32::from_str_radix(&slice[val_index..end_index], radix)
    }
    (None, None) => i32::from_str_radix(&slice[val_index..], radix),
  }?;

  let decimal = match dot_index {
    Some(dot_index) => {
      Some(u16::from_str_radix(&slice[dot_index + 1..], radix)?)
    }
    None => None,
  };

  Ok(NumLit {
    prefix,
    val,
    decimal,
    ty,
  })
}

pub struct TokenIter<'source>(logos::SpannedIter<'source, Token>, Span);

impl<'source> TokenIter<'source> {
  pub fn new(text: &'source str) -> Self {
    TokenIter(
      Token::lexer(text).spanned(),
      Span::new((), BytePos(text.len() as u32)..BytePos(text.len() as u32)),
    )
  }

  pub fn into_stream(
    self,
  ) -> chumsky::Stream<'source, Token, Span, TokenIter<'source>> {
    chumsky::Stream::from_iter(self.1, self)
  }
}

impl Iterator for TokenIter<'_> {
  type Item = (Token, Span);

  fn next(&mut self) -> Option<Self::Item> {
    self.0.next().map(|(token, span)| {
      (
        token,
        Span {
          lo: BytePos(span.start as u32),
          hi: BytePos(span.end as u32),
        },
      )
    })
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct StrLit(pub Symbol);

impl<T: AsRef<str> + ?Sized> PartialEq<T> for StrLit {
  fn eq(&self, other: &T) -> bool {
    *self.0 == *other.as_ref()
  }
}

impl Display for StrLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.0, f)
  }
}

impl Debug for StrLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "\"{self}\"")
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NumLit {
  pub prefix: Option<NumLitPrefix>,
  pub val: i32,
  pub decimal: Option<u16>,
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

impl Display for NumLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let NumLit {
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
