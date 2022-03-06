pub use self::generated::*;
use chumsky::Span as S;
use core::num::ParseIntError;
use logos::{Lexer, Logos};
pub(crate) use toyc_span::{
  symbol::{
    BoolLit, Ident, InvalidIdent, NumLit, NumLitPrefix, NumLitType, StrLit,
    Symbol,
  },
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
  let span = lexer.span();
  StrLit {
    value: Symbol::new(slice),
    span: Span {
      lo: BytePos(span.start as u32),
      hi: BytePos(span.end as u32),
    },
  }
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

  let (ty, ty_index) = match slice
    .chars()
    .enumerate()
    .find(|(_, c)| *c == 'i' || *c == 'u' || *c == 'f')
    .map(|(idx, ty)| -> Result<_, ParseIntError> {
      let bits = slice[idx + 1..].parse()?;
      Ok((
        match ty {
          'i' => NumLitType::Int(bits),
          'u' => NumLitType::Unsigned(bits),
          'f' => NumLitType::Float(bits),
          _ => unreachable!(),
        },
        idx,
      ))
    }) {
    Some(data) => {
      let (idx, ty) = data?;
      (Some(idx), Some(ty))
    }
    None => (None, None),
  };

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

  let span = lexer.span();

  Ok(NumLit {
    prefix,
    val,
    decimal,
    ty,
    span: Span {
      lo: BytePos(span.start as u32),
      hi: BytePos(span.end as u32),
    },
  })
}

fn make_bool_lit(lexer: &mut Lexer<Token>) -> BoolLit {
  BoolLit {
    value: match lexer.slice() {
      "true" => true,
      "false" => false,
      _ => unreachable!(),
    },
    span: {
      let span = lexer.span();
      Span {
        lo: BytePos(span.start as u32),
        hi: BytePos(span.end as u32),
      }
    },
  }
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
