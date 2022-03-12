// TODO remove the allow attribute from ast::generated
#[allow(clippy::enum_variant_names, clippy::type_complexity)]
mod generated;

pub use generated::*;
use toyc_span::{
  symbol::{BoolLit, Ident, NumLit, StrLit},
  Span,
};

impl GenericParam {
  pub fn span(&self) -> Span {
    match self {
      GenericParam::ConstParam(ConstParam { span, .. })
      | GenericParam::TypeParam(TypeParam { span, .. }) => *span,
    }
  }
}

impl Literal {
  pub fn span(&self) -> Span {
    match self {
      Literal::NumLit(NumLit { span, .. })
      | Literal::StrLit(StrLit { span, .. })
      | Literal::BoolLit(BoolLit { span, .. }) => *span,
    }
  }
}

impl Type {
  pub fn as_ident(&self) -> Option<Ident> {
    if self.path.len() == 1 {
      let segment = self.path.first().unwrap();
      if segment.generics.is_none() {
        Some(segment.ident)
      } else {
        None
      }
    } else {
      None
    }
  }
}
