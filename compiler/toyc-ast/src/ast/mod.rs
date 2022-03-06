// TODO remove the allow attribute from ast::generated
#[allow(clippy::enum_variant_names, clippy::type_complexity)]
mod generated;

pub use generated::*;
use toyc_span::Span;

impl GenericParam {
  pub fn span(&self) -> Span {
    match self {
      GenericParam::ConstParam(ConstParam { span, .. })
      | GenericParam::TypeParam(TypeParam { span, .. }) => *span,
    }
  }
}
