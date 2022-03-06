// TODO make toyc-hir no_std?

pub use self::{hir::*, hir_id::*};
use std::collections::HashMap;

mod hir;
mod hir_id;

toyc_arena::declare_arena! {
  #[derive(Default)]
  pub struct HirContext<'hir> {
    bodies: HashMap<HirId<'hir>, &'hir Expr<'hir>>,
  }
  Typed {
    pub items: Item<'hir>,
    pub field_defs: FieldDef<'hir>,
    pub types: Type<'hir>,
    pub exprs: Expr<'hir>,
    pub consts: NamedConst<'hir>,
  }
  dropless {
    GenericParams<'hir> GenericParam<'hir> AnonConst<'hir> Attr<'hir> AttrData<'hir>
    AttrValue<'hir>
  }
}

impl<'hir> HirContext<'hir> {
  pub fn bodies(
    &'hir self,
  ) -> impl Iterator<Item = (BodyId, &'hir Expr<'hir>)> + 'hir {
    self
      .bodies
      .iter()
      .map(|(id, expr)| (id.to_body_id(), *expr))
  }
}
