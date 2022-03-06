use toyc_hir::{
  AnonConst, Attr, AttrData, AttrValue, Expr, FieldDef, GenericParam, HirId,
  HirIdFactory, Item, Literal, NamedConst, StructDef, TraitItem, Type,
};
use toyc_session::Session;

toyc_arena::declare_arena! {
  pub struct HirContext<'hir> {
    pub(crate) session: &'hir Session,
    id_factory: HirIdFactory<'hir>,
  }
  Typed {
    pub items: Item<'hir>,
    pub field_defs: FieldDef<'hir>,
    pub types: Type<'hir>,
    pub exprs: Expr<'hir>,
    pub consts: NamedConst<'hir>,
  }
  dropless {
    GenericParam<'hir> AnonConst<'hir> Attr<'hir> AttrData<'hir> AttrValue<'hir>
    Literal<'hir> StructDef<'hir> TraitItem<'hir>
  }
}

impl<'hir> HirContext<'hir> {
  pub fn new(session: &'hir Session) -> HirContext<'hir> {
    HirContext {
      session,
      id_factory: Default::default(),
      items: Default::default(),
      field_defs: Default::default(),
      types: Default::default(),
      exprs: Default::default(),
      consts: Default::default(),
      dropless: Default::default(),
    }
  }

  pub(crate) fn next_id(&self) -> HirId<'hir> {
    self.id_factory.next_id()
  }
}
