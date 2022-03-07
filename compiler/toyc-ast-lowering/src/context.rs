use toyc_arena::Arena;
use toyc_ast::ast;
use toyc_hir::{
  AnonConst, Attr, AttrData, AttrValue, Expr, FieldDef, GenericParam, HirId,
  HirIdFactory, Item, Literal, NamedConst, Package, StructDef, TraitItem, Type,
};
use toyc_session::Session;

toyc_arena::declare_arena! {
  pub struct HirContext<'hir> {
    pub(crate) session: &'hir Session,
    id_factory: HirIdFactory<'hir>,
    package: Option<&'hir Package<'hir>>,
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
    Literal<'hir> StructDef<'hir> TraitItem<'hir> Package<'hir>
  }
}

impl<'hir> HirContext<'hir> {
  pub fn new(session: &'hir Session, ast: &ast::File) -> HirContext<'hir> {
    let mut ctx = HirContext {
      session,
      id_factory: Default::default(),
      package: None,
      items: Default::default(),
      field_defs: Default::default(),
      types: Default::default(),
      exprs: Default::default(),
      consts: Default::default(),
      dropless: Default::default(),
    };
    ctx.package = Some(ctx.alloc(ctx.lower_package(ast)));
    ctx
  }

  pub fn root(&self) -> &Package<'hir> {
    self.package.as_ref().unwrap()
  }

  pub(crate) fn next_id(&self) -> HirId<'hir> {
    self.id_factory.next_id()
  }
}
