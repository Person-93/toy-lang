use crate::{
  AnonConst, Body, BodyId, Expr, FieldDef, GenericParam, HirId, Item, Literal,
  NamedConst, Node, Package, Param, StructDef, TraitItem, Type,
};
use std::collections::HashMap;

pub struct HirContext<'hir> {
  arena: HirArena<'hir>,
  nodes: HashMap<HirId<'hir>, Node<'hir>>,
  bodies: Bodies<'hir>,
  package: &'hir Package<'hir>,
}

impl<'hir> HirContext<'hir> {
  pub fn new(
    arena: HirArena<'hir>,
    nodes: HashMap<HirId<'hir>, Node<'hir>>,
    bodies: Bodies<'hir>,
    package: &'hir Package<'hir>,
  ) -> HirContext<'hir> {
    HirContext {
      arena,
      nodes,
      bodies,
      package,
    }
  }

  pub fn root(&self) -> &Package<'hir> {
    self.package
  }

  pub fn nodes(&self) -> impl Iterator<Item = Node<'hir>> + '_ {
    self.nodes.values().copied()
  }

  pub fn items(&self) -> impl Iterator<Item = &Item<'hir>> + '_ {
    self.arena.items.iter()
  }

  pub fn field_defs(&self) -> impl Iterator<Item = &FieldDef<'hir>> + '_ {
    self.arena.field_defs.iter()
  }

  pub fn types(&self) -> impl Iterator<Item = &Type<'hir>> + '_ {
    self.arena.types.iter()
  }

  pub fn exprs(&self) -> impl Iterator<Item = &Expr<'hir>> + '_ {
    self.arena.exprs.iter()
  }

  pub fn constants(&self) -> impl Iterator<Item = &NamedConst<'hir>> + '_ {
    self.arena.consts.iter()
  }

  pub fn get_body(&self, body_id: BodyId<'hir>) -> Body<'hir> {
    self.bodies.get(body_id).unwrap()
  }
}

#[derive(Default)]
pub struct Bodies<'hir>(HashMap<HirId<'hir>, Body<'hir>>);

impl<'hir> Bodies<'hir> {
  pub fn insert(
    &mut self,
    expr: &'hir Expr<'hir>,
    params: &'hir [Param],
  ) -> BodyId<'hir> {
    let id = BodyId(expr.id);
    self.0.insert(id.0, Body { params, expr });
    id
  }

  pub fn get(&self, id: BodyId<'hir>) -> Option<Body<'hir>> {
    self.0.get(&id.0).copied()
  }
}

toyc_arena::declare_arena! {
  #[derive(Default)]
  pub struct HirArena<'hir> { _m: (), }
  Typed {
    pub items: Item<'hir>,
    pub field_defs: FieldDef<'hir>,
    pub types: Type<'hir>,
    pub exprs: Expr<'hir>,
    pub consts: NamedConst<'hir>,
  }
  dropless {
    GenericParam<'hir> AnonConst<'hir> Literal<'hir> StructDef<'hir>
    TraitItem<'hir> Package<'hir> Param<'hir>
  }
}
