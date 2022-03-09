use std::collections::HashMap;
use toyc_arena::Arena as _;
use toyc_ast::ast;
use toyc_hir::{
  AnonConst, Attr, AttrData, AttrValue, Expr, FieldDef, GenericParam, HirId,
  HirIdFactory, Item, Literal, NamedConst, Node, Package, StructDef, TraitItem,
  Type,
};
use toyc_util::freezable::Freezable;

pub struct HirContext<'hir> {
  pub(crate) nodes: Freezable<Nodes<'hir>>,
  pub(crate) arena: Arena<'hir>,
  id_factory: HirIdFactory<'hir>,
  package: Option<&'hir Package<'hir>>,
}

impl<'hir> HirContext<'hir> {
  pub fn new(ast: &ast::File) -> HirContext<'hir> {
    let mut ctx = HirContext {
      nodes: Default::default(),
      arena: Arena {
        _m: (),
        items: Default::default(),
        field_defs: Default::default(),
        types: Default::default(),
        exprs: Default::default(),
        consts: Default::default(),
        dropless: Default::default(),
      },
      id_factory: Default::default(),
      package: None,
    };
    let package = ctx.lower_package(ast);
    ctx.package = Some(ctx.arena.alloc(package));
    ctx.nodes.freeze_in_place();
    ctx
  }

  pub fn root(&self) -> &Package<'hir> {
    self.package.unwrap()
  }

  pub fn nodes(&self) -> impl Iterator<Item = Node<'hir>> + '_ {
    self.nodes.as_frozen().0.values().copied()
  }

  pub(crate) fn next_id(&self) -> HirId<'hir> {
    self.id_factory.next_id()
  }
}

toyc_arena::declare_arena! {
  pub struct Arena<'hir> { _m: (), }
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

#[derive(Debug, Default)]
pub(crate) struct Nodes<'hir>(HashMap<HirId<'hir>, Node<'hir>>);

impl<'hir> Nodes<'hir> {
  pub(crate) fn insert<T>(&mut self, object: &'hir T) -> &'hir T
  where
    Node<'hir>: From<&'hir T>,
  {
    let node: Node = object.into();
    self.0.insert(node.id(), node);
    object
  }

  pub(crate) fn insert_slice<T>(&mut self, objects: &'hir [T]) -> &'hir [T]
  where
    Node<'hir>: From<&'hir T>,
  {
    self.0.reserve(objects.len());
    for object in objects {
      let node: Node = object.into();
      self.0.insert(node.id(), node);
    }
    objects
  }
}
