use std::{cell::RefCell, collections::HashMap};
use toyc_arena::Arena as _;
use toyc_ast::ast;
use toyc_hir::{
  Bodies, HirArena, HirContext, HirId, HirIdFactory, Node, Package,
};

pub struct LoweringContext<'hir> {
  pub(crate) nodes: RefCell<Nodes<'hir>>,
  pub(crate) bodies: RefCell<Bodies<'hir>>,
  pub(crate) arena: HirArena<'hir>,
  id_factory: HirIdFactory<'hir>,
  package: Option<&'hir Package<'hir>>,
}

impl<'hir> LoweringContext<'hir> {
  pub fn new(ast: &ast::File) -> LoweringContext<'hir> {
    let mut ctx = LoweringContext {
      nodes: Default::default(),
      bodies: Default::default(),
      arena: HirArena::default(),
      id_factory: Default::default(),
      package: None,
    };
    let package = ctx.lower_package(ast);
    ctx.package = Some(ctx.arena.alloc(package));
    ctx
  }

  pub(crate) fn next_id(&self) -> HirId<'hir> {
    self.id_factory.next_id()
  }
}

#[allow(clippy::from_over_into)]
impl<'hir> Into<HirContext<'hir>> for LoweringContext<'hir> {
  fn into(self) -> HirContext<'hir> {
    HirContext::new(
      self.arena,
      self.nodes.into_inner().0,
      self.bodies.into_inner(),
      self.package.unwrap(),
    )
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
