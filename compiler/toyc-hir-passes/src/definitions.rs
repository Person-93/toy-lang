use alloc::rc::{Rc, Weak};
use hashbrown::HashMap;
use toyc_hir::{
  EnumDef, Expr, Function, HirContext, Item, NamedConst, StructDef, Trait,
  Type, TypeAlias,
};
use toyc_span::symbol::Ident;

#[derive(Clone, Debug)]
pub struct Definitions<'hir: 'a, 'a> {
  pub(crate) defs: HashMap<Ident, Definition<'hir, 'a>>,
  pub(crate) parent_scope: Weak<Definitions<'hir, 'a>>,
}

impl<'hir: 'a, 'a> Definitions<'hir, 'a> {
  pub fn new(context: &'a HirContext<'hir>) -> Rc<Self> {
    Definitions::from_items(context.root().items, Weak::new())
  }

  fn from_items(
    items: &'hir [Item<'hir>],
    parent_scope: Weak<Definitions<'hir, 'a>>,
  ) -> Rc<Self> {
    let mut definitions = Rc::new(Definitions {
      defs: Default::default(),
      parent_scope,
    });

    for item in items {
      match item {
        Item::Mod(mod_) => {
          Rc::get_mut(&mut definitions)
            .unwrap()
            .defs
            .entry(mod_.ident)
            .or_default()
            .mod_ = Some(Definitions::from_items(
            mod_.items,
            Rc::downgrade(&definitions),
          ))
        }
        Item::Function(function) => {
          Rc::get_mut(&mut definitions)
            .unwrap()
            .defs
            .entry(function.ident)
            .or_default()
            .function = Some(function)
        }
        Item::Enum(enum_) => {
          Rc::get_mut(&mut definitions)
            .unwrap()
            .defs
            .entry(enum_.ident)
            .or_default()
            .user_type = Some(UserType::Enum(enum_))
        }
        Item::Struct(struct_) => {
          Rc::get_mut(&mut definitions)
            .unwrap()
            .defs
            .entry(struct_.ident)
            .or_default()
            .user_type = Some(UserType::Struct(struct_))
        }
        Item::Trait(trait_) => {
          Rc::get_mut(&mut definitions)
            .unwrap()
            .defs
            .entry(trait_.ident)
            .or_default()
            .user_type = Some(UserType::Trait(trait_))
        }
        Item::TypeAlias(type_alias) => {
          Rc::get_mut(&mut definitions)
            .unwrap()
            .defs
            .entry(type_alias.ident)
            .or_default()
            .user_type = Some(UserType::TypeAlias(type_alias))
        }
      }
    }
    definitions
  }
}

#[derive(Clone, Debug, Default)]
pub struct Definition<'hir: 'a, 'a> {
  pub function: Option<&'a Function<'hir>>,
  pub user_type: Option<UserType<'hir, 'a>>,
  pub constant: Option<Constant<'hir, 'a>>,
  pub binding: Option<Binding<'hir, 'a>>,
  pub mod_: Option<Rc<Definitions<'hir, 'a>>>,
}

#[derive(Copy, Clone, Debug)]
pub enum UserType<'hir: 'a, 'a> {
  Struct(&'a StructDef<'hir>),
  Enum(&'a EnumDef<'hir>),
  Trait(&'a Trait<'hir>),
  TypeAlias(&'a TypeAlias<'hir>),
  GenericParam,
}

#[derive(Copy, Clone, Debug)]
pub enum Constant<'hir: 'a, 'a> {
  Named(&'a NamedConst<'hir>),
  GenericPAram,
}

#[derive(Copy, Clone, Debug)]
pub enum Binding<'hir: 'a, 'a> {
  Param(&'a Type<'hir>),
  Expr(&'a Expr<'hir>),
}
