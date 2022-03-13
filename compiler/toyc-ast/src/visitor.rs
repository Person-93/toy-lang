use crate::ast::{
  AssociatedConst, AssociatedType, AttrData, CodeBlock, ConstParam, Enum, Expr,
  ExprFragment, Function, FunctionParam, GenericParam, Item, ItemKind, Literal,
  Segment, Statement, Static, Struct, StructBody, StructField, Trait,
  TraitItem, TraitItemKind, TupleField, Type, TypeAlias, TypeParam, Variant,
  VisKind,
};
use toyc_span::symbol::{BoolLit, Ident, NumLit, StrLit};

pub trait Visitor {
  fn walk_package(&mut self, attrs: &[AttrData], items: &[Item]) {
    walk_package(self, attrs, items);
  }

  fn walk_attrs(&mut self, attrs: &[AttrData]) {
    walk_attrs(self, attrs);
  }

  fn walk_attr(&mut self, _attr: &AttrData) {}

  fn walk_mod(&mut self, attrs: &[AttrData], items: &[Item]) {
    walk_mod(self, attrs, items);
  }

  fn walk_static(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    static_: &Static,
  ) {
    walk_static(self, attrs, visibility, static_);
  }

  fn walk_function(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    function: &Function,
  ) {
    walk_function(self, attrs, visibility, function);
  }

  fn walk_extern(&mut self, _extern_: &StrLit) {}

  fn walk_struct(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    struct_: &Struct,
  ) {
    walk_struct(self, attrs, visibility, struct_);
  }

  fn walk_enum(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    enum_: &Enum,
  ) {
    walk_enum(self, attrs, visibility, enum_);
  }

  fn walk_trait(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    trait_: &Trait,
  ) {
    walk_trait(self, attrs, visibility, trait_);
  }

  fn walk_type_alias(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    type_alias: &TypeAlias,
  ) {
    walk_type_alias(self, attrs, visibility, type_alias)
  }

  fn walk_ident(&mut self, _ident: Ident) {}

  fn walk_generic_params(&mut self, generic_params: &[GenericParam]) {
    walk_generic_params(self, generic_params);
  }

  fn walk_generic_param(&mut self, generic_param: &GenericParam) {
    walk_generic_param(self, generic_param);
  }

  fn walk_param(&mut self, param: &FunctionParam) {
    walk_param(self, param)
  }

  fn walk_type(&mut self, type_: &Type) {
    walk_type(self, type_);
  }

  fn walk_segment(&mut self, segment: &Segment) {
    walk_segment(self, segment);
  }

  fn walk_struct_field(&mut self, struct_field: &StructField) {
    walk_struct_field(self, struct_field);
  }

  fn walk_tuple_field(&mut self, tuple_field: &TupleField) {
    walk_tuple_field(self, tuple_field);
  }

  fn walk_variant(&mut self, variant: &Variant) {
    walk_variant(self, variant);
  }

  fn walk_const_param(&mut self, const_param: &ConstParam) {
    walk_const_param(self, const_param);
  }

  fn walk_type_param(&mut self, type_param: &TypeParam) {
    walk_type_param(self, type_param);
  }

  fn walk_expr(&mut self, expr: &Expr) {
    walk_expr(self, expr);
  }

  fn walk_expr_fragment(&mut self, expr_fragment: &ExprFragment) {
    walk_expr_fragment(self, expr_fragment);
  }

  fn walk_code_block(&mut self, code_block: &CodeBlock) {
    walk_code_block(self, code_block);
  }

  fn walk_statement(&mut self, statement: &Statement) {
    walk_statement(self, statement);
  }

  fn walk_visibility(&mut self, _visibility: Option<&VisKind>) {}

  fn walk_associated_type(&mut self, associated_type: &AssociatedType) {
    walk_associated_type(self, associated_type);
  }

  fn walk_associated_const(&mut self, associated_const: &AssociatedConst) {
    walk_associated_const(self, associated_const);
  }

  fn walk_literal(&mut self, literal: &Literal) {
    walk_literal(self, literal);
  }

  fn walk_string_literal(&mut self, _str_lit: &StrLit) {}

  fn walk_numeric_literal(&mut self, _num_lit: &NumLit) {}

  fn walk_bool_literal(&mut self, _bool_lit: &BoolLit) {}
}

pub fn walk_package<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  items: &[Item],
) {
  package_or_mod(visitor, attrs, items);
}

pub fn walk_mod<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  items: &[Item],
) {
  package_or_mod(visitor, attrs, items);
}

fn package_or_mod<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  items: &[Item],
) {
  visitor.walk_attrs(attrs);
  for item in items {
    let Item {
      attrs,
      vis,
      item_kind,
      span: _,
    } = item;
    let vis = vis.as_ref().map(Option::as_ref);
    match item_kind {
      ItemKind::Static(static_) => visitor.walk_static(attrs, vis, static_),
      ItemKind::Function(function) => {
        visitor.walk_function(attrs, vis, function)
      }
      ItemKind::Struct(struct_) => visitor.walk_struct(attrs, vis, struct_),
      ItemKind::Enum(enum_) => visitor.walk_enum(attrs, vis, enum_),
      ItemKind::Trait(trait_) => visitor.walk_trait(attrs, vis, trait_),
      ItemKind::TypeAlias(type_alias) => {
        visitor.walk_type_alias(attrs, vis, type_alias)
      }
    }
  }
}

pub fn walk_attrs<V: Visitor + ?Sized>(visitor: &mut V, attrs: &[AttrData]) {
  for attr in attrs {
    visitor.walk_attr(attr);
  }
}

pub fn walk_attr<V: Visitor + ?Sized>(visitor: &mut V, attr: &AttrData) {
  visitor.walk_attr(attr);
}

pub fn walk_static<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  visibility: Option<Option<&VisKind>>,
  static_: &Static,
) {
  visitor.walk_attrs(attrs);
  if let Some(visibility) = visibility {
    visitor.walk_visibility(visibility);
  }
  let Static {
    static_kind: _,
    ident,
    type_,
    expr,
    span: _,
  } = static_;
  visitor.walk_ident(*ident);
  visitor.walk_type(type_);
  visitor.walk_expr(expr);
}

pub fn walk_function<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  visibility: Option<Option<&VisKind>>,
  function: &Function,
) {
  visitor.walk_attrs(attrs);
  if let Some(visibility) = visibility {
    visitor.walk_visibility(visibility);
  }
  let Function {
    extern_,
    const_: _,
    unsafe_: _,
    ident,
    generic_params,
    params,
    return_type,
    body,
    span: _,
  } = function;
  if let Some(extern_) = extern_ {
    visitor.walk_extern(extern_);
  }
  visitor.walk_ident(*ident);
  if let Some(generics) = generic_params {
    visitor.walk_generic_params(generics);
  }
  for param in params {
    let FunctionParam {
      attrs,
      ident,
      type_,
      span: _,
    } = param;
    visitor.walk_attrs(attrs);
    visitor.walk_ident(*ident);
    visitor.walk_type(type_);
  }
  if let Some(return_type) = return_type {
    visitor.walk_type(return_type);
  }
  if let Some(body) = body {
    visitor.walk_expr(body);
  }
}

pub fn walk_struct<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  visibility: Option<Option<&VisKind>>,
  struct_: &Struct,
) {
  visitor.walk_attrs(attrs);
  if let Some(visibility) = visibility {
    visitor.walk_visibility(visibility);
  }
  let Struct {
    ident,
    generic_params,
    fields,
    span: _,
  } = struct_;
  visitor.walk_ident(*ident);
  if let Some(generics) = generic_params {
    visitor.walk_generic_params(generics);
  }
  struct_body(visitor, fields);
}

pub fn walk_enum<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  visibility: Option<Option<&VisKind>>,
  enum_: &Enum,
) {
  visitor.walk_attrs(attrs);
  if let Some(visibility) = visibility {
    visitor.walk_visibility(visibility);
  }
  let Enum {
    attrs,
    ident,
    generic_params,
    variants,
    span: _,
  } = enum_;
  for attr in attrs {
    visitor.walk_attr(attr);
  }
  visitor.walk_ident(*ident);
  if let Some(generics) = generic_params {
    visitor.walk_generic_params(generics);
  }
  for variant in variants {
    visitor.walk_variant(variant);
  }
}

pub fn walk_trait<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  visibility: Option<Option<&VisKind>>,
  trait_: &Trait,
) {
  visitor.walk_attrs(attrs);
  if let Some(visibility) = visibility {
    visitor.walk_visibility(visibility);
  }
  let Trait {
    ident,
    generic_params,
    items,
    span: _,
  } = trait_;
  visitor.walk_ident(*ident);
  if let Some(generics) = generic_params {
    visitor.walk_generic_params(generics);
  }
  for item in items {
    let TraitItem {
      attrs,
      trait_item_kind,
      span: _,
    } = item;
    match trait_item_kind {
      TraitItemKind::Function(function) => {
        visitor.walk_function(attrs, None, function)
      }
      TraitItemKind::AssociatedType(type_) => {
        visitor.walk_associated_type(type_)
      }
      TraitItemKind::AssociatedConst(const_) => {
        visitor.walk_associated_const(const_)
      }
    }
  }
}

pub fn walk_type_alias<V: Visitor + ?Sized>(
  visitor: &mut V,
  attrs: &[AttrData],
  visibility: Option<Option<&VisKind>>,
  type_alias: &TypeAlias,
) {
  visitor.walk_attrs(attrs);
  if let Some(visibility) = visibility {
    visitor.walk_visibility(visibility);
  }
  let TypeAlias {
    ident,
    generic_params,
    type_,
    span: _,
  } = type_alias;
  visitor.walk_ident(*ident);
  if let Some(generic_params) = generic_params {
    visitor.walk_generic_params(generic_params);
  }
  visitor.walk_type(type_);
}

pub fn walk_generic_params<V: Visitor + ?Sized>(
  visitor: &mut V,
  generic_params: &[GenericParam],
) {
  for generic in generic_params {
    visitor.walk_generic_param(generic);
  }
}

pub fn walk_generic_param<V: Visitor + ?Sized>(
  visitor: &mut V,
  generic_param: &GenericParam,
) {
  match generic_param {
    GenericParam::ConstParam(param) => visitor.walk_const_param(param),
    GenericParam::TypeParam(type_param) => visitor.walk_type_param(type_param),
  }
}

pub fn walk_param<V: Visitor + ?Sized>(visitor: &mut V, param: &FunctionParam) {
  let FunctionParam {
    attrs,
    ident,
    type_,
    span: _,
  } = param;
  visitor.walk_attrs(attrs);
  visitor.walk_ident(*ident);
  visitor.walk_type(type_);
}

pub fn walk_type<V: Visitor + ?Sized>(visitor: &mut V, type_: &Type) {
  for segment in &type_.path {
    visitor.walk_segment(segment);
  }
}

pub fn walk_segment<V: Visitor + ?Sized>(visitor: &mut V, segment: &Segment) {
  visitor.walk_ident(segment.ident);
  if let Some(generics) = &segment.generics {
    visitor.walk_generic_params(generics);
  }
}

pub fn walk_struct_field<V: Visitor + ?Sized>(
  visitor: &mut V,
  struct_field: &StructField,
) {
  let StructField {
    attrs,
    vis,
    ident,
    type_,
    span: _,
  } = struct_field;
  visitor.walk_attrs(attrs);
  if let Some(vis) = vis {
    visitor.walk_visibility(vis.as_ref());
  }
  visitor.walk_ident(*ident);
  visitor.walk_type(type_);
}

pub fn walk_tuple_field<V: Visitor + ?Sized>(
  visitor: &mut V,
  tuple_field: &TupleField,
) {
  let TupleField {
    attrs,
    vis,
    type_,
    span: _,
  } = tuple_field;
  visitor.walk_attrs(attrs);
  if let Some(vis) = vis {
    visitor.walk_visibility(vis.as_ref());
  }
  visitor.walk_type(type_);
}

pub fn walk_variant<V: Visitor + ?Sized>(visitor: &mut V, variant: &Variant) {
  let Variant {
    attrs,
    ident,
    kind,
    span: _,
  } = variant;
  visitor.walk_attrs(attrs);
  visitor.walk_ident(*ident);
  struct_body(visitor, kind);
}

fn struct_body<V: Visitor + ?Sized>(visitor: &mut V, struct_body: &StructBody) {
  match struct_body {
    StructBody::NamedStruct(fields) => {
      for field in fields {
        visitor.walk_struct_field(field);
      }
    }
    StructBody::TupleStruct(fields) => {
      for field in fields {
        visitor.walk_tuple_field(field);
      }
    }
    StructBody::UnitStruct => {}
  }
}

pub fn walk_const_param<V: Visitor + ?Sized>(
  visitor: &mut V,
  const_param: &ConstParam,
) {
  let ConstParam {
    ident,
    type_,
    default,
    span: _,
  } = const_param;
  visitor.walk_ident(*ident);
  visitor.walk_type(type_);
  if let Some(default) = default {
    visitor.walk_expr(default);
  }
}

pub fn walk_type_param<V: Visitor + ?Sized>(
  visitor: &mut V,
  type_param: &TypeParam,
) {
  let TypeParam {
    ident,
    default,
    span: _,
  } = type_param;
  visitor.walk_ident(*ident);
  if let Some(default) = default {
    visitor.walk_type(default);
  }
}

pub fn walk_expr<V: Visitor + ?Sized>(visitor: &mut V, expr: &Expr) {
  let Expr {
    primary,
    args,
    span: _,
  } = expr;
  visitor.walk_expr_fragment(primary);
  if let Some(args) = args {
    for arg in args {
      visitor.walk_expr(arg);
    }
  }
}

pub fn walk_expr_fragment<V: Visitor + ?Sized>(
  visitor: &mut V,
  expr_fragment: &ExprFragment,
) {
  match expr_fragment {
    ExprFragment::CodeBlock(code_block) => visitor.walk_code_block(code_block),
    ExprFragment::Ident(ident) => visitor.walk_ident(*ident),
    ExprFragment::Literal(literal) => visitor.walk_literal(literal),
  }
}

pub fn walk_code_block<V: Visitor + ?Sized>(
  visitor: &mut V,
  code_block: &CodeBlock,
) {
  let CodeBlock {
    statements,
    trailing: expr,
    span: _,
  } = code_block;
  for statement in statements {
    visitor.walk_statement(statement);
  }
  if let Some(expr) = expr {
    visitor.walk_expr(expr);
  }
}

pub fn walk_statement<V: Visitor + ?Sized>(
  visitor: &mut V,
  statement: &Statement,
) {
  let Statement {
    attrs,
    expr,
    span: _,
  } = statement;
  visitor.walk_attrs(attrs);
  visitor.walk_expr(expr);
}

pub fn walk_associated_type<V: Visitor + ?Sized>(
  visitor: &mut V,
  associated_type: &AssociatedType,
) {
  let AssociatedType {
    ident,
    default,
    span: _,
  } = associated_type;
  visitor.walk_ident(*ident);
  if let Some(default) = default {
    visitor.walk_type(default);
  }
}

pub fn walk_associated_const<V: Visitor + ?Sized>(
  visitor: &mut V,
  associated_const: &AssociatedConst,
) {
  let AssociatedConst {
    ident,
    type_,
    default,
    span: _,
  } = associated_const;
  visitor.walk_ident(*ident);
  visitor.walk_type(type_);
  if let Some(default) = default {
    visitor.walk_expr(default);
  }
}

pub fn walk_literal<V: Visitor + ?Sized>(visitor: &mut V, literal: &Literal) {
  match literal {
    Literal::NumLit(lit) => visitor.walk_numeric_literal(lit),
    Literal::StrLit(lit) => visitor.walk_string_literal(lit),
    Literal::BoolLit(lit) => visitor.walk_bool_literal(lit),
  }
}
