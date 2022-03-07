pub use self::context::HirContext;
use core::fmt::{self, Display, Formatter};
use std::error::Error;
use toyc_arena::Arena;
use toyc_ast::ast;
use toyc_hir::{
  Abi, AnonConst, Attr, AttrData, AttrKind, AttrValue, Constness, EnumDef,
  Expr, ExprKind, Extern, FieldDef, FloatBits, FnRetTy, FnType, Function,
  GenericParam, GenericParamKind, GenericParams, IntBits, Item, LitKind,
  Literal, MutType, Mutability, NumLit, NumLitRadix, NumLitType, Package,
  PrimitiveType, SelfKind, StructDef, StructDefKind, Trait, TraitConst,
  TraitItem, TraitType, Type, TypeAlias, TypeKind, Unsafety, Visibility,
};
use toyc_span::symbol::{self, Ident};

mod context;

impl<'hir> HirContext<'hir> {
  #[must_use]
  fn lower_package(&self, root: &ast::File) -> Package<'hir> {
    Package {
      attrs: self
        .alloc_iter(root.attrs.iter().map(|attr| self.lower_attr(attr))),
      items: self
        .alloc_iter(root.items.iter().map(|item| self.lower_item(item))),
      span: root.span,
    }
  }

  #[must_use]
  fn lower_attr(&self, attr: &ast::AttrData) -> Attr<'hir> {
    Attr::from_data(self.lower_attr_data(attr))
  }

  #[must_use]
  fn lower_attr_data(&self, attr_data: &ast::AttrData) -> AttrData<'hir> {
    AttrData {
      id: self.next_id(),
      ident: attr_data.ident,
      kind: match &attr_data.kind {
        None => AttrKind::Plain,
        Some(ast::AttrDataKind::Assign(literal)) => {
          AttrKind::Assign(self.alloc(self.lower_literal(literal)))
        }
        Some(ast::AttrDataKind::Call(args)) => AttrKind::Call(self.alloc_iter(
          args.iter().map(|value| self.lower_attr_call_value(value)),
        )),
      },
      span: attr_data.span,
    }
  }

  #[must_use]
  fn lower_attr_call_value(
    &self,
    value: &ast::AttrCallValue,
  ) -> AttrValue<'hir> {
    match value {
      ast::AttrCallValue::Literal(literal) => {
        AttrValue::Lit(self.alloc(self.lower_literal(literal)))
      }
      ast::AttrCallValue::Nested(inner) => {
        AttrValue::Nested(self.alloc(self.lower_attr_data(inner)))
      }
    }
  }

  #[must_use]
  fn lower_item(&self, item: &ast::Item) -> Item<'hir> {
    match &item.item_kind {
      ast::ItemKind::Function(f) => {
        Item::Function(self.lower_function(f, &item.vis))
      }
      ast::ItemKind::Struct(s) => Item::Struct(self.lower_struct(s, &item.vis)),
      ast::ItemKind::Enum(e) => Item::Enum(self.lower_enum(e, &item.vis)),
      ast::ItemKind::Trait(t) => Item::Trait(self.lower_trait(t, &item.vis)),
      ast::ItemKind::TypeAlias(ta) => {
        Item::TypeAlias(self.lower_type_alias(ta, &item.vis))
      }
    }
  }

  #[must_use]
  fn lower_function(
    &self,
    function: &ast::Function,
    vis: &AstVis,
  ) -> Function<'hir> {
    Function {
      id: self.next_id(),
      ident: function.ident,
      visibility: convert_visibility(vis),
      fn_type: FnType {
        extern_: function.extern_.map_or(Extern::No, |ext| {
          Extern::Yes(match &*ext.value {
            "toy" => Abi::Toy,
            "C" => Abi::C,
            _ => Abi::Err(ext),
          })
        }),
        unsafety: Unsafety::No,
        constness: Constness::No,
        generics: function
          .generic_params
          .as_ref()
          .map(|g| self.lower_generic_params(g)),
        inputs: self.alloc_iter(
          function
            .params
            .iter()
            .map(|param| self.lower_type(&param.type_)),
        ),
        output: function
          .return_type
          .as_ref()
          .map_or(FnRetTy::Default, |ty| {
            FnRetTy::Explicit(self.alloc(self.lower_type(ty)))
          }),
        self_kind: SelfKind::None,
      },
      body_id: function
        .body
        .as_ref()
        .map(|expr| self.lower_expr(expr).id.to_body_id()),
      span: function.span,
    }
  }

  #[must_use]
  fn lower_struct(
    &self,
    struct_: &ast::Struct,
    vis: &AstVis,
  ) -> StructDef<'hir> {
    StructDef {
      id: self.next_id(),
      ident: struct_.ident,
      vis: convert_visibility(vis),
      generics: struct_
        .generic_params
        .as_ref()
        .map(|generics| self.lower_generic_params(generics)),
      kind: self.lower_struct_fields(&struct_.fields),
      span: struct_.span,
    }
  }

  #[must_use]
  fn lower_enum(&self, enum_: &ast::Enum, vis: &AstVis) -> EnumDef<'hir> {
    EnumDef {
      id: self.next_id(),
      ident: enum_.ident,
      vis: convert_visibility(vis),
      generics: enum_
        .generic_params
        .as_ref()
        .map(|g| self.lower_generic_params(g)),
      def: self.alloc_iter(enum_.variants.iter().map(|variant| StructDef {
        id: self.next_id(),
        ident: variant.ident,
        vis: Visibility::Inherited,
        generics: None,
        kind: self.lower_struct_fields(&variant.kind),
        span: variant.span,
      })),
      span: enum_.span,
    }
  }

  #[must_use]
  fn lower_struct_fields(
    &self,
    fields: &ast::StructBody,
  ) -> StructDefKind<'hir> {
    match fields {
      ast::StructBody::NamedStruct(fields) => {
        StructDefKind::Struct(self.alloc_iter(fields.iter().map(|field| {
          FieldDef {
            id: self.next_id(),
            ident: field.ident,
            type_: self.alloc(self.lower_type(&field.type_)),
            vis: convert_visibility(&field.vis),
            span: field.span,
          }
        })))
      }
      ast::StructBody::TupleStruct(fields) => {
        StructDefKind::Tuple(self.alloc_iter(fields.iter().enumerate().map(
          |(idx, field)| FieldDef {
            id: self.next_id(),
            ident: Ident::synthesize_number(idx),
            type_: self.alloc(self.lower_type(&field.type_)),
            vis: convert_visibility(&field.vis),
            span: field.span,
          },
        )))
      }
      ast::StructBody::UnitStruct => StructDefKind::Unit,
    }
  }

  #[must_use]
  fn lower_trait(&self, trait_: &ast::Trait, vis: &AstVis) -> Trait<'hir> {
    Trait {
      id: self.next_id(),
      ident: trait_.ident,
      visibility: convert_visibility(vis),
      generics: trait_
        .generic_params
        .as_ref()
        .map(|g| self.lower_generic_params(g)),
      items: self.alloc_iter(trait_.items.iter().map(|item| {
        match &item.trait_item_kind {
          ast::TraitItemKind::Function(f) => {
            TraitItem::Function(self.lower_function(f, &None))
          }
          ast::TraitItemKind::AssociatedType(t) => TraitItem::Type(TraitType {
            id: self.next_id(),
            ident: t.ident,
            default: t
              .default
              .as_ref()
              .map(|t| &*self.alloc(self.lower_type(t))),
            span: t.span,
          }),
          ast::TraitItemKind::AssociatedConst(c) => {
            TraitItem::Const(TraitConst {
              id: self.next_id(),
              ident: c.ident,
              default: c.default.as_ref().map(|c| AnonConst {
                body: self.lower_expr(c).id.to_body_id(),
              }),
              span: c.span,
            })
          }
        }
      })),
      span: trait_.span,
    }
  }

  #[must_use]
  fn lower_type_alias(
    &self,
    type_alias: &ast::TypeAlias,
    vis: &AstVis,
  ) -> TypeAlias<'hir> {
    TypeAlias {
      id: self.next_id(),
      ident: type_alias.ident,
      visibility: convert_visibility(vis),
      generics: type_alias
        .generic_params
        .as_ref()
        .map(|g| self.lower_generic_params(g)),
      type_: self.alloc(self.lower_type(&type_alias.type_)),
      span: type_alias.span,
    }
  }

  #[must_use]
  #[allow(clippy::mut_from_ref)]
  fn lower_expr(&self, expr: &ast::Expr) -> Expr<'hir> {
    let (kind, span) = match &expr.primary {
      ast::ExprFragment::CodeBlock(codeblock) => (
        ExprKind::CodeBlock {
          unsafety: Unsafety::No,
          constness: Constness::No,
          statements: self.alloc_iter(
            codeblock
              .statements
              .iter()
              .map(|statement| self.lower_expr(&statement.expr)),
          ),
          trailing: codeblock
            .trailing
            .as_ref()
            .map(|trailing| &*self.alloc(self.lower_expr(trailing))),
        },
        codeblock.span,
      ),
      // TODO how to tell if ExprKind should be an ident or a type?
      ast::ExprFragment::Ident(ident) => (ExprKind::Ident(*ident), ident.span),
      ast::ExprFragment::Literal(literal) => (
        ExprKind::Lit(self.alloc(self.lower_literal(literal))),
        literal.span(),
      ),
    };
    let primary = Expr {
      id: self.next_id(),
      kind,
      span,
    };
    match &expr.args {
      None => primary,
      Some(args) => Expr {
        id: self.next_id(),
        kind: ExprKind::Call {
          callee: self.alloc(primary),
          args: self.alloc_iter(args.iter().map(|arg| self.lower_expr(arg))),
        },
        span: expr.span,
      },
    }
  }

  #[must_use]
  fn lower_generic_params(
    &self,
    generics: &[ast::GenericParam],
  ) -> GenericParams<'hir> {
    GenericParams {
      id: self.next_id(),
      params: self
        .alloc_iter(generics.iter().map(|g| self.lower_generic_param(g))),
      span: generics
        .first()
        .unwrap()
        .span()
        .combine(generics.last().unwrap().span()),
    }
  }

  #[must_use]
  fn lower_generic_param(
    &self,
    generic: &ast::GenericParam,
  ) -> GenericParam<'hir> {
    match generic {
      ast::GenericParam::ConstParam(c) => GenericParam {
        id: self.next_id(),
        name: c.ident,
        kind: GenericParamKind::Const {
          ty: self.alloc(self.lower_type(&c.type_)),
          default: c.default.as_ref().map(|c| AnonConst {
            body: self.alloc(self.lower_expr(c)).id.to_body_id(),
          }),
        },
        span: c.span,
      },
      ast::GenericParam::TypeParam(type_param) => GenericParam {
        id: self.next_id(),
        name: type_param.ident,
        kind: GenericParamKind::Type {
          default: type_param
            .default
            .as_ref()
            .map(|t| &*self.alloc(self.lower_type(t))),
        },
        span: type_param.span,
      },
    }
  }

  #[must_use]
  fn lower_type(&self, type_: &ast::Type) -> Type<'hir> {
    match type_ {
      ast::Type::Ident(ident) => Type {
        id: self.next_id(),
        kind: match &*ident.name {
          "u8" => TypeKind::Primitive(PrimitiveType::Uint(IntBits::Eight)),
          "u16" => TypeKind::Primitive(PrimitiveType::Uint(IntBits::Sixteen)),
          "u32" => TypeKind::Primitive(PrimitiveType::Uint(IntBits::ThirtyTwo)),
          "u64" => TypeKind::Primitive(PrimitiveType::Uint(IntBits::SixtyFour)),
          "u128" => {
            TypeKind::Primitive(PrimitiveType::Uint(IntBits::OneTwentyEight))
          }
          "usize" => TypeKind::Primitive(PrimitiveType::Uint(IntBits::Size)),

          "i8" => TypeKind::Primitive(PrimitiveType::Int(IntBits::Eight)),
          "i16" => TypeKind::Primitive(PrimitiveType::Int(IntBits::Sixteen)),
          "i32" => TypeKind::Primitive(PrimitiveType::Int(IntBits::ThirtyTwo)),
          "i64" => TypeKind::Primitive(PrimitiveType::Int(IntBits::SixtyFour)),
          "i128" => {
            TypeKind::Primitive(PrimitiveType::Int(IntBits::OneTwentyEight))
          }
          "isize" => TypeKind::Primitive(PrimitiveType::Int(IntBits::Size)),

          "str" => TypeKind::Primitive(PrimitiveType::Str),

          _ => TypeKind::Path(*ident),
        },
        span: ident.span,
      },
      ast::Type::Ref(inner) => {
        let inner = self.alloc(self.lower_type(inner));
        Type {
          id: self.next_id(),
          kind: TypeKind::Ref(MutType {
            inner,
            mutability: Mutability::No,
          }),
          // TODO get span of the outer type
          span: inner.span,
        }
      }
    }
  }

  #[must_use]
  fn lower_literal(&self, literal: &ast::Literal) -> Literal<'hir> {
    let (kind, span) = match literal {
      ast::Literal::NumLit(num_lit) => (
        LitKind::Num(NumLit {
          type_: match num_lit.ty {
            Some(symbol::NumLitType::Int(bits)) => {
              NumLitType::Signed(convert_int_bits(bits).unwrap())
            }
            Some(symbol::NumLitType::Unsigned(bits)) => {
              NumLitType::Unsigned(convert_int_bits(bits).unwrap())
            }
            Some(symbol::NumLitType::Float(bits)) => {
              NumLitType::Float(convert_float_bits(bits).unwrap())
            }
            None => NumLitType::Unspecified,
          },
          value: num_lit.val,
          floating: num_lit.decimal,
          radix: match num_lit.prefix {
            Some(symbol::NumLitPrefix::Binary) => NumLitRadix::Bin,
            Some(symbol::NumLitPrefix::Hex) => NumLitRadix::Hex,
            None => NumLitRadix::Dec,
          },
        }),
        num_lit.span,
      ),
      ast::Literal::StrLit(str_lit) => {
        (LitKind::Str(str_lit.value), str_lit.span)
      }
      ast::Literal::BoolLit(bool_lit) => {
        (LitKind::Bool(bool_lit.value), bool_lit.span)
      }
    };
    Literal {
      id: self.next_id(),
      kind,
      span,
    }
  }
}

#[must_use]
fn convert_visibility(raw: &AstVis) -> Visibility {
  match raw {
    None => Visibility::Inherited,
    Some(None) => Visibility::Public,
    Some(Some(ast::VisKind::KwSuper)) => Visibility::Super,
    Some(Some(ast::VisKind::KwPackage)) => Visibility::Package,
  }
}

type AstVis = Option<Option<ast::VisKind>>;

fn convert_int_bits(raw: u16) -> Result<IntBits, BadBitNumber> {
  match raw {
    8 => Ok(IntBits::Eight),
    16 => Ok(IntBits::Sixteen),
    32 => Ok(IntBits::ThirtyTwo),
    64 => Ok(IntBits::SixtyFour),
    128 => Ok(IntBits::OneTwentyEight),
    b => Err(BadBitNumber(b)),
  }
}

fn convert_float_bits(raw: u16) -> Result<FloatBits, BadBitNumber> {
  match raw {
    32 => Ok(FloatBits::ThirtyTwo),
    64 => Ok(FloatBits::SixtyFour),
    b => Err(BadBitNumber(b)),
  }
}

#[derive(Copy, Clone, Debug)]
struct BadBitNumber(u16);

impl Error for BadBitNumber {}

impl Display for BadBitNumber {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "invalid bit count {}", self.0)
  }
}
