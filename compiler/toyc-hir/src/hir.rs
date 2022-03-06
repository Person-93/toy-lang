use crate::{BodyId, HirId};
use core::ops::Deref;
use toyc_span::symbol::StrLit;
use toyc_span::{
  symbol::{Ident, Symbol},
  Span,
};

#[derive(Copy, Clone, Debug)]
pub enum Node<'hir> {
  Package(&'hir Mod<'hir>),
  Item(&'hir Item<'hir>),
  Field(&'hir FieldDef<'hir>),
  GenericParams(&'hir GenericParams<'hir>),
  GenericParam(&'hir GenericParam<'hir>),
  Type(&'hir Type<'hir>),
  Expr(&'hir Expr<'hir>),
  NamedConst(&'hir NamedConst<'hir>),
  Attr(&'hir Attr<'hir>),
  AttrData(&'hir AttrData<'hir>),
  AttrValue(&'hir AttrValue<'hir>),
}

impl<'hir> Node<'hir> {
  pub fn id(self) -> HirId<'hir> {
    match self {
      Node::Package(package) => package.id,
      Node::Item(item) => item.id(),
      Node::Field(field) => field.id,
      Node::GenericParams(params) => params.id,
      Node::GenericParam(param) => param.id,
      Node::Type(type_) => type_.id,
      Node::Expr(expr) => expr.id,
      Node::NamedConst(const_) => const_.id,
      Node::Attr(attr) => attr.id,
      Node::AttrData(data) => data.id,
      Node::AttrValue(value) => value.id(),
    }
  }

  pub fn span(self) -> Span {
    match self {
      Node::Package(package) => package.span,
      Node::Item(item) => item.span(),
      Node::Field(field) => field.span,
      Node::GenericParams(params) => params.span,
      Node::GenericParam(param) => param.span,
      Node::Type(type_) => type_.span,
      Node::Expr(expr) => expr.span,
      Node::NamedConst(const_) => const_.span,
      Node::Attr(attr) => attr.span,
      Node::AttrData(data) => data.span,
      Node::AttrValue(value) => value.span(),
    }
  }
}

#[derive(Debug)]
pub struct Package<'hir> {
  pub attrs: &'hir [Attr<'hir>],
  pub items: &'hir [Item<'hir>],
  pub span: Span,
}

#[derive(Debug)]
pub enum Item<'hir> {
  Mod(Mod<'hir>),
  Function(Function<'hir>),
  Enum(EnumDef<'hir>),
  Struct(StructDef<'hir>),
  Trait(Trait<'hir>),
  TypeAlias(TypeAlias<'hir>),
}

impl<'hir> Item<'hir> {
  pub fn id(&self) -> HirId<'hir> {
    match self {
      Item::Mod(mod_) => mod_.id,
      Item::Function(function) => function.id,
      Item::Enum(enum_) => enum_.id,
      Item::Struct(struct_) => struct_.id,
      Item::Trait(trait_) => trait_.id,
      Item::TypeAlias(type_alias) => type_alias.id,
    }
  }

  pub fn span(&self) -> Span {
    match self {
      Item::Mod(mod_) => mod_.span,
      Item::Function(function) => function.span,
      Item::Enum(enum_) => enum_.span,
      Item::Struct(struct_) => struct_.span,
      Item::Trait(trait_) => trait_.span,
      Item::TypeAlias(type_alias) => type_alias.span,
    }
  }
}

#[derive(Debug)]
pub struct Mod<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub vis: Visibility,
  pub attrs: &'hir [Attr<'hir>],
  pub items: &'hir [&'hir Item<'hir>],
  pub span: Span,
  pub span_in_parent: Option<Span>,
}

impl Mod<'_> {
  pub fn is_root(&self) -> bool {
    self.id == HirId::ROOT_ID
  }
}

#[derive(Debug)]
pub struct Function<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub visibility: Visibility,
  pub fn_type: FnType<'hir>,
  pub body_id: Option<BodyId<'hir>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct FnType<'hir> {
  pub extern_: Extern,
  pub unsafety: Unsafety,
  pub constness: Constness,
  pub generics: Option<GenericParams<'hir>>,
  pub inputs: &'hir [Type<'hir>],
  pub output: FnRetTy<'hir>,
  pub self_kind: SelfKind,
}

#[derive(Copy, Clone, Debug)]
pub enum Abi {
  Toy,
  C,
  Err(StrLit),
}

#[derive(Debug)]
pub enum FnRetTy<'hir> {
  Default,
  Explicit(&'hir Type<'hir>),
}

#[derive(Copy, Clone, Debug)]
pub enum SelfKind {
  Imm,
  Mut,
  Ref,
  RefMut,
  None,
}

#[derive(Debug)]
pub struct Body<'hir> {
  pub params: &'hir [Param<'hir>],
  pub value: Expr<'hir>,
}

impl Body<'_> {
  pub fn id(&self) -> BodyId {
    self.value.id.to_body_id()
  }
}

#[derive(Debug)]
pub struct Param<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub ty_span: Span,
  pub span: Span,
}

#[derive(Debug)]
pub struct EnumDef<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub vis: Visibility,
  pub generics: Option<GenericParams<'hir>>,
  pub def: &'hir [StructDef<'hir>],
  pub span: Span,
}

#[derive(Debug)]
pub struct StructDef<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub vis: Visibility,
  pub generics: Option<GenericParams<'hir>>,
  pub kind: StructDefKind<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub enum StructDefKind<'hir> {
  Struct(&'hir [FieldDef<'hir>]),
  Tuple(&'hir [FieldDef<'hir>]),
  Unit,
}

#[derive(Debug)]
pub struct FieldDef<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub type_: &'hir Type<'hir>,
  pub vis: Visibility,
  pub span: Span,
}

#[derive(Debug)]
pub struct Trait<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub visibility: Visibility,
  pub generics: Option<GenericParams<'hir>>,
  pub items: &'hir [TraitItem<'hir>],
  pub span: Span,
}

#[derive(Debug)]
pub enum TraitItem<'hir> {
  Function(Function<'hir>),
  Const(TraitConst<'hir>),
  Type(TraitType<'hir>),
}

#[derive(Debug)]
pub struct TraitConst<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub default: Option<AnonConst<'hir>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct TraitType<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  // TODO pub bounds...
  pub default: Option<&'hir Type<'hir>>,
  pub span: Span,
}

#[derive(Debug)]
pub struct TypeAlias<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub visibility: Visibility,
  pub generics: Option<GenericParams<'hir>>,
  pub type_: &'hir Type<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub struct GenericParams<'hir> {
  pub id: HirId<'hir>,
  pub params: &'hir [GenericParam<'hir>],
  pub span: Span,
}

#[derive(Debug)]
pub struct GenericParam<'hir> {
  pub id: HirId<'hir>,
  pub name: Ident,
  pub kind: GenericParamKind<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub enum GenericParamKind<'hir> {
  Type {
    default: Option<&'hir Type<'hir>>,
  },
  Const {
    ty: &'hir Type<'hir>,
    default: Option<AnonConst<'hir>>,
  },
}

#[derive(Debug)]
pub struct Type<'hir> {
  pub id: HirId<'hir>,
  pub kind: TypeKind<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind<'hir> {
  Primitive(PrimitiveType),
  Path(Ident),
  Slice(&'hir Type<'hir>),
  Array(&'hir Type<'hir>, ArrayLen<'hir>),
  Ptr(MutType<'hir>),
  Ref(MutType<'hir>),
  Fn(FnType<'hir>),
  Never,
  Tuple(&'hir [Type<'hir>]),
  Enum(&'hir EnumDef<'hir>),
  Struct(&'hir StructDef<'hir>),
  Infer,
  Err,
}

#[derive(Debug)]
pub struct MutType<'hir> {
  pub inner: &'hir Type<'hir>,
  pub mutability: Mutability,
}

impl MutType<'_> {
  pub fn id(&self) -> &HirId {
    &self.inner.id
  }
}

#[derive(Copy, Clone, Debug)]
pub enum Mutability {
  Yes,
  No,
}

#[derive(Copy, Clone, Debug)]
pub enum Constness {
  Yes,
  No,
}

#[derive(Copy, Clone, Debug)]
pub enum Extern {
  Yes(Abi),
  No,
}

#[derive(Copy, Clone, Debug)]
pub enum Unsafety {
  Yes,
  No,
}

#[derive(Debug)]
pub enum Visibility {
  Public,
  Super,
  Package,
  Inherited,
  // TODO add 'Restricted' vis once QPath is implemented
}

#[derive(Debug)]
pub enum ArrayLen<'hir> {
  Given(AnonConst<'hir>),
  Infer(Span),
}

#[derive(Copy, Clone, Debug)]
pub enum PrimitiveType {
  Int(IntBits),
  Uint(IntBits),
  Float(FloatBits),
  Str,
  Bool,
  Char,
}

#[derive(Copy, Clone, Debug)]
pub enum IntBits {
  Eight,
  Sixteen,
  ThirtyTwo,
  SixtyFour,
  OneTwentyEight,
  Size,
}

#[derive(Copy, Clone, Debug)]
pub enum FloatBits {
  ThirtyTwo,
  SixtyFour,
}

#[derive(Debug)]
pub struct Expr<'hir> {
  pub id: HirId<'hir>,
  pub kind: ExprKind<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind<'hir> {
  Ident(Ident),
  Lit(&'hir Literal<'hir>),
  CodeBlock {
    unsafety: Unsafety,
    constness: Constness,
    statements: &'hir [Expr<'hir>],
    trailing: Option<&'hir Expr<'hir>>,
  },
  Assign {
    op: AssignOp,
    receiver: &'hir Expr<'hir>,
    value: &'hir Expr<'hir>,
  },
  Binary(BinOp, &'hir Expr<'hir>, &'hir Expr<'hir>),
  Unary(UnOp, &'hir Expr<'hir>),
  Call {
    callee: &'hir Expr<'hir>,
    args: &'hir [Expr<'hir>],
  },
  MethodCall {
    receiver: &'hir Expr<'hir>,
    callee: &'hir Expr<'hir>,
    args: &'hir [Expr<'hir>],
  },
  Index {
    receiver: &'hir Expr<'hir>,
    value: &'hir Expr<'hir>,
  },
  Tuple(&'hir [Expr<'hir>]),
  Cast(&'hir Expr<'hir>, &'hir Type<'hir>),
  Type(&'hir Expr<'hir>, &'hir Type<'hir>),
  FieldAccess(&'hir Expr<'hir>, Ident),
  // TODO AddrOf(BorrowKind, &'hir Expr<'hir>),
  Err,
}

#[derive(Copy, Clone, Debug)]
pub enum AssignOp {
  Plain,
  Band,
  Bor,
  Xor,
  Shl,
  Shr,
  Add,
  Sub,
  Rem,
  Mul,
  Div,
}

#[derive(Copy, Clone, Debug)]
pub enum BinOp {
  Eq,
  Neq,
  Lt,
  Lte,
  Gt,
  Gte,
  Band,
  Bor,
  Xor,
  Shl,
  Shr,
  Add,
  Sub,
  Rem,
  Mul,
  Div,
  Range,
  RangeInc,
}

#[derive(Copy, Clone, Debug)]
pub enum UnOp {
  Deref,
  Not,
  Neg,
}

#[derive(Debug)]
pub struct NamedConst<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub value: AnonConst<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub struct AnonConst<'hir> {
  pub body: BodyId<'hir>,
}

#[derive(Debug)]
pub struct Attr<'hir>(AttrData<'hir>);

impl Attr<'_> {
  pub fn from_data(data: AttrData) -> Attr {
    Attr(data)
  }
}

impl<'hir> Deref for Attr<'hir> {
  type Target = AttrData<'hir>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

#[derive(Debug)]
pub struct AttrData<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub kind: AttrKind<'hir>,
  pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum AttrKind<'hir> {
  Plain,
  Assign(&'hir Literal<'hir>),
  Call(&'hir [AttrValue<'hir>]),
}

#[derive(Copy, Clone, Debug)]
pub enum AttrValue<'hir> {
  Lit(&'hir Literal<'hir>),
  Nested(&'hir AttrData<'hir>),
}

impl<'hir> AttrValue<'hir> {
  pub fn id(self) -> HirId<'hir> {
    match self {
      AttrValue::Lit(Literal { id, .. })
      | AttrValue::Nested(AttrData { id, .. }) => *id,
    }
  }

  pub fn span(self) -> Span {
    match self {
      AttrValue::Lit(Literal { span, .. })
      | AttrValue::Nested(AttrData { span, .. }) => *span,
    }
  }
}

#[derive(Debug)]
pub struct Literal<'hir> {
  pub id: HirId<'hir>,
  pub kind: LitKind,
  pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum LitKind {
  Str(Symbol),
  Num(NumLit),
  Bool(bool),
}

#[derive(Debug)]
pub struct BoolLit<'hir> {
  pub id: HirId<'hir>,
  pub value: bool,
  pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub struct NumLit {
  pub type_: NumLitType,
  pub value: i32,
  pub floating: Option<u16>,
  pub radix: NumLitRadix,
}

#[derive(Copy, Clone, Debug)]
pub enum NumLitType {
  Signed(IntBits),
  Unsigned(IntBits),
  Float(FloatBits),
  Unspecified,
}

#[derive(Copy, Clone, Debug)]
pub enum NumLitRadix {
  Bin,
  Dec,
  Hex,
}
