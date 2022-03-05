use crate::{BodyId, HirId};
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
      Node::AttrValue(value) => value.id,
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
      Node::Attr(attr) => attr.span(),
      Node::AttrData(data) => data.span,
      Node::AttrValue(value) => value.span,
    }
  }
}

#[derive(Debug)]
pub enum Item<'hir> {
  Mod(Mod<'hir>),
  Function(Function<'hir>),
  Enum(EnumDef<'hir>),
  Struct(StructDef<'hir>),
}

impl<'hir> Item<'hir> {
  pub fn id(&self) -> HirId<'hir> {
    match self {
      Item::Mod(mod_) => mod_.id,
      Item::Function(function) => function.id,
      Item::Enum(enum_) => enum_.id,
      Item::Struct(struct_) => struct_.id,
    }
  }

  pub fn span(&self) -> Span {
    match self {
      Item::Mod(mod_) => mod_.span,
      Item::Function(function) => function.span,
      Item::Enum(enum_) => enum_.span,
      Item::Struct(struct_) => struct_.span,
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
  pub generics: GenericParams<'hir>,
  pub signature: &'hir FnDecl<'hir>,
  pub body_id: BodyId<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub struct FnDecl<'hir> {
  pub extern_: Extern,
  pub unsafety: Unsafety,
  pub constness: Constness,
  pub inputs: &'hir [Type<'hir>],
  pub output: FnRetTy<'hir>,
  pub self_kind: SelfKind,
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
  pub generics: GenericParams<'hir>,
  pub def: &'hir [StructDef<'hir>],
  pub span: Span,
}

#[derive(Debug)]
pub struct StructDef<'hir> {
  pub id: HirId<'hir>,
  pub ident: Ident,
  pub vis: Visibility,
  pub generics: GenericParams<'hir>,
  pub def: StructDefKind<'hir>,
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
pub struct GenericParams<'hir> {
  pub id: HirId<'hir>,
  pub params: &'hir [GenericParam<'hir>],
  pub span: Span,
}

#[derive(Debug)]
pub struct GenericParam<'hir> {
  pub id: HirId<'hir>,
  pub name: Ident,
  pub kind: &'hir GenericParamKind<'hir>,
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
  pub span: Span,
}

impl MutType<'_> {
  pub fn id(&self) -> &HirId {
    &self.inner.id
  }
}

#[derive(Debug)]
pub struct FnType<'hir> {
  pub decl: FnDecl<'hir>,
  pub generic_params: &'hir GenericParams<'hir>,
  pub param_names: &'hir [Option<Ident>],
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
  Yes,
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
  pub kind: &'hir ExprKind<'hir>,
  pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind<'hir> {
  Lit(Lit),
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
pub struct Attr<'hir> {
  pub id: HirId<'hir>,
  pub data: &'hir AttrData<'hir>,
}

impl Attr<'_> {
  pub fn span(&self) -> Span {
    self.data.span
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
  Assign(Lit),
  Call(&'hir [AttrValue<'hir>]),
}

#[derive(Debug)]
pub struct AttrValue<'hir> {
  pub id: HirId<'hir>,
  pub kind: AttrValueKind<'hir>,
  pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum AttrValueKind<'hir> {
  NumLit(Lit),
  Nested(&'hir AttrData<'hir>),
}

#[derive(Copy, Clone, Debug)]
pub enum Lit {
  Str(Symbol),
  Num(NumLitKind),
  Bool(bool),
}

#[derive(Debug)]
pub struct BoolLit<'hir> {
  pub id: HirId<'hir>,
  pub value: bool,
  pub span: Span,
}

#[derive(Debug)]
pub struct StrLit<'hir> {
  pub id: HirId<'hir>,
  pub value: Symbol,
  pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum NumLitKind {
  Signed(i16),
  Unsigned(u32),
  Float(i16, u32),
}
