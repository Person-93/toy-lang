#![forbid(unsafe_code)]
#![no_std]

use core::fmt::{Result, Write};
use toyc_hir::{
  Abi, ArrayLen, AssignOp, BinOp, Body, Constness, EnumDef, Expr, ExprKind,
  Extern, FieldDef, FloatBits, FnRetTy, FnType, Function, GenericParam,
  GenericParamKind, GenericParams, HirContext, IntBits, Item, LitKind, Mod,
  MutType, Mutability, NumLit, NumLitRadix, NumLitType, PrimitiveType,
  SelfKind, StructDef, StructDefKind, Trait, Type, TypeAlias, TypeKind, UnOp,
  Unsafety, Visibility,
};

pub fn print<W: Write>(context: &HirContext<'_>, writer: &mut W) -> Result {
  Printer { context, writer }.print()
}

struct Printer<'hir: 'a, 'a, W: Write> {
  context: &'a HirContext<'hir>,
  writer: &'a mut W,
}

impl<W: Write> Printer<'_, '_, W> {
  fn print(mut self) -> Result {
    for item in self.context.root().items {
      writeln!(self.writer)?;
      match item {
        Item::Mod(m) => self.print_mod(m, 0),
        Item::Function(f) => self.print_function(f, 0),
        Item::Enum(e) => self.print_enum(e, 0),
        Item::Struct(s) => self.print_struct(s, 0),
        Item::Trait(t) => self.print_trait(t, 0),
        Item::TypeAlias(t) => self.print_type_alias(t, 0),
      }?;
      writeln!(self.writer)?;
    }
    Ok(())
  }

  fn print_mod(&mut self, _mod_: &Mod<'_>, _indentation: u8) -> Result {
    todo!("print mod")
  }

  fn print_function(
    &mut self,
    Function {
      id: _,
      ident,
      visibility,
      self_kind,
      fn_type,
      body_id,
      span: _,
    }: &Function<'_>,
    indentation: u8,
  ) -> Result {
    let FnType {
      extern_,
      unsafety,
      constness,
      generics,
      inputs,
      output,
    } = fn_type;

    self.do_indent(indentation)?;

    self.print_visibility(visibility)?;
    self.print_extern(*extern_)?;
    self.print_constness(*constness)?;
    self.print_unsafety(*unsafety)?;

    write!(self.writer, "fn {ident}")?;

    if let Some(generics) = generics {
      self.print_generic_params(generics, indentation + 1)?;
    }

    match *body_id {
      Some(body_id) => {
        let Body { params, expr } = self.context.get_body(body_id);

        write!(self.writer, "(")?;
        let mut params = params.iter().zip(inputs.iter());
        if let Some((param, ty)) = params.next() {
          match self_kind {
            SelfKind::Imm => write!(self.writer, "self"),
            SelfKind::Mut => write!(self.writer, "mut self"),
            SelfKind::Ref => write!(self.writer, "&self"),
            SelfKind::RefMut => write!(self.writer, "&mut self"),
            SelfKind::None => {
              write!(self.writer, "{}: ", param.ident)?;
              self.print_type(ty, indentation + 1)
            }
          }?;
        }
        for (param, ty) in params {
          write!(self.writer, "{}: ", param.ident)?;
          self.print_type(ty, indentation + 1)?;
        }
        write!(self.writer, ")")?;

        if let FnRetTy::Explicit(output) = output {
          write!(self.writer, " -> ")?;
          self.print_type(output, indentation + 1)?;
        }

        write!(self.writer, ": ")?;

        self.print_expr(expr, indentation + 1)?;
      }
      None => {
        write!(self.writer, "(")?;
        let mut inputs = inputs.iter();
        if let Some(input) = inputs.next() {
          self.print_type(input, indentation + 1)?;
        }
        for input in inputs {
          self.print_type(input, indentation + 1)?;
        }
        write!(self.writer, ")")?;

        if let FnRetTy::Explicit(output) = output {
          write!(self.writer, " -> ")?;
          self.print_type(output, indentation + 1)?;
        }

        write!(self.writer, ";")?;
      }
    }

    Ok(())
  }

  fn print_enum(
    &mut self,
    EnumDef {
      id: _,
      ident,
      vis,
      generics,
      def,
      span: _,
    }: &EnumDef<'_>,
    indentation: u8,
  ) -> Result {
    self.do_indent(indentation)?;
    self.print_visibility(vis)?;
    write!(self.writer, "enum {ident}")?;
    if let Some(generics) = generics {
      self.print_generic_params(generics, indentation + 1)?;
    }
    writeln!(self.writer, " {{")?;
    for variant in def.iter() {
      self.do_indent(indentation + 1)?;
      write!(self.writer, "{ident}")?;
      self.print_struct_kind(&variant.kind, indentation + 1)?;
      writeln!(self.writer, ";")?;
    }
    write!(self.writer, "}}")?;

    Ok(())
  }

  fn print_struct(
    &mut self,
    StructDef {
      id: _,
      ident,
      vis,
      generics,
      kind,
      span: _,
    }: &StructDef<'_>,
    indentation: u8,
  ) -> Result {
    self.do_indent(indentation)?;
    self.print_visibility(vis)?;
    write!(self.writer, "struct {ident}")?;
    if let Some(generics) = generics {
      self.print_generic_params(generics, indentation + 1)?;
    }
    writeln!(self.writer, " {{")?;
    self.print_struct_kind(kind, indentation + 1)
  }

  fn print_struct_kind(
    &mut self,
    struct_kind: &StructDefKind<'_>,
    indentation: u8,
  ) -> Result {
    match struct_kind {
      StructDefKind::Struct(fields) | StructDefKind::Tuple(fields) => {
        for field in fields.iter() {
          let FieldDef {
            id: _,
            ident,
            type_,
            vis,
            span: _,
          } = field;

          self.do_indent(indentation)?;
          self.print_visibility(vis)?;
          write!(self.writer, "{ident}: ")?;
          self.print_type(type_, indentation + 1)?;
        }
      }
      StructDefKind::Unit => write!(self.writer, ";")?,
    }
    Ok(())
  }

  fn print_trait(&mut self, _trait_: &Trait<'_>, _indentation: u8) -> Result {
    todo!("print trait")
  }

  fn print_type_alias(
    &mut self,
    _type_alias: &TypeAlias<'_>,
    _indentation: u8,
  ) -> Result {
    todo!("print type alias")
  }

  fn print_expr(&mut self, expr: &Expr<'_>, indentation: u8) -> Result {
    match &expr.kind {
      ExprKind::Ident(ident) => write!(self.writer, "{ident}")?,
      ExprKind::Lit(lit) => match lit.kind {
        LitKind::Str(lit) => write!(self.writer, "{lit}"),
        LitKind::Num(lit) => {
          let NumLit {
            type_,
            value,
            floating,
            radix,
          } = lit;
          match radix {
            NumLitRadix::Bin => write!(self.writer, "0b{value}")?,
            NumLitRadix::Dec => (),
            NumLitRadix::Hex => write!(self.writer, "0x{value}")?,
          }
          if let Some(float) = floating {
            write!(self.writer, ".{float}")?;
          }
          match type_ {
            NumLitType::Signed(IntBits::Eight) => write!(self.writer, "i8"),
            NumLitType::Signed(IntBits::Sixteen) => write!(self.writer, "i16"),
            NumLitType::Signed(IntBits::ThirtyTwo) => {
              write!(self.writer, "i32")
            }
            NumLitType::Signed(IntBits::SixtyFour) => {
              write!(self.writer, "i64")
            }
            NumLitType::Signed(IntBits::OneTwentyEight) => {
              write!(self.writer, "i128")
            }
            NumLitType::Signed(IntBits::Size) => write!(self.writer, "isize"),
            NumLitType::Unsigned(IntBits::Eight) => write!(self.writer, "u8"),
            NumLitType::Unsigned(IntBits::Sixteen) => {
              write!(self.writer, "u16")
            }
            NumLitType::Unsigned(IntBits::ThirtyTwo) => {
              write!(self.writer, "u32")
            }
            NumLitType::Unsigned(IntBits::SixtyFour) => {
              write!(self.writer, "u64")
            }
            NumLitType::Unsigned(IntBits::OneTwentyEight) => {
              write!(self.writer, "u128")
            }
            NumLitType::Unsigned(IntBits::Size) => write!(self.writer, "usize"),
            NumLitType::Float(FloatBits::ThirtyTwo) => {
              write!(self.writer, "f32")
            }
            NumLitType::Float(FloatBits::SixtyFour) => {
              write!(self.writer, "f64")
            }
            NumLitType::Unspecified => Ok(()),
          }?;
          Ok(())
        }
        LitKind::Bool(true) => write!(self.writer, "true"),
        LitKind::Bool(false) => write!(self.writer, "false"),
      }?,
      ExprKind::CodeBlock {
        unsafety,
        constness,
        statements,
        trailing,
      } => {
        self.print_unsafety(*unsafety)?;
        self.print_constness(*constness)?;
        writeln!(self.writer, "{{")?;
        for statement in statements.iter() {
          self.do_indent(indentation)?;
          self.print_expr(statement, indentation + 1)?;
          writeln!(self.writer, ";")?;
        }
        if let Some(trailing) = trailing {
          self.do_indent(indentation)?;
          write!(self.writer, "return ")?;
          self.print_expr(trailing, indentation + 1)?;
          writeln!(self.writer, ";")?;
        }
        writeln!(self.writer, "}}")?;
      }
      ExprKind::Assign {
        op,
        receiver,
        value,
      } => {
        self.print_expr(receiver, indentation + 1)?;
        match op {
          AssignOp::Plain => write!(self.writer, " := "),
          AssignOp::Band => write!(self.writer, " &= "),
          AssignOp::Bor => write!(self.writer, " |= "),
          AssignOp::Xor => write!(self.writer, " ^= "),
          AssignOp::Shl => write!(self.writer, " <<= "),
          AssignOp::Shr => write!(self.writer, " >>= "),
          AssignOp::Add => write!(self.writer, " += "),
          AssignOp::Sub => write!(self.writer, " -= "),
          AssignOp::Rem => write!(self.writer, " %= "),
          AssignOp::Mul => write!(self.writer, " *= "),
          AssignOp::Div => write!(self.writer, " /= "),
        }?;
        self.print_expr(value, indentation + 1)?;
      }
      ExprKind::Binary(op, lhs, rhs) => {
        self.print_expr(lhs, indentation + 1)?;
        match op {
          BinOp::Eq => write!(self.writer, " = "),
          BinOp::Neq => write!(self.writer, " != "),
          BinOp::Lt => write!(self.writer, " < "),
          BinOp::Lte => write!(self.writer, " <= "),
          BinOp::Gt => write!(self.writer, " > "),
          BinOp::Gte => write!(self.writer, " >= "),
          BinOp::Band => write!(self.writer, " & "),
          BinOp::Bor => write!(self.writer, " | "),
          BinOp::Xor => write!(self.writer, " ^ "),
          BinOp::Shl => write!(self.writer, " << "),
          BinOp::Shr => write!(self.writer, " >> "),
          BinOp::Add => write!(self.writer, " + "),
          BinOp::Sub => write!(self.writer, " - "),
          BinOp::Rem => write!(self.writer, " % "),
          BinOp::Mul => write!(self.writer, " * "),
          BinOp::Div => write!(self.writer, " / "),
          BinOp::Range => write!(self.writer, " .. "),
          BinOp::RangeInc => write!(self.writer, " ..= "),
        }?;
        self.print_expr(rhs, indentation + 1)?;
      }
      ExprKind::Unary(op, expr) => {
        match op {
          UnOp::Deref => write!(self.writer, "*"),
          UnOp::Not => write!(self.writer, "not "),
          UnOp::Neg => write!(self.writer, "-"),
        }?;
        self.print_expr(expr, indentation + 1)?;
      }
      ExprKind::Call { callee, args } => {
        self.print_expr(callee, indentation + 1)?;
        write!(self.writer, "(")?;
        let mut args = args.iter();
        if let Some(arg) = args.next() {
          self.print_expr(arg, indentation + 1)?;
        }
        for arg in args {
          write!(self.writer, ", ")?;
          self.print_expr(arg, indentation + 1)?;
        }
        write!(self.writer, ")")?;
      }
      ExprKind::MethodCall {
        receiver,
        callee,
        args,
      } => {
        self.print_expr(receiver, indentation + 1)?;
        write!(self.writer, ".")?;
        self.print_expr(callee, indentation + 1)?;
        write!(self.writer, "(")?;
        let mut args = args.iter();
        if let Some(arg) = args.next() {
          self.print_expr(arg, indentation + 1)?;
        }
        for arg in args {
          write!(self.writer, ", ")?;
          self.print_expr(arg, indentation + 1)?;
        }
        write!(self.writer, ")")?;
      }
      ExprKind::Index { receiver, value } => {
        self.print_expr(receiver, indentation + 1)?;
        write!(self.writer, "[")?;
        self.print_expr(value, indentation + 1)?;
        write!(self.writer, "]")?;
      }
      ExprKind::Tuple(members) => {
        let mut members = members.iter();
        write!(self.writer, "(")?;
        if let Some(member) = members.next() {
          self.print_expr(member, indentation + 1)?;
        }
        for member in members {
          write!(self.writer, ", ")?;
          self.print_expr(member, indentation + 1)?;
        }
        write!(self.writer, ")")?;
      }
      ExprKind::Cast(expr, ty) => {
        self.print_expr(expr, indentation + 1)?;
        write!(self.writer, " as ")?;
        self.print_type(ty, indentation + 1)?;
      }
      ExprKind::Type(ty) => self.print_type(ty, indentation + 1)?,
      ExprKind::FieldAccess(expr, ident) => {
        self.print_expr(expr, indentation + 1)?;
        write!(self.writer, ".{ident}")?;
      }
      ExprKind::Err => write!(self.writer, "<INVALID-EXPR>")?,
    }
    Ok(())
  }

  fn print_generic_params(
    &mut self,
    generics: &GenericParams<'_>,
    indentation: u8,
  ) -> Result {
    write!(self.writer, "::<")?;
    let mut generics = generics.params.iter();
    if let Some(generic) = generics.next() {
      self.print_generic_param(generic, indentation)?;
    }
    for generic in generics {
      write!(self.writer, ", ")?;
      self.print_generic_param(generic, indentation)?;
    }
    write!(self.writer, ">")
  }

  fn print_generic_param(
    &mut self,
    generic: &GenericParam<'_>,
    indentation: u8,
  ) -> Result {
    match &generic.kind {
      GenericParamKind::Type { default } => {
        write!(self.writer, "{}", generic.name)?;
        if let Some(default) = default {
          write!(self.writer, " = ")?;
          self.print_type(default, indentation + 1)?;
        }
      }
      GenericParamKind::Const { ty, default } => {
        write!(self.writer, "const {}: ", generic.name)?;
        self.print_type(ty, indentation + 1)?;
        if let Some(default) = default {
          write!(self.writer, " = ")?;
          let body = self.context.get_body(default.body);
          self.print_expr(body.expr, indentation + 1)?;
        }
      }
    }
    Ok(())
  }

  fn print_type(&mut self, ty: &Type<'_>, indentation: u8) -> Result {
    match &ty.kind {
      TypeKind::Primitive(PrimitiveType::Int(IntBits::Eight)) => {
        write!(self.writer, "i8")?
      }
      TypeKind::Primitive(PrimitiveType::Int(IntBits::Sixteen)) => {
        write!(self.writer, "i16")?
      }
      TypeKind::Primitive(PrimitiveType::Int(IntBits::ThirtyTwo)) => {
        write!(self.writer, "i32")?
      }
      TypeKind::Primitive(PrimitiveType::Int(IntBits::SixtyFour)) => {
        write!(self.writer, "i64")?
      }
      TypeKind::Primitive(PrimitiveType::Int(IntBits::OneTwentyEight)) => {
        write!(self.writer, "i128")?
      }
      TypeKind::Primitive(PrimitiveType::Int(IntBits::Size)) => {
        write!(self.writer, "isize")?
      }
      TypeKind::Primitive(PrimitiveType::Uint(IntBits::Eight)) => {
        write!(self.writer, "u8")?
      }
      TypeKind::Primitive(PrimitiveType::Uint(IntBits::Sixteen)) => {
        write!(self.writer, "u16")?
      }
      TypeKind::Primitive(PrimitiveType::Uint(IntBits::ThirtyTwo)) => {
        write!(self.writer, "u32")?
      }
      TypeKind::Primitive(PrimitiveType::Uint(IntBits::SixtyFour)) => {
        write!(self.writer, "u64")?
      }
      TypeKind::Primitive(PrimitiveType::Uint(IntBits::OneTwentyEight)) => {
        write!(self.writer, "u128")?
      }
      TypeKind::Primitive(PrimitiveType::Uint(IntBits::Size)) => {
        write!(self.writer, "usize")?
      }
      TypeKind::Primitive(PrimitiveType::Float(FloatBits::ThirtyTwo)) => {
        write!(self.writer, "f32")?
      }
      TypeKind::Primitive(PrimitiveType::Float(FloatBits::SixtyFour)) => {
        write!(self.writer, "f64")?
      }
      TypeKind::Primitive(PrimitiveType::Str) => write!(self.writer, "str")?,
      TypeKind::Primitive(PrimitiveType::Bool) => write!(self.writer, "bool")?,
      TypeKind::Primitive(PrimitiveType::Char) => write!(self.writer, "char")?,
      TypeKind::Path(ident) => write!(self.writer, "{ident}")?,
      TypeKind::Slice(inner) => {
        write!(self.writer, "[")?;
        self.print_type(inner, indentation + 1)?;
        write!(self.writer, "]")?;
      }
      TypeKind::Array(inner, len) => {
        write!(self.writer, "[")?;
        self.print_type(inner, indentation + 1)?;
        write!(self.writer, "; ")?;
        match len {
          ArrayLen::Given(given) => self.print_expr(
            self.context.get_body(given.body).expr,
            indentation + 1,
          )?,
          ArrayLen::Infer(_) => write!(self.writer, "_")?,
        }
        write!(self.writer, "]")?;
      }
      TypeKind::Ptr(mut_type) => {
        write!(self.writer, "*")?;
        self.print_mut_type(mut_type, indentation + 1)?;
      }
      TypeKind::Ref(mut_type) => {
        write!(self.writer, "&")?;
        self.print_mut_type(mut_type, indentation + 1)?;
      }
      TypeKind::Fn(fn_type) => {
        let FnType {
          extern_,
          unsafety,
          constness,
          generics,
          inputs,
          output,
        } = fn_type;

        self.print_extern(*extern_)?;
        self.print_unsafety(*unsafety)?;
        self.print_constness(*constness)?;

        write!(self.writer, "fn")?;
        if let Some(generics) = generics {
          self.print_generic_params(generics, indentation + 1)?;
        }

        write!(self.writer, "(")?;
        let mut inputs = inputs.iter();
        if let Some(input) = inputs.next() {
          self.print_type(input, indentation + 1)?;
        }
        for input in inputs {
          write!(self.writer, ", ")?;
          self.print_type(input, indentation + 1)?;
        }
        write!(self.writer, ")")?;

        match output {
          FnRetTy::Default => (),
          FnRetTy::Explicit(ty) => self.print_type(ty, indentation + 1)?,
        }
      }
      TypeKind::Never => write!(self.writer, "!")?,
      TypeKind::Tuple(members) => {
        let mut members = members.iter();

        write!(self.writer, "(")?;
        if let Some(member) = members.next() {
          self.print_type(member, indentation + 1)?;
        }
        for member in members {
          write!(self.writer, ", ")?;
          self.print_type(member, indentation + 1)?;
        }
        write!(self.writer, ")")?;
      }
      TypeKind::Enum(EnumDef { ident, .. })
      | TypeKind::Struct(StructDef { ident, .. }) => {
        write!(self.writer, "{ident}")?
      }
      TypeKind::Infer => write!(self.writer, "_")?,
      TypeKind::Err => write!(self.writer, "<INVALID-TYPE>")?,
    }
    Ok(())
  }

  fn print_mut_type(
    &mut self,
    MutType { inner, mutability }: &MutType<'_>,
    indentation: u8,
  ) -> Result {
    if matches!(mutability, Mutability::Yes) {
      write!(self.writer, "mut ")?;
    }
    self.print_type(inner, indentation)
  }

  fn print_visibility(&mut self, visibility: &Visibility) -> Result {
    match visibility {
      Visibility::Public => write!(self.writer, "pub "),
      Visibility::Super => write!(self.writer, "pub(super) "),
      Visibility::Package => write!(self.writer, "pub(package) "),
      Visibility::Inherited => Ok(()),
    }
  }

  fn print_extern(&mut self, extern_: Extern) -> Result {
    match extern_ {
      Extern::Yes(Abi::Toy) => write!(self.writer, "extern \"toy\" "),
      Extern::Yes(Abi::C) => write!(self.writer, "extern \"C\" "),
      Extern::Yes(Abi::Err(lit)) => {
        write!(self.writer, "extern INVALID({lit}) ")
      }
      Extern::No => Ok(()),
    }
  }

  fn print_constness(&mut self, constness: Constness) -> Result {
    if matches!(constness, Constness::Yes) {
      write!(self.writer, "const ")?;
    }
    Ok(())
  }

  fn print_unsafety(&mut self, unsafety: Unsafety) -> Result {
    if matches!(unsafety, Unsafety::Yes) {
      write!(self.writer, "unsafe ")?;
    }
    Ok(())
  }

  fn do_indent(&mut self, indentation: u8) -> Result {
    for _ in 0..indentation {
      write!(self.writer, "  ")?
    }
    Ok(())
  }
}
