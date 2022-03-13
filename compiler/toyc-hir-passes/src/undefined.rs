use crate::definitions::{
  Binding, Constant, Definition, Definitions, UserType,
};
use alloc::rc::Rc;
use toyc_hir::{
  EnumDef, Expr, ExprKind, FnRetTy, Function, GenericParamKind, GenericParams,
  HirContext, Item, MutType, StructDef, StructDefKind, Trait, Type, TypeAlias,
  TypeKind,
};
use toyc_session::Session;
use toyc_span::symbol::Ident;
use toyc_span::Span;

pub(crate) fn check_items(
  session: &Session,
  context: &HirContext<'_>,
  definitions: Rc<Definitions<'_, '_>>,
  items: &[Item<'_>],
) {
  for item in items {
    match item {
      Item::Mod(mod_) => {
        if let Some(definitions) = definitions.lookup_mod(mod_.ident) {
          check_items(session, context, definitions.clone(), mod_.items);
        }
      }
      Item::Function(function) => {
        check_function(session, context, definitions.clone(), function)
      }
      Item::Enum(enum_) => {
        check_enum(session, context, definitions.clone(), enum_)
      }
      Item::Struct(struct_) => {
        check_struct(session, context, definitions.clone(), struct_)
      }
      Item::Trait(trait_) => {
        check_trait(session, context, definitions.clone(), trait_)
      }
      Item::TypeAlias(type_alias) => {
        check_type_alias(session, context, definitions.clone(), type_alias)
      }
    }
  }
}

fn check_function(
  session: &Session,
  context: &HirContext<'_>,
  definitions: Rc<Definitions<'_, '_>>,
  function: &Function<'_>,
) {
  let definitions = if let Some(generics) = &function.fn_type.generics {
    check_generic_params(session, context, definitions, generics)
  } else {
    definitions
  };

  for input in function.fn_type.inputs {
    if let Err(missing) = definitions.type_exists(input) {
      session
        .handler
        .error("function parameter type does not exist")
        .add_span(missing.0, "this type is missing")
        .emit();
    }
  }

  match &function.fn_type.output {
    FnRetTy::Default => (),
    FnRetTy::Explicit(output) => {
      if let Err(missing) = definitions.type_exists(output) {
        session
          .handler
          .error("function return type does not exist")
          .add_span(missing.0, "this type is missing")
          .emit();
      }
    }
  }

  if let Some(body) = function.body_id.map(|id| context.get_body(id)) {
    let definitions = {
      let mut definitions = Rc::new(Definitions {
        defs: Default::default(),
        parent_scope: Rc::downgrade(&definitions),
      });
      {
        let definitions = Rc::get_mut(&mut definitions).unwrap();

        for (param, type_) in body.params.iter().zip(function.fn_type.inputs) {
          definitions.defs.entry(param.ident).or_default().binding =
            Some(Binding::Param(type_));
        }
      }
      definitions
    };

    check_expr(session, context, &definitions, body.expr);
  }
}

fn check_enum(
  session: &Session,
  context: &HirContext<'_>,
  definitions: Rc<Definitions<'_, '_>>,
  enum_: &EnumDef<'_>,
) {
  let definitions = if let Some(generics) = &enum_.generics {
    check_generic_params(session, context, definitions.clone(), generics)
  } else {
    definitions
  };

  for variant in enum_.def {
    check_struct(session, context, definitions.clone(), variant);
  }
}

fn check_struct(
  session: &Session,
  context: &HirContext<'_>,
  definitions: Rc<Definitions<'_, '_>>,
  struct_: &StructDef<'_>,
) {
  let definitions = if let Some(generics) = &struct_.generics {
    check_generic_params(session, context, definitions, generics)
  } else {
    definitions
  };

  match &struct_.kind {
    StructDefKind::Struct(fields) | StructDefKind::Tuple(fields) => {
      for field in *fields {
        if let Err(missing) = definitions.type_exists(field.type_) {
          session
            .handler
            .error("field type does not exist")
            .add_span(missing.0, "this type does not exist")
            .emit();
        }
      }
    }
    StructDefKind::Unit => (),
  }
}

fn check_trait(
  _session: &Session,
  _context: &HirContext<'_>,
  _definitions: Rc<Definitions<'_, '_>>,
  _trait_: &Trait<'_>,
) {
  todo!("check trait for use of undefined symbols")
}

fn check_type_alias(
  _session: &Session,
  _context: &HirContext<'_>,
  _definitions: Rc<Definitions<'_, '_>>,
  _type_alias: &TypeAlias<'_>,
) {
  todo!("check type alias for use of undefined symbols")
}

fn check_expr(
  session: &Session,
  context: &HirContext<'_>,
  definitions: &Definitions<'_, '_>,
  expr: &Expr<'_>,
) {
  match &expr.kind {
    ExprKind::Ident(_) => (),
    ExprKind::Lit(_) => (),
    ExprKind::CodeBlock {
      statements,
      trailing,
      ..
    } => {
      for statement in *statements {
        check_expr(session, context, definitions, statement);
      }
      if let Some(trailing) = trailing {
        check_expr(session, context, definitions, trailing);
      }
    }
    ExprKind::Assign {
      receiver, value, ..
    } => {
      check_expr(session, context, definitions, receiver);
      check_expr(session, context, definitions, value);
    }
    ExprKind::Binary(_, lhs, rhs) => {
      check_expr(session, context, definitions, lhs);
      check_expr(session, context, definitions, rhs);
    }
    ExprKind::Unary(_, expr) => check_expr(session, context, definitions, expr),
    ExprKind::Call { callee, args } => {
      check_expr(session, context, definitions, callee);
      for arg in *args {
        check_expr(session, context, definitions, arg);
      }
    }
    ExprKind::MethodCall {
      receiver,
      callee,
      args,
    } => {
      check_expr(session, context, definitions, receiver);
      check_expr(session, context, definitions, callee);
      for arg in *args {
        check_expr(session, context, definitions, arg);
      }
    }
    ExprKind::Index { receiver, value } => {
      check_expr(session, context, definitions, receiver);
      check_expr(session, context, definitions, value);
    }
    ExprKind::Tuple(members) => {
      for member in *members {
        check_expr(session, context, definitions, member);
      }
    }
    ExprKind::Cast(expr, ty) => {
      check_expr(session, context, definitions, expr);
      if let Err(missing) = definitions.type_exists(ty) {
        session
          .handler
          .error("cast as type that doesn't exist")
          .add_span(missing.0, "this type does not exist")
          .emit();
      }
    }
    ExprKind::FieldAccess(expr, _) => {
      check_expr(session, context, definitions, expr)
    }
    ExprKind::Err => (),
  }
}

/// Emits diagnostics for the generic params and returns a new [`Definitions`]
/// with the generic params included.
fn check_generic_params<'hir: 'a, 'a>(
  session: &Session,
  context: &HirContext<'_>,
  definitions: Rc<Definitions<'hir, 'a>>,
  generics: &GenericParams<'_>,
) -> Rc<Definitions<'hir, 'a>> {
  let mut definitions = Rc::new(Definitions {
    defs: Default::default(),
    parent_scope: Rc::downgrade(&definitions),
  });
  {
    let definitions = Rc::get_mut(&mut definitions).unwrap();
    for generic in generics.params {
      match &generic.kind {
        GenericParamKind::Type { default } => {
          if let Some(default) = default {
            if let Err(missing) = definitions.type_exists(default) {
              session
                .handler
                .error("default type of generic parameter does not exist")
                .add_span(missing.0, "this type does not exist")
                .emit();
            }
          }
          definitions.defs.entry(generic.name).or_default().user_type =
            Some(UserType::GenericParam);
        }
        GenericParamKind::Const { ty, default } => {
          if let Err(missing) = definitions.type_exists(ty) {
            session
              .handler
              .error("type of constant generic parameter does not exist")
              .add_span(missing.0, "this type does not exist")
              .emit();
          }
          if let Some(default) = default {
            let body = context.get_body(default.body);
            check_expr(session, context, definitions, body.expr);
          }
          definitions.defs.entry(generic.name).or_default().constant =
            Some(Constant::GenericPAram);
        }
      }
    }
  }
  definitions
}

impl<'hir: 'a, 'a> Definitions<'hir, 'a> {
  #[must_use]
  fn lookup_mod(&self, ident: Ident) -> Option<Rc<Definitions<'hir, 'a>>> {
    match self.defs.get(&ident).and_then(|def| def.mod_.clone()) {
      Some(def) => Some(def),
      None => {
        if let Some(parent) = self.parent_scope.upgrade() {
          parent.lookup_mod(ident)
        } else {
          None
        }
      }
    }
  }

  fn type_exists(&self, ty: &Type<'_>) -> Result<(), MissingType> {
    match &ty.kind {
      TypeKind::Primitive(_) => Ok(()),
      TypeKind::Path(ident) => match self.defs.get(ident) {
        Some(Definition {
          user_type: Some(_), ..
        }) => Ok(()),
        Some(Definition {
          user_type: None, ..
        })
        | None => Err(MissingType(ident.span)),
      },
      TypeKind::Slice(inner)
      | TypeKind::Array(inner, _)
      | TypeKind::Ptr(MutType { inner, .. })
      | TypeKind::Ref(MutType { inner, .. }) => self.type_exists(inner),
      TypeKind::Fn(fn_type) => {
        for input in fn_type.inputs {
          self.type_exists(input)?;
        }
        match fn_type.output {
          FnRetTy::Default => Ok(()),
          FnRetTy::Explicit(ty) => self.type_exists(ty),
        }
      }
      TypeKind::Never => Ok(()),
      TypeKind::Tuple(members) => {
        for member in *members {
          self.type_exists(member)?;
        }
        Ok(())
      }
      TypeKind::Enum(_)
      | TypeKind::Struct(_)
      | TypeKind::Infer
      | TypeKind::Err => Ok(()),
    }
  }
}

struct MissingType(Span);
