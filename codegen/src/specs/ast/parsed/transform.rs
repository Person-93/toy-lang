use super::{
  super::{
    raw::{Ast as RawAst, NodeDef},
    Modifier, Specs,
  },
  Ast, Ident, Node, NodeKind,
};
use std::{cell::RefCell, collections::hash_map::HashMap};
use thiserror::Error;

impl<'a> RawAst<'a> {
  pub fn transform(&'a self, specs: &'a Specs<'a>) -> Result<Ast<'a>, Error> {
    let validator = Validator::new(self, specs);
    Ok(Ast(
      self
        .iter()
        .map(|entry| -> Result<_, _> {
          validator
            .validate_node_def(&entry.node_def)
            .map(|node_def| Node {
              ident: entry.ident,
              kind: node_from_def(node_def),
            })
        })
        .collect::<Result<_, _>>()?,
    ))
  }
}

fn node_from_def(node_def: ValidNodeDef) -> NodeKind {
  match node_def {
    ValidNodeDef::Node(ident) => NodeKind::Node(ident),
    ValidNodeDef::Modified(inner, modifier) => {
      NodeKind::Modified(Box::new(node_from_def(*inner)), modifier)
    }
    ValidNodeDef::StaticToken(ident) => NodeKind::StaticToken(ident),
    ValidNodeDef::DynamicToken(ident) => NodeKind::DynamicToken(ident),
    ValidNodeDef::Group(node_defs) => {
      NodeKind::Group(node_defs.into_iter().map(node_from_def).collect::<Vec<_>>())
    }
    ValidNodeDef::Choice(node_defs) => {
      NodeKind::Choice(node_defs.into_iter().map(node_from_def).collect::<Vec<_>>())
    }
    ValidNodeDef::Delimited(inner, delimiter) => {
      NodeKind::Delimited(Box::new(node_from_def(*inner)), delimiter)
    }
  }
}

struct Validator<'v, 'ast: 'v> {
  ast: &'v RawAst<'ast>,
  specs: &'v Specs<'ast>,
  cache: RefCell<HashMap<&'v NodeDef<'ast>, ValidNodeDef<'ast>>>,
}

impl<'v, 'ast: 'v> Validator<'v, 'ast> {
  fn new(ast: &'v RawAst<'ast>, specs: &'v Specs<'ast>) -> Validator<'v, 'ast> {
    Validator {
      ast,
      specs,
      cache: Default::default(),
    }
  }

  fn validate_node_def<'r: 'v>(
    &'r self,
    node_def: &'v NodeDef<'ast>,
  ) -> Result<ValidNodeDef<'ast>, Error> {
    let cache = self.cache.borrow();
    Ok(if let Some(valid_node_def) = cache.get(node_def) {
      valid_node_def.clone()
    } else {
      drop(cache);
      let valid_node = match node_def {
        NodeDef::Simple(ident) => {
          let ident = *ident;
          if self.ast.get(&ident).is_some() {
            ValidNodeDef::Node(ident)
          } else if self.specs.static_tokens.contains(ident.0) {
            ValidNodeDef::StaticToken(ident)
          } else if self.specs.dynamic_tokens.contains(ident.0) {
            ValidNodeDef::DynamicToken(ident)
          } else {
            return Err(Error::UnknownIdent(String::from(ident.0)));
          }
        }
        NodeDef::Modified(inner, modifier) => {
          ValidNodeDef::Modified(Box::new(self.validate_node_def(inner)?), *modifier)
        }
        NodeDef::Delimiter(inner, delimiter) => {
          if self.specs.delimiters.contains(delimiter.0) {
            ValidNodeDef::Delimited(Box::new(self.validate_node_def(inner)?), *delimiter)
          } else {
            return Err(Error::UnknownDelim(String::from(delimiter.0)));
          }
        }
        NodeDef::Group(inner) => ValidNodeDef::Group(
          inner
            .iter()
            .map(|node| self.validate_node_def(node))
            .collect::<Result<_, _>>()?,
        ),
        NodeDef::Choice(first, second) => {
          ValidNodeDef::Choice(self.validate_choices(first, second, Vec::with_capacity(1))?)
        }
      };
      self.cache.borrow_mut().insert(node_def, valid_node.clone());
      valid_node
    })
  }

  fn validate_choices<'r: 'v>(
    &'r self,
    first: &'v NodeDef<'ast>,
    second: &'v NodeDef<'ast>,
    mut processed: Vec<ValidNodeDef<'ast>>,
  ) -> Result<Vec<ValidNodeDef<'ast>>, Error> {
    processed.push(self.validate_node_def(first)?);
    match second {
      NodeDef::Choice(first, second) => self.validate_choices(first, second, processed),
      node_def => {
        processed.push(self.validate_node_def(node_def)?);
        Ok(processed)
      }
    }
  }
}

#[derive(Clone, Debug)]
enum ValidNodeDef<'a> {
  Node(Ident<'a>),
  Modified(Box<ValidNodeDef<'a>>, Modifier),
  StaticToken(Ident<'a>),
  DynamicToken(Ident<'a>),
  Group(Vec<ValidNodeDef<'a>>),
  Choice(Vec<ValidNodeDef<'a>>),
  Delimited(Box<ValidNodeDef<'a>>, Ident<'a>),
}

#[derive(Clone, Debug, Error)]
pub enum Error {
  #[error("unknown identifier: `{0}`")]
  UnknownIdent(String),
  #[error("unknown delimiter: `{0}`")]
  UnknownDelim(String),
}
