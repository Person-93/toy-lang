use serde::Deserialize;
use std::{collections::BTreeMap, ops::Deref};

mod parse;

// TODO Ast struct should be a set of entries instead of a map

#[derive(Debug)]
pub struct Ast<'a>(BTreeMap<Ident<'a>, NodeDef<'a>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry<'a> {
  node_type: Ident<'a>,
  node_def: NodeDef<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum NodeDef<'a> {
  Simple(Ident<'a>),
  Modified {
    inner: Box<NodeDef<'a>>,
    modifier: Modifier,
  },
  Delimiter {
    delimiter: Ident<'a>,
    inner: Box<NodeDef<'a>>,
  },
  Group(Vec<NodeDef<'a>>),
  Choice {
    first: Box<NodeDef<'a>>,
    second: Box<NodeDef<'a>>,
  },
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Deserialize)]
pub struct Ident<'a>(&'a str);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Modifier {
  Repeat,
  Csv,
  OnePlus,
  CsvOnePlus,
  Optional,
}

impl<'a> AsRef<<Ident<'a> as Deref>::Target> for Ident<'a> {
  fn as_ref(&self) -> &str {
    self.0
  }
}

impl<'a> Deref for Ident<'a> {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    self.0
  }
}
