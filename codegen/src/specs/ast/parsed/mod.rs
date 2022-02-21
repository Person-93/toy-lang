use super::{Ident, Modifier};
use crate::collections::{NamedItem, NamedSet, Unnamed};
use std::{
  fmt::{self, Display, Formatter},
  ops::{Deref, DerefMut},
};

mod transform;

#[derive(Debug, Clone)]
pub struct Ast<'a>(NamedSet<'a, Node<'a>>);

#[derive(Clone, Debug)]
pub struct Node<'n> {
  pub ident: Ident<'n>,
  pub kind: NodeKind<'n>,
}

#[derive(Clone, Debug)]
pub enum NodeKind<'n> {
  Node(Ident<'n>),
  StaticToken(Ident<'n>),
  DynamicToken(Ident<'n>),
  Group(Vec<NodeKind<'n>>),
  Choice(Vec<NodeKind<'n>>),
  Delimited(Box<NodeKind<'n>>, Ident<'n>),
  Modified(Box<NodeKind<'n>>, Modifier),
  Todo,
}

impl<'a, 'n: 'a> FromIterator<Node<'n>> for Ast<'a> {
  fn from_iter<T: IntoIterator<Item = Node<'n>>>(iter: T) -> Self {
    Ast(iter.into_iter().collect())
  }
}

impl<'a> DerefMut for Ast<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<'a> Deref for Ast<'a> {
  type Target = NamedSet<'a, Node<'a>>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Display for Node<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.ident, self.kind)
  }
}

impl Display for NodeKind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      NodeKind::Node(ident) | NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => {
        write!(f, "{ident}")
      }
      NodeKind::Group(group) => write!(
        f,
        "{}",
        group
          .iter()
          .map(|child| match child {
            NodeKind::Group(_) | NodeKind::Choice(_) => format!("({child})"),
            NodeKind::Node(_)
            | NodeKind::StaticToken(_)
            | NodeKind::DynamicToken(_)
            | NodeKind::Delimited(..)
            | NodeKind::Modified(..) => child.to_string(),
            NodeKind::Todo => String::from("!todo"),
          })
          .collect::<Vec<_>>()
          .join(" ")
      ),
      NodeKind::Choice(choices) => write!(
        f,
        "{}",
        choices
          .iter()
          .map(|child| match child {
            NodeKind::Group(_) | NodeKind::Choice(_) => format!("({child})"),
            NodeKind::Node(_)
            | NodeKind::StaticToken(_)
            | NodeKind::DynamicToken(_)
            | NodeKind::Delimited(..)
            | NodeKind::Modified(..) => child.to_string(),
            NodeKind::Todo => String::from("!todo"),
          })
          .collect::<Vec<_>>()
          .join(" | ")
      ),
      NodeKind::Delimited(inner, delimiter) => write!(f, "delim[{delimiter}]<{inner}>"),
      NodeKind::Modified(inner, modifier) => match inner.as_ref() {
        NodeKind::Group(_) | NodeKind::Choice(_) => write!(f, "({inner}){modifier}"),
        NodeKind::Node(_)
        | NodeKind::StaticToken(_)
        | NodeKind::DynamicToken(_)
        | NodeKind::Delimited(..) => write!(f, "{inner}{modifier}"),
        NodeKind::Modified(..) => unreachable!(),
        NodeKind::Todo => write!(f, "!todo"),
      },
      NodeKind::Todo => write!(f, "!todo"),
    }
  }
}

impl<'n> NamedItem<'n> for Node<'n> {
  type Unnamed = NodeKind<'n>;

  fn name(&self) -> &'n str {
    self.ident.0
  }

  fn dummy(name: &'n str) -> Self {
    Node {
      ident: Ident(name),
      kind: NodeKind::Node(Ident("")),
    }
  }
}

impl<'n> Unnamed<'n> for NodeKind<'n> {
  type Named = Node<'n>;

  fn add_name(self, name: &'n str) -> Self::Named {
    Node {
      ident: Ident(name),
      kind: self,
    }
  }
}
