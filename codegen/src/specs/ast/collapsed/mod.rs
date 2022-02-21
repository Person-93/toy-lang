use super::{Ident, Modifier};
use crate::collections::{NamedItem, NamedSet, Unnamed};
use std::{
  fmt::{self, Display, Formatter},
  ops::Deref,
};

mod transform;

#[derive(Debug, Clone)]
pub struct Ast<'a> {
  pub nodes: NamedSet<'a, Node<'a>>,
  pub cyclic: Vec<bool>,
}

#[derive(Debug, Clone)]
pub struct Node<'a> {
  pub ident: Ident<'a>,
  pub kind: NodeKind<'a>,
}

#[derive(Clone, Debug)]
pub enum NodeKind<'n> {
  Node(Ident<'n>),
  StaticToken(Ident<'n>),
  DynamicToken(Ident<'n>),
  Group(Group<'n>),
  Choice(Choice<'n>),
  Delimited(Box<NodeKind<'n>>, Ident<'n>),
  Modified(Box<NodeKind<'n>>, Modifier),
  Todo,
}

#[derive(Clone, Debug)]
pub struct Group<'g> {
  pub members: Vec<Node<'g>>,
  pub kind: GroupKind,
  pub inline: bool,
}

#[derive(Clone, Debug)]
pub enum GroupKind {
  Zero,
  One(usize),
  Many(Vec<usize>),
}

#[derive(Clone, Debug)]
pub struct Choice<'c> {
  pub kind: ChoiceKind<'c>,
  pub inline: bool,
}

#[derive(Clone, Debug)]
pub enum ChoiceKind<'c> {
  Regular(Vec<Node<'c>>),
  Option {
    primary: Box<Node<'c>>,
    secondary: Ident<'c>,
  },
}

impl<'a> Deref for Ast<'a> {
  type Target = NamedSet<'a, Node<'a>>;

  fn deref(&self) -> &Self::Target {
    &self.nodes
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
      kind: NodeKind::StaticToken(Ident("")),
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

impl Display for NodeKind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      NodeKind::Node(ident) | NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => {
        write!(f, "{ident}")
      }
      NodeKind::Group(nodes) => write!(
        f,
        "{}",
        nodes
          .members
          .iter()
          .map(|child| match &child.kind {
            NodeKind::Group(_) | NodeKind::Choice(_) => format!("({})", child.ident),
            NodeKind::Node(_)
            | NodeKind::StaticToken(_)
            | NodeKind::DynamicToken(_)
            | NodeKind::Delimited(..)
            | NodeKind::Modified(..) => child.ident.to_string(),
            NodeKind::Todo => String::from("!todo"),
          })
          .collect::<Vec<_>>()
          .join(" ")
      ),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        write!(
          f,
          "{}",
          choices
            .iter()
            .map(|child| match &child.kind {
              NodeKind::Group(_) | NodeKind::Choice(_) => format!("({})", child.ident),
              NodeKind::Node(_)
              | NodeKind::StaticToken(_)
              | NodeKind::DynamicToken(_)
              | NodeKind::Delimited(..)
              | NodeKind::Modified(..) => child.ident.to_string(),
              NodeKind::Todo => String::from("!todo"),
            })
            .collect::<Vec<_>>()
            .join(" | ")
        )
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { primary, secondary },
        inline: _,
      }) => {
        write!(f, "{} | {}", primary.ident, secondary.0)
      }
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
