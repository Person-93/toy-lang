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
  pub tag: Option<Ident<'a>>,
}

#[derive(Debug, Clone)]
pub struct TaggedNodeKind<'n> {
  pub kind: NodeKind<'n>,
  pub tag: Option<Ident<'n>>,
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

impl Display for Node<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.kind)?;
    if let Some(tag) = self.tag {
      write!(f, ":{tag}")?;
    }
    Ok(())
  }
}

impl<'a> Deref for Ast<'a> {
  type Target = NamedSet<'a, Node<'a>>;

  fn deref(&self) -> &Self::Target {
    &self.nodes
  }
}

impl<'n> NamedItem<'n> for Node<'n> {
  type Unnamed = TaggedNodeKind<'n>;

  fn name(&self) -> &'n str {
    self.ident.0
  }

  fn dummy(name: &'n str) -> Self {
    Node {
      ident: Ident(name),
      kind: NodeKind::StaticToken(Ident("")),
      tag: None,
    }
  }
}

impl<'n> Unnamed<'n> for TaggedNodeKind<'n> {
  type Named = Node<'n>;

  fn add_name(self, name: &'n str) -> Self::Named {
    Node {
      ident: Ident(name),
      kind: self.kind,
      tag: self.tag,
    }
  }
}

impl Display for TaggedNodeKind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.kind)?;
    if let Some(tag) = self.tag {
      write!(f, ":{tag}")?;
    }
    Ok(())
  }
}

impl Display for NodeKind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      NodeKind::Node(ident) | NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => {
        write!(f, "{ident}")
      }
      NodeKind::Group(Group {
        members,
        kind: _,
        inline,
      }) => {
        if *inline {
          write!(f, "(")?;
        }

        let mut members = members.iter();
        write!(f, "{}", members.next().unwrap())?;
        for member in members {
          write!(f, " {member}")?;
        }

        if *inline {
          write!(f, ")")?;
        }

        Ok(())
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline,
      }) => {
        if *inline {
          write!(f, "(")?;
        }

        let mut choices = choices.iter();
        write!(f, "{}", choices.next().unwrap())?;

        if *inline {
          write!(f, ")")?;
        }
        for choice in choices {
          write!(f, " | {choice}")?;
        }

        Ok(())
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { primary, secondary },
        inline: _,
      }) => {
        write!(f, "{} | {}", primary.ident, secondary.0)
      }
      NodeKind::Delimited(inner, delimiter) => write!(f, "delim[{delimiter}]<{inner}>"),
      NodeKind::Modified(inner, modifier) => write!(f, "{inner}{modifier}"),
      NodeKind::Todo => write!(f, "!todo"),
    }
  }
}
