use super::{
  super::parsed::{Ast as ParsedAst, NodeKind as ParsedNodeKind},
  Ast, Choice, Group, GroupKind, Ident, Node, NodeKind,
};
use std::fmt::Display;
use thiserror::Error;

impl<'a> ParsedAst<'a> {
  pub fn transform(&'a self) -> Result<Ast<'a>, Error> {
    Ok(Ast(
      self
        .iter()
        .map(|node| {
          self
            .transform_node_kind(&node.kind, Some(node.ident))
            .and_then(|node| Ok(Node::try_from(node)?))
            .map_err(|err| err.context(format!("failed to parse node: {node}")))
        })
        .collect::<Result<_, _>>()?,
    ))
  }

  fn transform_node_kind(
    &'a self,
    kind: &'a ParsedNodeKind<'a>,
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    match kind {
      ParsedNodeKind::Node(child) => {
        let ident = self.get(child.0).unwrap().ident;
        Ok((NodeKind::Node(ident), hint))
      }
      ParsedNodeKind::StaticToken(ident) => Ok((NodeKind::StaticToken(*ident), hint)),
      ParsedNodeKind::DynamicToken(ident) => Ok((NodeKind::DynamicToken(*ident), hint)),
      ParsedNodeKind::Group(nodes) => self.transform_group(nodes, hint),
      ParsedNodeKind::Choice(nodes) => self.transform_choice(nodes, hint),
      ParsedNodeKind::Delimited(inner, delimiter) => {
        self.transform_delimited(*delimiter, inner, hint)
      }
      ParsedNodeKind::Modified(inner, modifier) => self
        .transform_node_kind(inner, hint)
        .map(|(inner, hint)| (NodeKind::Modified(Box::new(inner), *modifier), hint)),
    }
  }

  fn transform_group(
    &'a self,
    nodes: &'a [ParsedNodeKind<'a>],
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    let nodes: Vec<_> = nodes
      .iter()
      .map(|child| self.transform_node_kind(child, None))
      .collect::<Result<_, _>>()
      .map_err(|err| {
        err.context(format!(
          "failed to collapse group: {}",
          nodes
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
        ))
      })?;

    let indices: Vec<_> = nodes
      .iter()
      .enumerate()
      .filter_map(|(idx, (kind, _))| {
        return get_index(kind, idx);
        fn get_index(kind: &NodeKind, idx: usize) -> Option<usize> {
          match kind {
            NodeKind::StaticToken(_) => None,
            NodeKind::Node(_) | NodeKind::DynamicToken(_) | NodeKind::Choice(_) => Some(idx),
            NodeKind::Group(Group { kind, members: _ }) => match kind {
              GroupKind::Zero => None,
              GroupKind::One(_) => Some(idx),
              GroupKind::Many(..) => Some(idx),
            },
            NodeKind::Delimited(inner, _) | NodeKind::Modified(inner, _) => get_index(inner, idx),
          }
        }
      })
      .collect();

    let (kind, generated_hint) = match indices.len() {
      0 => (GroupKind::Zero, None),
      1 => (GroupKind::One(indices[0]), nodes[indices[0]].1),
      _ => (GroupKind::Many(indices), None),
    };

    Ok((
      NodeKind::Group(Group {
        members: nodes
          .into_iter()
          .map(|(kind, hint)| {
            Ok(match hint {
              Some(ident) => Node { ident, kind },
              None => Node::try_from((kind, hint))?,
            })
          })
          .collect::<Result<_, Error>>()?,
        kind,
      }),
      hint.or(generated_hint),
    ))
  }

  fn transform_choice(
    &'a self,
    nodes: &'a [ParsedNodeKind<'a>],
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    let nodes: Vec<_> = nodes
      .iter()
      .map(|node| Ok(Node::try_from(self.transform_node_kind(node, None)?)?))
      .collect::<Result<_, Error>>()
      .map_err(|err| {
        err.context(format!(
          "failed to collapse choice: {}",
          nodes
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" | ")
        ))
      })?;

    Ok((
      NodeKind::Choice(match nodes.len() {
        0 | 1 => unreachable!(),
        2 => match (&nodes[0].kind, &nodes[1].kind) {
          (NodeKind::StaticToken(_), NodeKind::StaticToken(_)) => Choice::Regular(nodes),
          (NodeKind::StaticToken(ident), _) => Choice::Option {
            secondary: *ident,
            primary: Box::new(nodes.into_iter().nth(1).unwrap()),
          },
          (_, NodeKind::StaticToken(ident)) => Choice::Option {
            secondary: *ident,
            primary: Box::new(nodes.into_iter().next().unwrap()),
          },
          (_, _) => Choice::Regular(nodes),
        },
        _ => Choice::Regular(nodes),
      }),
      hint,
    ))
  }

  fn transform_delimited(
    &'a self,
    delimiter: Ident<'a>,
    inner: &'a ParsedNodeKind<'a>,
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    self
      .transform_node_kind(inner, hint)
      .map(|(inner, hint)| (NodeKind::Delimited(Box::new(inner), delimiter), hint))
  }
}

#[derive(Clone, Debug, Error)]
pub enum Error {
  #[error(transparent)]
  MissingName(#[from] MissingName),
  #[error("{1}")]
  Context(#[source] Box<Error>, String),
}

impl Error {
  fn context<T: Display>(self, message: T) -> Self {
    Error::Context(Box::new(self), message.to_string())
  }
}

impl<'n> TryFrom<(NodeKind<'n>, Option<Ident<'n>>)> for Node<'n> {
  type Error = MissingName;

  fn try_from((kind, hint): (NodeKind<'n>, Option<Ident<'n>>)) -> Result<Self, Self::Error> {
    match hint {
      Some(ident) => Ok(Node { ident, kind }),
      None => match kind {
        NodeKind::Node(ident) | NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => {
          Ok(Node { ident, kind })
        }
        NodeKind::Group(ref group) => match &group.kind {
          GroupKind::Zero => Err(MissingName::new("failed to name zero sized group", &kind)),
          GroupKind::One(idx) => Ok(Node {
            ident: group.members.get(*idx).unwrap().ident,
            kind,
          }),
          GroupKind::Many(_) => Err(MissingName::new("failed to name group", &kind)),
        },
        NodeKind::Choice(Choice::Regular(_)) => {
          Err(MissingName::new("failed to name choice", &kind))
        }
        NodeKind::Choice(Choice::Option {
          ref primary,
          secondary: _,
        }) => Ok(Node {
          ident: primary.ident,
          kind,
        }),
        NodeKind::Delimited(inner, delimiter) => Node::try_from((*inner, hint)).map(|node| Node {
          ident: node.ident,
          kind: NodeKind::Delimited(Box::new(node.kind), delimiter),
        }),
        NodeKind::Modified(inner, modifier) => Node::try_from((*inner, hint)).map(|node| Node {
          ident: node.ident,
          kind: NodeKind::Modified(Box::new(node.kind), modifier),
        }),
      },
    }
  }
}

#[derive(Clone, Debug, Error)]
#[error("{0}")]
pub struct MissingName(String);

impl MissingName {
  fn new<T: Display>(message: T, node: &NodeKind<'_>) -> Self {
    Self(format!("{message}: {node}"))
  }
}
