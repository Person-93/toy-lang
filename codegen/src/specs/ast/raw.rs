use super::{lex::Lexer, Ident, Modifier};
use crate::collections::{NamedItem, NamedSet, Unnamed};
use std::ops::DerefMut;
use std::{
  fmt::{Display, Formatter},
  ops::{Deref, Range},
};
use thiserror::Error;

#[derive(Debug)]
pub struct Ast<'a>(NamedSet<'a, Entry<'a>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry<'a> {
  pub ident: Ident<'a>,
  pub node_def: NodeDef<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum NodeDef<'a> {
  Simple(Ident<'a>),
  Modified(Box<NodeDef<'a>>, Modifier),
  Delimiter(Box<NodeDef<'a>>, Ident<'a>),
  Group(Vec<NodeDef<'a>>),
  Choice(Box<NodeDef<'a>>, Box<NodeDef<'a>>),
}

impl<'a> DerefMut for Ast<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<'a> Deref for Ast<'a> {
  type Target = NamedSet<'a, Entry<'a>>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Ast<'_> {
  pub fn parse(text: &str) -> Result<Ast, Error> {
    let mut ast = Ast(Default::default());
    let mut fragment: Option<Range<_>> = None;

    for range in LineRanges::new(text) {
      let range = match &fragment {
        Some(f) => f.start..range.end,
        None => range,
      };
      let line = &text[range.clone()];
      if line.starts_with('#') || line.is_empty() {
        continue;
      };
      if line.ends_with('\\') {
        fragment = Some(range);
        continue;
      }

      if !ast.insert(Entry::parse(line)?) {
        return Err(Error {
          text: String::from(line),
          index: 0,
          kind: ErrorKind::Duplicate,
        });
      }

      fragment = None;
    }

    Ok(ast)
  }
}

impl Entry<'_> {
  fn parse(line: &str) -> Result<Entry, Error> {
    let buffer = Lexer::new(line);
    let node_type = Ident::parse(&buffer)?;

    buffer.skip_whitespace();
    if !buffer.parse_literal("=") {
      return Err(Error {
        text: String::from(line),
        index: 0,
        kind: ErrorKind::MissingEquals,
      });
    }
    buffer.skip_whitespace();

    let node_def = NodeDef::parse(&buffer)?;

    Ok(Entry {
      ident: node_type,
      node_def,
    })
  }
}

impl<'e> NamedItem<'e> for Entry<'e> {
  type Unnamed = NodeDef<'e>;

  fn name(&self) -> &'e str {
    self.ident.0
  }

  fn dummy(name: &'e str) -> Self {
    Entry {
      ident: Ident(name),
      node_def: NodeDef::Simple(Ident("")),
    }
  }
}

impl<'e> Unnamed<'e> for NodeDef<'e> {
  type Named = Entry<'e>;

  fn add_name(self, name: &'e str) -> Self::Named {
    Entry {
      ident: Ident(name),
      node_def: self,
    }
  }
}

impl NodeDef<'_> {
  fn parse<'p>(lexer: &Lexer<'p>) -> Result<NodeDef<'p>, Error> {
    let mut defs = Vec::new();
    while matches!(lexer.peek(), Some(c) if !IDENT_TERMINALS.contains(&c)) {
      if lexer.parse_literal("|") {
        lexer.skip_whitespace();
        return Ok(NodeDef::Choice(
          Box::new(NodeDef::from_vec(lexer, defs)?),
          Box::new(NodeDef::parse(lexer)?),
        ));
      }

      let def = if lexer.parse_literal("(") {
        let node_def = NodeDef::parse(lexer)?;
        if lexer.parse_literal(")") {
          node_def
        } else {
          return Err(Error {
            text: String::from(lexer.text()),
            index: lexer.index(),
            kind: ErrorKind::MismatchedDelimiter,
          });
        }
      } else if lexer.parse_literal("delim[") {
        let ident = Ident::parse(lexer)?;

        if !lexer.parse_literal("]<") {
          return Err(Error {
            text: String::from(ident.0),
            index: lexer.index(),
            kind: ErrorKind::MismatchedDelimiter,
          });
        }

        let node_def = NodeDef::parse(lexer)?;

        if !lexer.parse_literal(">") {
          return Err(Error {
            text: String::from(lexer.extract().unwrap()),
            index: lexer.index(),
            kind: ErrorKind::MismatchedDelimiter,
          });
        }

        NodeDef::Delimiter(Box::new(node_def), ident)
      } else {
        NodeDef::Simple(Ident::parse(lexer)?)
      };

      defs.push(match Modifier::parse(lexer) {
        Some(modifier) => NodeDef::Modified(Box::new(def), modifier),
        None => def,
      });

      lexer.skip_whitespace();
    }

    NodeDef::from_vec(lexer, defs)
  }

  fn from_vec<'a>(buffer: &Lexer<'a>, mut defs: Vec<NodeDef<'a>>) -> Result<NodeDef<'a>, Error> {
    match defs.len() {
      0 => Err(Error {
        text: String::from(buffer.text()),
        index: buffer.index() - 1,
        kind: ErrorKind::MissingNodeDef,
      }),
      1 => Ok(defs.pop().unwrap()),
      _ => Ok(NodeDef::Group(defs)),
    }
  }
}

impl Modifier {
  fn parse(lexer: &Lexer) -> Option<Modifier> {
    if lexer.parse_literal("*") {
      Some(Modifier::Repeat)
    } else if lexer.parse_literal(",*") {
      Some(Modifier::Csv)
    } else if lexer.parse_literal("+") {
      Some(Modifier::OnePlus)
    } else if lexer.parse_literal(",+") {
      Some(Modifier::CsvOnePlus)
    } else if lexer.parse_literal("?") {
      Some(Modifier::Optional)
    } else {
      None
    }
  }
}

impl Ident<'_> {
  fn parse<'p>(lexer: &Lexer<'p>) -> Result<Ident<'p>, Error> {
    lexer.bump_while(|c| c.is_alphanumeric() || c == '_');
    if lexer.peek().map_or(true, |c| IDENT_TERMINALS.contains(&c)) {
      Ok(Ident(lexer.extract().unwrap()))
    } else {
      lexer.bump_while(|c| c != ' ');
      Err(Error {
        text: String::from(lexer.extract().unwrap_or_else(|| lexer.text())),
        index: lexer.index(),
        kind: ErrorKind::InvalidIdent,
      })
    }
  }
}

impl Display for Ident<'_> {
  #[inline(always)]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Display::fmt(self.0, f)
  }
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

const IDENT_TERMINALS: &[char] = &[' ', ',', '+', '*', '?', ')', ']', '>'];

struct LineRanges<'a> {
  text: &'a str,
  index: usize,
}

impl LineRanges<'_> {
  fn new(text: &str) -> LineRanges {
    LineRanges { text, index: 0 }
  }
}

impl Iterator for LineRanges<'_> {
  type Item = Range<usize>;

  fn next(&mut self) -> Option<Self::Item> {
    (self.text.len() > self.index).then(|| {
      match self.text[self.index..self.text.len()].find('\n') {
        Some(i) => {
          let r = self.index..self.index + i;
          self.index += i + 1;
          r
        }
        None => {
          let r = self.index..self.text.len();
          self.index = self.text.len();
          r
        }
      }
    })
  }
}

#[derive(Clone, Debug, Error)]
#[error("{kind}: {index}: {text}")]
pub struct Error {
  text: String,
  index: usize,
  kind: ErrorKind,
}

#[derive(Copy, Clone, Debug)]
enum ErrorKind {
  InvalidIdent,
  MismatchedDelimiter,
  MissingEquals,
  MissingNodeDef,
  Duplicate,
}

impl Display for ErrorKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ErrorKind::InvalidIdent => write!(f, "invalid identifier"),
      ErrorKind::MismatchedDelimiter => write!(f, "mismatched delimiter"),
      ErrorKind::MissingEquals => write!(f, "expected '='"),
      ErrorKind::MissingNodeDef => write!(f, "expected node definition"),
      ErrorKind::Duplicate => write!(f, "duplicate key"),
    }
  }
}
