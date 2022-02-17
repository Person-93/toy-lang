use super::{Ast, Entry, Ident, Modifier, NodeDef};
use std::{
  cell::Cell,
  collections::btree_map::Entry as MapEntry,
  fmt::{Display, Formatter},
  ops::Range,
};
use thiserror::Error;

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

      let Entry {
        node_type,
        node_def,
      } = Entry::parse(line)?;

      match ast.0.entry(node_type) {
        MapEntry::Vacant(entry) => {
          entry.insert(node_def);
        }
        MapEntry::Occupied(entry) => {
          return Err(Error {
            text: String::from(entry.key().as_ref()),
            index: 0,
            kind: ErrorKind::Duplicate,
          })
        }
      }
      fragment = None;
    }

    Ok(ast)
  }
}

impl Entry<'_> {
  fn parse(line: &str) -> Result<Entry, Error> {
    let buffer = ParseBuffer::new(line);
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
      node_type,
      node_def,
    })
  }
}

impl Ident<'_> {
  fn parse<'p>(buffer: &ParseBuffer<'p>) -> Result<Ident<'p>, Error> {
    buffer.bump_while(|c| c.is_alphanumeric() || c == '_');
    if buffer.peek().map_or(true, |c| IDENT_TERMINALS.contains(&c)) {
      Ok(Ident(buffer.extract().unwrap()))
    } else {
      buffer.bump_while(|c| c != ' ');
      Err(Error {
        text: String::from(buffer.extract().unwrap_or(buffer.text)),
        index: buffer.index.get(),
        kind: ErrorKind::InvalidIdent,
      })
    }
  }
}

impl NodeDef<'_> {
  fn parse<'p>(buffer: &ParseBuffer<'p>) -> Result<NodeDef<'p>, Error> {
    let mut defs = Vec::new();
    while matches!(buffer.peek(), Some(c) if !IDENT_TERMINALS.contains(&c)) {
      if buffer.parse_literal("|") {
        buffer.skip_whitespace();
        return Ok(NodeDef::Choice {
          first: Box::new(NodeDef::from_vec(buffer, defs)?),
          second: Box::new(NodeDef::parse(buffer)?),
        });
      }

      let def = if buffer.parse_literal("(") {
        let node_def = NodeDef::parse(buffer)?;
        if buffer.parse_literal(")") {
          node_def
        } else {
          return Err(Error {
            text: String::from(buffer.text),
            index: buffer.index.get(),
            kind: ErrorKind::MismatchedDelimiter,
          });
        }
      } else if buffer.parse_literal("delim[") {
        let ident = Ident::parse(buffer)?;

        if !buffer.parse_literal("]<") {
          return Err(Error {
            text: String::from(ident.0),
            index: buffer.index.get(),
            kind: ErrorKind::MismatchedDelimiter,
          });
        }

        let node_def = NodeDef::parse(buffer)?;

        if !buffer.parse_literal(">") {
          return Err(Error {
            text: String::from(buffer.extract().unwrap()),
            index: buffer.index.get(),
            kind: ErrorKind::MismatchedDelimiter,
          });
        }

        NodeDef::Delimiter {
          delimiter: ident,
          inner: Box::new(node_def),
        }
      } else {
        NodeDef::Simple(Ident::parse(buffer)?)
      };

      defs.push(match parse_modifier(buffer) {
        Some(modifier) => NodeDef::Modified {
          inner: Box::new(def),
          modifier,
        },
        None => def,
      });

      buffer.skip_whitespace();
    }

    NodeDef::from_vec(buffer, defs)
  }

  fn from_vec<'a>(
    buffer: &ParseBuffer<'a>,
    mut defs: Vec<NodeDef<'a>>,
  ) -> Result<NodeDef<'a>, Error> {
    match defs.len() {
      0 => Err(Error {
        text: String::from(buffer.text),
        index: buffer.index.get() - 1,
        kind: ErrorKind::MissingNodeDef,
      }),
      1 => Ok(defs.pop().unwrap()),
      _ => Ok(NodeDef::Group(defs)),
    }
  }
}

fn parse_modifier(buffer: &ParseBuffer) -> Option<Modifier> {
  if buffer.parse_literal("*") {
    Some(Modifier::Repeat)
  } else if buffer.parse_literal(",*") {
    Some(Modifier::Csv)
  } else if buffer.parse_literal("+") {
    Some(Modifier::OnePlus)
  } else if buffer.parse_literal(",+") {
    Some(Modifier::CsvOnePlus)
  } else if buffer.parse_literal("?") {
    Some(Modifier::Optional)
  } else {
    None
  }
}

#[derive(Clone)]
struct ParseBuffer<'p> {
  text: &'p str,
  start: Cell<usize>,
  index: Cell<usize>,
}

impl<'p> ParseBuffer<'p> {
  fn new(text: &str) -> ParseBuffer {
    ParseBuffer {
      text,
      start: Cell::new(0),
      index: Cell::new(0),
    }
  }

  fn extract(&self) -> Option<&'p str> {
    let val = (self.start < self.index).then(|| &self.text[self.start.get()..self.index.get()]);
    self.start.set(self.index.get());
    val
  }

  fn peek(&self) -> Option<char> {
    (self.index.get() < self.text.len()).then(|| self.text.as_bytes()[self.index.get()] as char)
  }

  fn skip_whitespace(&self) {
    self.bump_while(|c| c == ' ' || c == '\\' || c == '\n');
    self.start.set(self.index.get());
  }

  #[must_use]
  fn parse_literal(&self, value: &str) -> bool {
    let original = self.index.get();
    for i in 0..value.len() {
      if self
        .peek()
        .map_or(false, |c| value.as_bytes()[i] as char == c)
      {
        self.increment();
      } else {
        self.index.set(original);
        return false;
      }
    }
    self.start.set(self.index.get());
    true
  }

  fn bump_while(&self, f: fn(char) -> bool) {
    while self.peek().map_or(false, f) {
      self.increment()
    }
  }

  fn increment(&self) {
    self.index.set(self.index.get() + 1);
  }
}

const IDENT_TERMINALS: &[char] = &[' ', ',', '+', '*', '?', ')', ']', '>'];

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
