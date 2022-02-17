use serde::{
  de::{self, Error, MapAccess},
  Deserialize, Deserializer,
};
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::{
  any,
  fmt::{self, Formatter},
  marker::PhantomData,
};

#[derive(Debug)]
pub struct TokenSet<'t, T: Token<'t>>(BTreeSet<Wrapper<'t, T>>);

impl<'t, T: Token<'t>> Default for TokenSet<'t, T> {
  fn default() -> Self {
    Self(Default::default())
  }
}

impl<'t, T: Token<'t>> TokenSet<'t, T> {
  pub fn contains(&self, name: &'t str) -> bool {
    self.0.contains(&Wrapper(T::dummy(name), PhantomData))
  }

  pub fn iter(&'t self) -> impl Iterator<Item = T> + 't {
    self.0.iter().copied().map(|w| w.0)
  }
}

impl<'de: 't, 't, T: Token<'t>> Deserialize<'de> for TokenSet<'t, T> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    return deserializer.deserialize_map(Visitor(PhantomData, PhantomData));

    struct Visitor<'de: 't, 't, T>(PhantomData<&'t &'de ()>, PhantomData<T>);

    impl<'de: 't, 't, T: Token<'t>> de::Visitor<'de> for Visitor<'de, 't, T> {
      type Value = TokenSet<'t, T>;

      fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "a set of tokens")
      }

      fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
        let mut token_set = Self::Value::default();

        while let Some((name, value)) = map.next_entry()? {
          if !token_set
            .0
            .insert(Wrapper(T::new(name, value), PhantomData))
          {
            return Err(A::Error::custom(format!(
              "duplicate entry `{name}` in token set of type {}",
              any::type_name::<T>()
            )));
          }
        }

        Ok(token_set)
      }
    }
  }
}

#[derive(Copy, Clone, Debug)]
struct Wrapper<'t, T: Token<'t>>(T, PhantomData<&'t ()>);

impl<'t, T: Token<'t>> Ord for Wrapper<'t, T> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.0.name().cmp(other.0.name())
  }
}

impl<'t, T: Token<'t>> PartialOrd for Wrapper<'t, T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'t, T: Token<'t>> Eq for Wrapper<'t, T> {}

impl<'t, T: Token<'t>> PartialEq for Wrapper<'t, T> {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

pub trait Token<'t>: Copy {
  fn new(name: &'t str, value: &'t str) -> Self;
  fn name(&self) -> &'t str;
  fn dummy(name: &'t str) -> Self;
}

impl<'t, T: Token<'t>> TokenSet<'t, T> {
  pub fn get(&self, name: &'t str) -> Option<T> {
    self
      .0
      .get(&Wrapper(T::dummy(name), PhantomData))
      .copied()
      .map(|w| w.0)
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StaticToken<'t> {
  name: &'t str,
  symbol: &'t str,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct DynamicToken<'t> {
  name: &'t str,
  pattern: &'t str,
}

impl<'t> StaticToken<'t> {
  pub fn symbol(&self) -> &'t str {
    self.symbol
  }
}

impl<'t> Token<'t> for StaticToken<'t> {
  fn new(name: &'t str, value: &'t str) -> Self {
    Self {
      name,
      symbol: value,
    }
  }

  fn name(&self) -> &'t str {
    self.name
  }

  fn dummy(name: &'t str) -> Self {
    Self { name, symbol: "" }
  }
}

impl DynamicToken<'_> {
  pub fn pattern(&self) -> &str {
    self.pattern
  }
}

impl<'t> Token<'t> for DynamicToken<'t> {
  fn new(name: &'t str, value: &'t str) -> Self {
    Self {
      name,
      pattern: value,
    }
  }

  fn name(&self) -> &'t str {
    self.name
  }

  fn dummy(name: &'t str) -> Self {
    Self { name, pattern: "" }
  }
}
