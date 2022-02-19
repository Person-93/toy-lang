use indexmap::IndexSet;
use std::marker::PhantomData;

mod impls;

#[derive(Debug, Clone)]
pub struct NamedSet<'ni, T: NamedItem<'ni>>(IndexSet<Wrapper<'ni, T>>);

#[derive(Debug, Clone)]
struct Wrapper<'ni, T: NamedItem<'ni>>(T, PhantomData<&'ni ()>);

pub trait NamedItem<'ni> {
  type Unnamed: Unnamed<'ni, Named = Self>;

  fn name(&self) -> &'ni str;
  fn dummy(name: &'ni str) -> Self;
}

pub trait Unnamed<'ni> {
  type Named: NamedItem<'ni>;

  fn add_name(self, name: &'ni str) -> Self::Named;
}

impl<'ni, T: NamedItem<'ni>> NamedSet<'ni, T> {
  pub fn insert(&mut self, item: T) -> bool {
    self.0.insert(Wrapper::new(item))
  }

  pub fn get(&self, name: &'ni str) -> Option<&T> {
    self
      .0
      .get(&Wrapper::new(T::dummy(name)))
      .map(|wrapper| &wrapper.0)
  }

  pub fn contains(&self, name: &'ni str) -> bool {
    self.0.contains(&Wrapper(T::dummy(name), PhantomData))
  }

  pub fn iter(&'ni self) -> impl Iterator<Item = &'ni T> + 'ni {
    self.0.iter().map(|wrapper| &wrapper.0)
  }
}

impl<'ni, T: NamedItem<'ni>> Wrapper<'ni, T> {
  fn new(item: T) -> Self {
    Self(item, PhantomData)
  }
}
