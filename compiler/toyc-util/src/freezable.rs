use core::{
  cell::{Ref, RefCell, RefMut},
  ops::Deref,
};

#[derive(Clone, Debug)]
pub enum Freezable<T> {
  Frozen(T),
  Unfrozen(RefCell<T>),
}

impl<T: Default> Default for Freezable<T> {
  fn default() -> Self {
    Freezable::Unfrozen(Default::default())
  }
}

impl<T> Freezable<T> {
  pub fn borrow(&self) -> FreezableRef<'_, T> {
    match self {
      Freezable::Frozen(object) => FreezableRef::Frozen(object),
      Freezable::Unfrozen(cell) => FreezableRef::Unfrozen(cell.borrow()),
    }
  }

  pub fn borrow_mut(&self) -> RefMut<'_, T> {
    match self {
      Freezable::Frozen(_) => panic!("tried to borrow a frozen object"),
      Freezable::Unfrozen(cell) => cell.borrow_mut(),
    }
  }

  pub fn as_frozen(&self) -> &T {
    match self {
      Freezable::Frozen(object) => object,
      Freezable::Unfrozen(_) => {
        panic!("tried to borrow an unfrozen object as frozen")
      }
    }
  }

  pub fn as_frozen_mut(&mut self) -> &mut T {
    match self {
      Freezable::Frozen(object) => object,
      Freezable::Unfrozen(_) => {
        panic!("tried to borrow an unfrozen object as frozen")
      }
    }
  }

  pub fn freeze(self) -> Self {
    match self {
      Freezable::Frozen(_) => self,
      Freezable::Unfrozen(cell) => Freezable::Frozen(cell.into_inner()),
    }
  }

  pub fn freeze_in_place(&mut self)
  where
    T: Default,
  {
    match self {
      Freezable::Frozen(_) => (),
      Freezable::Unfrozen(cell) => *self = Freezable::Frozen(cell.take()),
    }
  }
}

#[derive(Debug)]
pub enum FreezableRef<'a, T> {
  Frozen(&'a T),
  Unfrozen(Ref<'a, T>),
}

impl<T> FreezableRef<'_, T> {
  #[allow(clippy::should_implement_trait)]
  pub fn clone(original: &Self) -> Self {
    match original {
      FreezableRef::Frozen(object) => FreezableRef::Frozen(*object),
      FreezableRef::Unfrozen(cell) => FreezableRef::Unfrozen(Ref::clone(cell)),
    }
  }
}

impl<'a, T> Deref for FreezableRef<'a, T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    match self {
      FreezableRef::Frozen(object) => *object,
      FreezableRef::Unfrozen(cell) => &**cell,
    }
  }
}
