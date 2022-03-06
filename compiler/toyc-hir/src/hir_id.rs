use core::{cell::Cell, marker::PhantomData};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HirId<'hir>(u32, PhantomData<*const ()>, PhantomData<&'hir ()>);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BodyId<'hir>(HirId<'hir>);

impl<'hir> HirId<'hir> {
  pub const ROOT_ID: HirId<'static> = HirId(0, PhantomData, PhantomData);

  pub fn to_body_id(self) -> BodyId<'hir> {
    BodyId(self)
  }
}

#[derive(Debug)]
pub struct HirIdFactory<'hir>(
  Cell<u32>,
  PhantomData<*const ()>,
  PhantomData<&'hir ()>,
);

impl Default for HirIdFactory<'_> {
  fn default() -> Self {
    Self::new()
  }
}

impl<'hir> HirIdFactory<'hir> {
  pub fn new() -> HirIdFactory<'hir> {
    HirIdFactory::LOCK.with(|flag| {
      if flag.get() {
        panic!("attempted to make a second hir id factory on the same thread");
      } else {
        flag.set(true);
      }
    });
    HirIdFactory(Cell::new(0), PhantomData, PhantomData)
  }

  pub fn next_id(&self) -> HirId<'hir> {
    let id = self.0.get();
    assert!(id < u32::MAX, "hir id overflowed");
    let id = id + 1;
    self.0.set(id);
    HirId(id, PhantomData, PhantomData)
  }

  thread_local! {
    static LOCK: Cell<bool> = Cell::new(false);
  }
}

impl Drop for HirIdFactory<'_> {
  fn drop(&mut self) {
    HirIdFactory::LOCK.with(|flag| flag.set(false));
  }
}
