use core::{cell::Cell, marker::PhantomData};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HirId<'hir>(u32, PhantomData<*const ()>, PhantomData<&'hir ()>);

#[derive(Debug, Eq, PartialEq)]
pub struct BodyId<'hir>(HirId<'hir>);

impl<'hir> HirId<'hir> {
  pub const ROOT_ID: HirId<'static> = HirId(0, PhantomData, PhantomData);

  pub(crate) fn to_body_id(self) -> BodyId<'hir> {
    BodyId(self)
  }
}

#[derive(Debug)]
pub struct HirIdFactory(Cell<u32>, PhantomData<*const ()>);

impl Default for HirIdFactory {
  fn default() -> Self {
    Self::new()
  }
}

impl HirIdFactory {
  pub fn new() -> HirIdFactory {
    HirIdFactory::LOCK.with(|flag| {
      if flag.get() {
        panic!("attempted to make a second hir id factory on the same thread");
      } else {
        flag.set(true);
      }
    });
    HirIdFactory(Cell::new(0), PhantomData)
  }

  pub fn next_id(&self) -> HirId {
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

impl Drop for HirIdFactory {
  fn drop(&mut self) {
    HirIdFactory::LOCK.with(|flag| flag.set(false));
  }
}
