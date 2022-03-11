#![forbid(unsafe_code)]
#![no_std]

// TODO check types
// TODO create structures to represent type-checked things?

extern crate alloc;

use crate::definitions::Definitions;
use alloc::rc::Rc;
use toyc_hir::HirContext;
use toyc_session::Session;

pub mod definitions;
mod undefined;

pub fn check_for_undefined(
  session: &Session,
  context: &HirContext<'_>,
  definitions: Rc<Definitions<'_, '_>>,
) {
  crate::undefined::check_items(
    session,
    context,
    definitions,
    context.root().items,
  );
}
