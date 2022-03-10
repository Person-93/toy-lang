#![forbid(unsafe_code)]
#![no_std]

use toyc_hir::HirContext;
use toyc_session::Session;

pub fn check_package(_session: &Session, _context: &HirContext<'_>) {
  // TODO resolve imports
  // TODO check for use of undefined symbols
  // TODO check types
  // TODO create structures to represent type-checked things?
}
