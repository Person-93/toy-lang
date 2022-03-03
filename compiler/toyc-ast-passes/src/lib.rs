use toyc_ast::{
  ast::{AttrData, Function, Item, VisKind},
  visitor::{self, Visitor},
};
use toyc_errors::Handler;

pub fn check_package(handler: &Handler, attrs: &[AttrData], items: &[Item]) {
  let mut validator = AstValidator { handler };
  validator.walk_package(attrs, items);
}

struct AstValidator<'a> {
  handler: &'a Handler,
}

impl Visitor for AstValidator<'_> {
  fn walk_function(
    &mut self,
    attrs: &[AttrData],
    visibility: Option<Option<&VisKind>>,
    function: &Function,
  ) {
    if function.extern_.is_none() && function.body.is_none() {
      self
        .handler
        .error("missing function body")
        .add_span(
          function.span.shrink_to_hi(),
          "consider replacing this semicolon with a function body",
        )
        .attach_note("only extern function can be declared without a body")
        .emit();
    }
    if let Some(abi) = &function.extern_ {
      if abi != "C" {
        self
          .handler
          .error(format!("'{abi}' is not a recognized ABI"))
          .emit();
      }
    }
    visitor::walk_function(self, attrs, visibility, function);
  }
}
