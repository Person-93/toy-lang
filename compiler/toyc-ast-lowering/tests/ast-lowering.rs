use test_util::{assert_debug_snapshot, snapshots, TestSession};
use toyc_ast_lowering::HirContext;

fn main() {
  snapshots(|text| {
    let session = TestSession::new("test-ast-lowering");
    let ast = toyc_ast::parse_single_file(text, &session.handler).unwrap();
    let ctx = HirContext::new(&session);
    assert_debug_snapshot!(ctx.lower_package(&ast));
  });
}
