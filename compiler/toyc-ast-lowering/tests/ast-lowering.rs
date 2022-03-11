use test_util::{assert_snapshot, snapshots, TestSession};
use toyc_ast_lowering::LoweringContext;
use toyc_hir::HirContext;

fn main() {
  snapshots(|text| {
    let session = TestSession::new("test-ast-lowering", text.to_string());
    let ast =
      toyc_ast::parse_single_file(&session.root_source, &session.handler)
        .unwrap();
    let ctx = LoweringContext::new(&ast);
    let ctx: HirContext = ctx.into();
    let mut pretty_hir = String::new();
    toyc_hir_pretty::print(&ctx, &mut pretty_hir).unwrap();
    assert_snapshot!(pretty_hir);
  });
}
