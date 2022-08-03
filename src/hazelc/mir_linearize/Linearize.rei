/**
  Linearize Hir into Anf.
 */
let linearize:
  (Hir_expr.typ_context, Hir_expr.delta, Hir_expr.expr) =>
  (Mir_anf.typ_context, Mir_anf.delta, Mir_anf.block);
