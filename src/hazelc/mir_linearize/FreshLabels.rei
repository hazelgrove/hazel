/**
  Compute fresh labels for hir expressions.
 */

[@deriving sexp]
type t = (
  Hir_expr.Expr.Label.t,
  Hir_expr.Expr.RuleLabel.t,
  Hir_expr.Pat.Label.t,
);

let fresh_labels: Hir_expr.expr => t;
