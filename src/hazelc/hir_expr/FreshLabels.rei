/**
  Compute fresh labels for hir expressions.
 */

[@deriving sexp]
type t = (Expr.Label.t, Expr.RuleLabel.t, Pat.Label.t);

let fresh_labels: Expr.t => t;
