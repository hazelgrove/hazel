open Anf;

[@deriving sexp]
type t = (ExprLabel.t, StmtLabel.t, RuleLabel.t, Pat.Label.t);

/**
  Compute fresh labels for mir expressions.
 */
let fresh_labels: block => t;
