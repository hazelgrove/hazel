/**
  The output from {!val:Evaluator.evaluate}.
 */

/**
  The type for the evaluation result, a {!type:DHExp.t} wrapped in its {v final
  v} judgment (boxed value or indeterminate).
 */
[@deriving sexp]
type t = DHExp.result;
