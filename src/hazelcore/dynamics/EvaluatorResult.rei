/**
  The output from {!val:Evaluator.evaluate}.
 */

/**
  The type for the evaluation result, a {!type:DHExp.t} wrapped in its {v final
  v} judgment (boxed value or indeterminate).
 */
[@deriving sexp]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

/**
  [unwrap r] is the inner expression.
 */
let unwrap: t => DHExp.t;

/**
  See {!val:DHExp.fast_equal}.
 */
let fast_equal: (t, t) => bool;
