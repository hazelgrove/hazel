/**
  The output from {!val:Evaluator.evaluate}.
 */

/**
  The type for the evaluation result, a {!type:DExp.t} wrapped in its {v final
  v} judgment (boxed value or indeterminate).
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DExp.t)
  | Indet(DExp.t);

/**
  [unbox r] is the inner expression.
 */
let unbox: t => DExp.t;

/**
  See {!val:DExp.fast_equal}.
 */
let fast_equal: (t, t) => bool;
