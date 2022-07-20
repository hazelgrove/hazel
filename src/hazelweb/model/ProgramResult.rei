[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t);

/**
  [get_dhexp r] is the {!type:DHExp.t} in [r].
 */
let get_dhexp: t => DHExp.t;

/**
  [empty] is the empty result.
 */
let empty: t;
