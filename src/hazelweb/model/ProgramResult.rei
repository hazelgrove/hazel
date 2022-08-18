[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t);

/**
  [get_dhexp r] is the {!type:DHExp.t} in [r].
 */
let get_dhexp: t => DHExp.t;

/**
  [get_hii r] is the {!type:HoleInstanceInfo.t} in [r].
 */
let get_hii: t => HoleInstanceInfo.t;

/**
  [empty] is the empty result.
 */
let empty: t;
