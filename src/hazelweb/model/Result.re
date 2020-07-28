[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, Evaluator.result);

let get_dhexp = (((d, _, _): t, _)) => d;

let get_dhexp_assert = (((d, _, _): t, map: AssertMap.t)) => (d, map);
