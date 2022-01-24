[@deriving sexp]
type t = (DHExp.t, HoleClosureInfo.t, Evaluator.result);

let get_dhexp = ((d, _, _): t) => d;
