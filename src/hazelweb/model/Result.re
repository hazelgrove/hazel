[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, Dynamics.Evaluator.result);

let get_dhexp = ((d, _, _): t) => d;
