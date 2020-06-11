[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, Dynamics_common.Evaluator.result);

let get_dhexp = ((d, _, _): t) => d;
