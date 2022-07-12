[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t);

let get_dhexp = ((d, _, _): t) => d;

let empty = (DHExp.empty, HoleInstanceInfo.empty, EvaluatorResult.empty);
