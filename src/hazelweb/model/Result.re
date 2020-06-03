[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, Evaluation.Evaluator.result);

let get_dhexp = ((d, _, _): t) => d;
