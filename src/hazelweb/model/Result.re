[@deriving (sexp, show)]
type t = (DHExp.t, HoleInstanceInfo.t, Evaluator.result);

let get_dhexp = ((d, _, _): t) => d;
