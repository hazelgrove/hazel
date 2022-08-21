[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t);

let get_dhexp = ((d, _, _): t) => d;
let get_hii = ((_, hii, _): t) => hii;

let empty = (DHExp.empty, HoleInstanceInfo.empty, EvaluatorResult.empty);
