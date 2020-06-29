[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, Eval_Result.result);

let get_dhexp = ((d, _, _): t) => d;
