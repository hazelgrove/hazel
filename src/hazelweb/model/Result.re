[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, LivelitInstanceInfo.t, Eval.result);

let get_dhexp = ((d, _, _, _): t) => d;
