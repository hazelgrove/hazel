[@deriving sexp]
type t = (
  DHExp.t,
  HoleInstanceInfo.t,
  LivelitInstanceInfo.t,
  Evaluator.result,
);

let get_dhexp = ((d, _, _, _): t) => d;
