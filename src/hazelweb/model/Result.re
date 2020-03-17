[@deriving sexp]
type t = (
  DHExp.t,
  NodeInstanceInfo.t,
  NodeInstanceInfo.t,
  Dynamics.Evaluator.result,
);

let get_dhexp = ((d, _, _, _): t) => d;
