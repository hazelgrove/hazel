[@deriving sexp]
type t = {
  eig: EnvironmentIdGen.t,
  stats: EvaluatorStats.t,
};

let init = {eig: EnvironmentIdGen.init, stats: EvaluatorStats.initial};

let get_eig = ({eig, _}) => eig;
let put_eig = (eig, es) => {...es, eig};
let next_ei = es => {
  let (ei, eig) = es |> get_eig |> EnvironmentIdGen.next;
  let es = es |> put_eig(eig);
  (ei, es);
};

let take_step = ({stats, _} as es) => {
  ...es,
  stats: stats |> EvaluatorStats.take_step,
};

let get_step = ({stats, _}) => stats |> EvaluatorStats.get_step;
