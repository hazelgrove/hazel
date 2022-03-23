[@deriving sexp]
type t = (EvalEnvId.t, EvalStats.t, FARInfo.t);

/* `EvalEnvId.empty` is a special value used for the empty
   environment at the beginning of evaluation or when resuming
   evaluation (fill and resume). */
let initial = (EvalEnvId.empty + 1, EvalStats.initial, FARInfo.empty);

let next_evalenvid = ((ei, stats, far_info): t): (t, EvalEnvId.t) => (
  (ei + 1, stats, far_info),
  ei,
);

let set_far_info = (far_info: FARInfo.t, (ei, stats, _): t): t => (
  ei,
  stats,
  far_info,
);
