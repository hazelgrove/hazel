[@deriving sexp]
type t = (EvalEnvId.t, EvalStats.t);

/* `EvalEnvId.empty` is a special value used for the empty
   environment at the beginning of evaluation or when resuming
   evaluation (fill and resume). */
let initial = (EvalEnvId.empty + 1, EvalStats.initial);

let next_evalenvid = ((ei, stats): t): (t, EvalEnvId.t) => (
  (ei + 1, stats),
  ei,
);

let inc_steps = ((ei, stats): t): t => (ei, stats |> EvalStats.inc_steps);

let get_stats = ((_, stats): t): EvalStats.t => stats;
