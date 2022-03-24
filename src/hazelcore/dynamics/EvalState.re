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

let get_fill_dhexp = (u: MetaVar.t, (_, _, far_info): t): option(DHExp.t) =>
  switch (far_info) {
  | NonFill => None
  | Fill(u', d) => u == u' ? Some(d) : None
  };

let inc_steps = ((ei, stats, far_info): t): t => (
  ei,
  stats |> EvalStats.inc_steps,
  far_info,
);

let get_stats = ((_, stats, _): t): EvalStats.t => stats;
