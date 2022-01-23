[@deriving sexp]
type t = (MetaVar.t, HoleClosureId.t, EvalEnv.t);

let u_of_hc = ((u, _, _): t): MetaVar.t => u;
let i_of_hc = ((_, i, _): t): HoleClosureId.t => i;
let sigma_of_hc = ((_, _, sigma): t): EvalEnv.t => sigma;
