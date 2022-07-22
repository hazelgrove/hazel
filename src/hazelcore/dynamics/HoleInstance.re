[@deriving sexp]
type t = (MetaVar.t, HoleInstanceId.t);

let u_of_hc = ((u, _): t): MetaVar.t => u;
let i_of_hc = ((_, i): t): HoleInstanceId.t => i;

let result_hc: t = ((-1), 0);
