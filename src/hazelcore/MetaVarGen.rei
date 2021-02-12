/* A simple metavariable generator */
[@deriving sexp]
type t = (MetaVar.t, MetaVar.t);
let init: t;
let next_hole: t => (MetaVar.t, t);
let next_livelit: t => (MetaVar.t, t);
let reset_hole: t => t;
