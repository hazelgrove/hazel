/* A simple metavariable generator */
[@deriving sexp]
type t = (MetaVar.t, MetaVar.t);
let init = (0, 0);
let next_hole = ((u, llu)) => {
  (u, (u + 1, llu));
};
let next_livelit = ((u, llu)) => {
  (llu, (u, llu + 1));
};
