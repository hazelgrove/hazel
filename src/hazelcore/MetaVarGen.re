/* A simple metavariable generator */
[@deriving sexp]
type t = (MetaVar.t, MetaVar.t);
let init = (0, 0);
let reset_hole = ((_, llu): t) => (0, llu);
let next_hole = ((u, llu)) => {
  (u, (u + 1, llu));
};
let next_livelit = ((u, llu)) => {
  (llu, (u, llu + 1));
};
