/* A simple metavariable generator */
[@deriving sexp]
type t = MetaVar.t;
let init = 0;
let next_hole = x => {
  let n = x + 1;
  (x, n);
};
