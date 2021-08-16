/* A simple metavariable generator */
[@deriving (sexp, show)]
type t = MetaVar.t;
let init = 0;
let next = x => {
  let n = x + 1;
  (x, n);
};
