module StmtLoc: {
  [@deriving sexp]
  type t;

  let init: t;
  let next: t => t;
  let nest: t => t;

  let compare: (t, t) => int;
};
