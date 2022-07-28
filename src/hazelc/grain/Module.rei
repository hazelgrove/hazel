[@deriving sexp]
type t = (list(Decl.t), Expr.block);

/**
  [empty] is an empty module.
 */
let empty: t;
