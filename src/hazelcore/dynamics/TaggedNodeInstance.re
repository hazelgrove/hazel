[@deriving sexp]
type kind =
  | Hole
  | Livelit;
type t = (kind, NodeInstance.t);
