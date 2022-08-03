open Mir_anf;

include ExprLabel.Map;

[@deriving sexp]
type t = ExprLabel.Map.t(Complete.t);
