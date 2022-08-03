open Mir_anf;

include (module type of ExprLabel.Map);

[@deriving sexp]
type t = ExprLabel.Map.t(Complete.t);
