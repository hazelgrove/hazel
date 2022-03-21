[@deriving sexp]
type t =
  | NotImplemented
  | BadBuiltin(Var.t);

[@deriving sexp]
exception Exception(t);
