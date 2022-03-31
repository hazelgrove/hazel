[@deriving sexp]
type t =
  | NotImplemented;

[@deriving sexp]
exception Exception(t);
