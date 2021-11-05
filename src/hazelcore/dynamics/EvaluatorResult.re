[@deriving sexp]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);
