[@deriving sexp]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

let empty = Indet(DHExp.empty);
