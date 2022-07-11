[@deriving sexp]
type t = DHExp.result = | BoxedValue(DHExp.t) | Indet(DHExp.t);

let unbox =
  fun
  | BoxedValue(d)
  | Indet(d) => d;
