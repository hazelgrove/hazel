[@deriving sexp]
type t = DHExp.result;

let unbox =
  fun
  | BoxedValue(d)
  | Indet(d) => d;
