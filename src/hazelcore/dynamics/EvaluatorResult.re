[@deriving sexp]
type t = DHExp.result = | BoxedValue(DHExp.t) | Indet(DHExp.t);
