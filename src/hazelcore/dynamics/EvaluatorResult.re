[@deriving sexp]
type exp =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type state = EvaluatorState.t;

[@deriving sexp]
type t = (exp, state);
