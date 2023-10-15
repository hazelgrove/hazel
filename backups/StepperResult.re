type t =
  | Expr(DHExp.t)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

let unbox =
  fun
  | Expr(d)
  | BoxedValue(d)
  | Indet(d) => d;
