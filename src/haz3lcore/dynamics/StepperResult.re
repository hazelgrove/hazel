type t =
  | Expr(DHExp.t)
  | Indet(DHExp.t)
  | BoxedValue(DHExp.t);

let unbox =
  fun
  | Expr(d)
  | Indet(d)
  | BoxedValue(d) => d;
