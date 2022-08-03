[@deriving sexp]
type t =
  | THole
  | TInt
  | TFloat
  | TBool
  | TArrow(t, t)
  | TSum(t, t)
  | TPair(t, t)
  | TUnit
  | TList(t);

let equal: (t, t) => bool;

let consistent: (t, t) => bool;
