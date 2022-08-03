[@deriving sexp]
type t =
  | THole
  | TInt
  | TFloat
  | TBool
  | TArrow(t, t)
  | TSum(t, t)
  | TProd(list(t))
  | TList(t);
