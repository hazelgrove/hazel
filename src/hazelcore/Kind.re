[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(t, HTypCore.t);
