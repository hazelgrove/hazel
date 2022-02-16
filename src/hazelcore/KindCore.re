[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTypCore.t);
