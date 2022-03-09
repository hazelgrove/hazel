[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTypSyntax.t);
