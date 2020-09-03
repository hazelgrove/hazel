[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTyp.t);

let to_string: t => string;
