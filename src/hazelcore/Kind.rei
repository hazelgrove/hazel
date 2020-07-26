[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTyp.t);

let consistent: (t, t) => bool;

let to_string: t => string;
