[@deriving sexp]
type t =
  | KHole
  | Type
  // higher singleton
  | Singleton(t, HTyp.t);

// let to_string: t => string;

// let is_singleton: t => bool;
