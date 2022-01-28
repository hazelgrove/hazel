// [@deriving sexp]
type t =
  | KHole
  | Type
  // higher singleton
  | Singleton(t, HTypCore.t);

// let equal: (t, t) => bool;

// let to_string: t => string;

// let is_singleton: t => bool;
