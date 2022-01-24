// [@deriving sexp]
type t('a) =
  | KHole
  | Type
  // higher singleton
  | Singleton(t('a), 'a);

// let equal: (t, t) => bool;

// let to_string: t => string;

// let is_singleton: t => bool;
