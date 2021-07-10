[@deriving sexp]
type t =
  | KHole
  | Type;

let consistent = (k1, k2) =>
  switch (k1, k2) {
  | (KHole, _)
  | (_, KHole)
  | (Type, Type) => true
  };

let to_string: t => string =
  fun
  | KHole => "KHole"
  | Type => "Type";
