[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTyp.t);

let consistent = (k1, k2) =>
  switch (k1, k2) {
  | (KHole, _)
  | (_, KHole)
  | (Type, Type) => true
  | (Type, _) => false // need to consider
  | (_, Type) => false // need to consider
  | (Singleton(t1), Singleton(t2)) => HTyp.consistent(t1, t2)
  };

let to_string: t => string =
  fun
  | KHole => "KHole"
  | Type => "Type"
  | _ => "Singleton";
