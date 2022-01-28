// [@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(t, HTypCore.t);

/** Kind equality

Kinds are equal (modulo indices) if they are structurally equal modulo types,
and types are equal (modulo indices).
*/;
// let rec equal = (k: t, k': t): bool =>
//   switch (k, k') {
//   | (KHole, KHole)
//   | (Type, Type) => true
//   | (KHole | Type, _) => false
//   | (Singleton(k1, ty), Singleton(k1', ty')) =>
//     equal(k1, k1') && HTyp.equal(ty, ty')
//   | (Singleton(_), _) => false
//   };

// let rec to_string: t => string =
//   fun
//   | KHole => "KHole"
//   | Type => "Type"
//   | Singleton(t, typ) =>
//     "Singleton("
//     ++ to_string(t)
//     ++ ", "
//     ++ (HTyp.sexp_of_t(typ) |> Sexp.to_string)
//     ++ ")";

// let is_singleton: t => bool =
//   fun
//   | Singleton(_) => true
//   | KHole
//   | Type => false;
