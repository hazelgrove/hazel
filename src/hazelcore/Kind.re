include KindCore;

/** Kind equivalence

Kinds are equivalent if they are equal modulo singletons and singleton types
are all equivalent.
*/
let equivalent = (k: t, k': t, ctx: TyCtx.t): bool =>
  switch (k, k') {
  | (KHole, KHole)
  | (Type, Type) => true
  | (KHole | Type, _) => false
  | (Singleton(Singleton(_, ty), _), Singleton(_, ty'))
  | (Singleton(_, ty), Singleton(Singleton(_, ty'), _))
  | (Singleton(_, ty), Singleton(_, ty')) =>
    ctx |> HTyp.equivalent(ty, ty')
  | (Singleton(_), _) => false
  };

let consistent_subkind = (k: t, k': t, ctx: TyCtx.t): bool =>
  ctx
  |> equivalent(k, k')
  || (
    switch (k, k') {
    | (KHole, _)
    | (_, KHole) => true
    | (Singleton(_, _ty), Type) =>
      // TODO: (eric) XXX
      true
    // ana_kind(ctx, ty |> UHTyp.contract, k')
    // |> Option.fold(~none=false, ~some=() => true)
    | (_, _) => false
    }
  );

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
