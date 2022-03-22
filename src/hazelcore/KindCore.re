[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTypSyntax.t);

let canonical_type: t => HTypSyntax.t =
  fun
  | KHole
  | Type => Hole
  | Singleton(ty) => ty;

let subst_tyvar = (kind: t, i: Index.t, ty: HTypSyntax.t): t =>
  switch (kind) {
  | KHole
  | Type => kind
  | Singleton(ty1) => Singleton(HTypSyntax.subst(ty1, i, ty))
  };

// let decrement_indices: t => t =
//   fun
//   | (KHole | Type) as kind => kind
//   | Singleton(ty) => Singleton(HTypSyntax.decrement_indices(ty));
