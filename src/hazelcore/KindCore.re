[@deriving sexp]
type t('i) =
  | KHole
  | T
  | Singleton(HTypSyntax.t('i));

let canonical_type: t('i) => HTypSyntax.t('i) =
  fun
  | KHole
  | T => HTypSyntax.Hole
  | Singleton(ty) => ty;

let increment_indices = (k: t('i)): t('i) =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) => Singleton(HTypSyntax.increment_indices(ty))
  };

let decrement_indices = (k: t('i)): t('i) =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) => Singleton(HTypSyntax.decrement_indices(ty))
  };

let subst_tyvar = (k: t('i), i: Index.t('i), ty: HTypSyntax.t('i)): t('i) =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty1) => Singleton(HTypSyntax.subst(ty1, i, ty))
  };

let to_rel = (~offset: int=0, k: t(Index.absolute)): t(Index.relative) =>
  switch (k) {
  | KHole => KHole
  | T => T
  | Singleton(ty) => Singleton(HTypSyntax.to_rel(~offset, ty))
  };

let to_abs = (~offset: int=0, k: t(Index.relative)): t(Index.absolute) =>
  switch (k) {
  | KHole => KHole
  | T => T
  | Singleton(ty) => Singleton(HTypSyntax.to_abs(~offset, ty))
  };
