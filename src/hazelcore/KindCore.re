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

let abs_to_rel = (~offset: int=0, k: t(Index.abs)): t(Index.rel) =>
  switch (k) {
  | KHole => KHole
  | T => T
  | Singleton(ty) => Singleton(HTypSyntax.abs_to_rel(~offset, ty))
  };

let rel_to_abs =
    (~offset: Index.t(Index.abs)=Index.of_int(0), k: t(Index.rel))
    : t(Index.abs) =>
  switch (k) {
  | KHole => KHole
  | T => T
  | Singleton(ty) => Singleton(HTypSyntax.rel_to_abs(~offset, ty))
  };
