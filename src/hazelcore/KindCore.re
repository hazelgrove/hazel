[@deriving sexp]
type t('idx) =
  | KHole
  | T
  | Singleton(HTypSyntax.t('idx));

let equal = (k: t('idx), k': t('idx)) =>
  switch (k, k') {
  | (Singleton(ty), Singleton(ty')) => HTypSyntax.equal(ty, ty')
  | _ => k == k'
  };

let canonical_type: t('idx) => HTypSyntax.t('idx) =
  fun
  | KHole
  | T => HTypSyntax.Hole
  | Singleton(ty) => ty;

let increment_indices = (k: t('idx)): t('idx) =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) => Singleton(HTypSyntax.increment_indices(ty))
  };

let decrement_indices = (k: t('idx)): t('idx) =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) => Singleton(HTypSyntax.decrement_indices(ty))
  };

let shift_indices = (~above: int, ~amount: int, k: t('idx)): t('idx) =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) => Singleton(HTypSyntax.shift_indices(~above, ~amount, ty))
  };

let subst_tyvar =
    (k: t('idx), i: Index.t('idx), ty: HTypSyntax.t('idx)): t('idx) =>
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
