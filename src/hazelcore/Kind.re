module HTyp = KindSystem.HTyp;
module Context = KindSystem.Context;

[@deriving sexp]
type s('idx) = KindSystem.Kind.t('idx) = | Hole | Type | S(HTyp.t('idx));

[@deriving sexp]
type t = s(Index.absolute);

let to_htyp: t => HTyp.abs = KindSystem.Kind.to_htyp;

let singleton = (ty: HTyp.abs): t => S(ty);

/* Properties of Kind */

let consistent_subkind = (ctx: Context.t, k: t, k': t): bool =>
  switch (k, k') {
  | (Hole, _)
  | (_, Hole) => true
  | (S(_), Type) => true
  | (S(ty), S(ty')) => HTyp.equivalent(ctx, ty, ty')
  | (Type, S(_)) => false
  | (Type, Type) => true
  };

/* Operations on [Kind] */

let subst_tyvars = (k: t, tyvars: list((Index.Abs.t, HTyp.abs))): t =>
  switch (k) {
  | Hole
  | Type => k
  | S(ty) => S(HTyp.subst_tyvars(ty, tyvars))
  };
