open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t('idx) =
  | TyVar(Index.t('idx), string)
  | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, string)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t('idx), t('idx))
  | Sum(t('idx), t('idx))
  | Prod(list(t('idx)))
  | List(t('idx));

let equal: (t('idx), t('idx)) => bool = (==);

let rec increment_indices: t('idx) => t('idx) =
  fun
  | TyVar(i, name) => TyVar(Index.increment(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(increment_indices(t1), increment_indices(t2))
  | Sum(t1, t2) => Sum(increment_indices(t1), increment_indices(t2))
  | List(t) => List(increment_indices(t))
  | Prod(lst) => Prod(List.map(increment_indices, lst));

let rec decrement_indices: t('idx) => t('idx) =
  fun
  | TyVar(i, name) => TyVar(Index.decrement(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(decrement_indices(t1), decrement_indices(t2))
  | Sum(t1, t2) => Sum(decrement_indices(t1), decrement_indices(t2))
  | List(t) => List(decrement_indices(t))
  | Prod(lst) => Prod(List.map(decrement_indices, lst));

/* decrements indices that aren't the target */
let rec subst = (ty: t('idx), i: Index.t('idx), new_ty: t('idx)): t('idx) => {
  let go = ty1 => subst(ty1, i, new_ty);
  switch (ty) {
  | TyVar(i', name) =>
    Index.equal(i, i') ? new_ty : TyVar(Index.decrement(i'), name)
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => ty
  | Arrow(ty1, ty2) => Arrow(go(ty1), go(ty2))
  | Sum(tyL, tyR) => Sum(go(tyL), go(tyR))
  | Prod(tys) => Prod(List.map(go, tys))
  | List(ty) => List(go(ty))
  };
};

let rec to_rel = (~offset: int=0, ty: t(Index.absolute)): t(Index.relative) =>
  switch (ty) {
  | TyVar(i, name) => TyVar(Index.Abs.to_rel(~offset, i), name)
  | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) => Arrow(to_rel(~offset, ty1), to_rel(~offset, ty2))
  | Sum(tyL, tyR) => Sum(to_rel(~offset, tyL), to_rel(~offset, tyR))
  | Prod(tys) => Prod(List.map(to_rel(~offset), tys))
  | List(ty) => List(to_rel(~offset, ty))
  };

let rec to_abs = (~offset: int=0, ty: t(Index.relative)): t(Index.absolute) =>
  switch (ty) {
  | TyVar(i, name) => TyVar(Index.Rel.to_abs(~offset, i), name)
  | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) => Arrow(to_abs(~offset, ty1), to_abs(~offset, ty2))
  | Sum(tyL, tyR) => Sum(to_abs(~offset, tyL), to_abs(~offset, tyR))
  | Prod(tys) => Prod(List.map(to_abs(~offset), tys))
  | List(ty) => List(to_abs(~offset, ty))
  };
