open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t('i) =
  | TyVar(Index.t('i), string)
  | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, string)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t('i), t('i))
  | Sum(t('i), t('i))
  | Prod(list(t('i)))
  | List(t('i));

let rec increment_indices: t('i) => t('i) =
  fun
  | TyVar(i, name) => TyVar(Index.increment(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(increment_indices(t1), increment_indices(t2))
  | Sum(t1, t2) => Sum(increment_indices(t1), increment_indices(t2))
  | List(t) => List(increment_indices(t))
  | Prod(lst) => Prod(List.map(increment_indices, lst));

let rec decrement_indices: t('i) => t('i) =
  fun
  | TyVar(i, name) => TyVar(Index.decrement(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(decrement_indices(t1), decrement_indices(t2))
  | Sum(t1, t2) => Sum(decrement_indices(t1), decrement_indices(t2))
  | List(t) => List(decrement_indices(t))
  | Prod(lst) => Prod(List.map(decrement_indices, lst));

/* decrements indices that aren't the target */
let rec subst = (ty: t('i), i: Index.t('i), new_ty: t('i)): t('i) => {
  let recur = ty1 => subst(ty1, i, new_ty);
  switch (ty) {
  | TyVar(i', name) =>
    Index.equal(i, i') ? new_ty : TyVar(Index.decrement(i'), name)
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => ty
  | Arrow(ty1, ty2) => Arrow(recur(ty1), recur(ty2))
  | Sum(tyL, tyR) => Sum(recur(tyL), recur(tyR))
  | Prod(tys) => Prod(List.map(recur, tys))
  | List(ty) => List(recur(ty))
  };
};

let rec abs_to_rel = (~offset: int=0, ty: t(Index.abs)): t(Index.rel) =>
  switch (ty) {
  | TyVar(i, name) => TyVar(Index.abs_to_rel(~offset, i), name)
  | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) =>
    Arrow(abs_to_rel(~offset, ty1), abs_to_rel(~offset, ty2))
  | Sum(tyL, tyR) =>
    Sum(abs_to_rel(~offset, tyL), abs_to_rel(~offset, tyR))
  | Prod(tys) => Prod(List.map(abs_to_rel(~offset), tys))
  | List(ty) => List(abs_to_rel(~offset, ty))
  };

let rec rel_to_abs =
        (~offset: Index.t(Index.abs)=Index.of_int(0), ty: t(Index.rel))
        : t(Index.abs) =>
  switch (ty) {
  | TyVar(i, name) => TyVar(Index.rel_to_abs(~offset, i), name)
  | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) =>
    Arrow(rel_to_abs(~offset, ty1), rel_to_abs(~offset, ty2))
  | Sum(tyL, tyR) =>
    Sum(rel_to_abs(~offset, tyL), rel_to_abs(~offset, tyR))
  | Prod(tys) => Prod(List.map(rel_to_abs(~offset), tys))
  | List(ty) => List(rel_to_abs(~offset, ty))
  };
