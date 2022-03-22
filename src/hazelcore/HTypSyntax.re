open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t =
  | TyVar(Index.t, string)
  | TyVarHole(TyVar.HoleReason.t, MetaVar.t, string)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

let rec increment_indices: t => t =
  fun
  | TyVar(i, name) => TyVar(Index.increment(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(increment_indices(t1), increment_indices(t2))
  | Sum(t1, t2) => Sum(increment_indices(t1), increment_indices(t2))
  | List(t) => List(increment_indices(t))
  | Prod(lst) => Prod(List.map(increment_indices, lst));

let rec decrement_indices: t => t =
  fun
  | TyVar(i, name) => TyVar(Index.decrement(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(decrement_indices(t1), decrement_indices(t2))
  | Sum(t1, t2) => Sum(decrement_indices(t1), decrement_indices(t2))
  | List(t) => List(decrement_indices(t))
  | Prod(lst) => Prod(List.map(decrement_indices, lst));

let rec subst = (ty: t, i: Index.t, new_ty: t): t => {
  let recur = ty1 => subst(ty1, i, new_ty);
  switch (ty) {
  | TyVar(i', _) => Index.equal(i, i') ? new_ty : ty
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
