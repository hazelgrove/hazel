/** The kind- and context-invariant parts of types with holes */
open Sexplib.Std;

[@deriving sexp]
type t =
  | TyVar(Index.t, TyVar.Name.t)
  | TyVarHole(TyVar.HoleReason.t, MetaVar.t, TyVar.Name.t)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

let precedence_Prod = Operators_Typ.precedence(Prod);
let precedence_Arrow = Operators_Typ.precedence(Arrow);
let precedence_Sum = Operators_Typ.precedence(Sum);
let precedence_const = Operators_Typ.precedence_const;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | Hole
  | Prod([])
  | List(_)
  | TyVar(_)
  | TyVarHole(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

let rec equal = (ty: t, ty': t): bool =>
  switch (ty, ty') {
  | (TyVar(_, name), TyVar(_, name')) => TyVar.Name.equal(name, name')
  | (TyVar(_), _) => false
  | ((TyVarHole(_) | Hole | Int | Float | Bool) as ty, ty') => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    equal(ty1, ty1') && equal(ty2, ty2')
  | (Arrow(_) | Sum(_), _) => false
  | (Prod(tys), Prod(tys')) => List.for_all2(equal, tys, tys')
  | (Prod(_), _) => false
  | (List(ty1), List(ty1')) => equal(ty1, ty1')
  | (List(_), _) => false
  };

/* matched arrow types */
let matched_arrow = (ty: t): option((t, t)) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None
  };

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

/* matched sum types */
let matched_sum = (ty: t): option((t, t)) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None
  };

/* matched list types */
let matched_list =
  fun
  | Hole
  | TyVarHole(_) => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None;

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole
  | TyVarHole(_) => false
  | TyVar(_)
  | Int
  | Float
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec increment_indices: t => t =
  fun
  | TyVar(i, name) => TyVar(Index.increment(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(increment_indices(t1), increment_indices(t2))
  | Sum(t1, t2) => Sum(increment_indices(t1), increment_indices(t2))
  | List(t) => List(increment_indices(t))
  | Prod(lst) => Prod(List.map(increment_indices, lst));

// let t_of_builtintype =
//   fun
//   | TyId.BuiltInType.Bool => Bool
//   | Float => Float
//   | Int => Int;
