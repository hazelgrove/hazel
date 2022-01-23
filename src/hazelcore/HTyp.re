open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t =
  | TyVar(TyVar.t)
  | TyVarHole(MetaVar.t, TyVar.t)
  | Hole(MetaVar.t)
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
  | Hole(_)
  | Prod([])
  | TyVar(_)
  | TyVarHole(_)
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

/* matched arrow types */
let matched_arrow =
  fun
  | Hole(u)
  | TyVarHole(u, _) => {
      Some((Hole(u), Hole(u)));
    }
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

/* matched sum types */
let matched_sum =
  fun
  | Hole(u)
  | TyVarHole(u, _) => {
      Some((Hole(u), Hole(u)));
    }
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

/* matched list types */
let matched_list =
  fun
  | Hole(u)
  | TyVarHole(u, _) => Some(Hole(u))
  | List(ty) => Some(ty)
  | _ => None;

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole(_) => false
  | TyVarHole(_) => false
  | TyVar(_) => true
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec increment_indices: t => t =
  fun
  | TyVar((i, t)) => TyVar((Index.increment(i), t))
  | (TyVarHole(_) | Hole(_) | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(increment_indices(t1), increment_indices(t2))
  | Sum(t1, t2) => Sum(increment_indices(t1), increment_indices(t2))
  | List(t) => List(increment_indices(t))
  | Prod(lst) => Prod(List.map(increment_indices, lst));

// let t_of_builtintype =
//   fun
//   | TyId.BuiltInType.Bool => Bool
//   | Float => Float
//   | Int => Int;
