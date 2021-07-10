open Sexplib.Std;

module Index = {
  [@deriving sexp]
  type t = int;

  /* TODO: What is identity function */
  let of_int = x => x;

  let eq = (==);

  /* Lookup at the nth position in context */
  let lookup = List.nth;
};

/* types with holes */
[@deriving sexp]
type t =
  | TyVar(Index.t, TyId.t) /* bound type variable */
  | TyVarHole(MetaVar.t, TyId.t) /* free type variables */
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

[@deriving sexp]
type join =
  | GLB
  | LUB;

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
  | Hole
  | TyVarHole(_) => {
      Some((Hole, Hole));
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
  | Hole
  | TyVarHole(_) => {
      Some((Hole, Hole));
    }
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

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
  | Hole => false
  | TyVarHole(_) => false
  | TyVar(_) => true
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec tyvar_debruijn_increment = t =>
  switch (t) {
  | TyVar(idx, id) => TyVar(idx + 1, id)
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => t
  | Arrow(t1, t2) =>
    Arrow(tyvar_debruijn_increment(t1), tyvar_debruijn_increment(t2))
  | Sum(t1, t2) =>
    Sum(tyvar_debruijn_increment(t1), tyvar_debruijn_increment(t2))
  | List(t) => List(tyvar_debruijn_increment(t))
  | Prod(lst) => Prod(List.map(tyvar_debruijn_increment, lst))
  };

let t_of_builtintype =
  fun
  | TyId.BuiltInType.Bool => Bool
  | Float => Float
  | Int => Int;
