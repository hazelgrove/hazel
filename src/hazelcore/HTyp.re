open Sexplib.Std;

[@deriving sexp]
type idx = int; /* we use de Bruijn indices */

/* types with holes */
[@deriving sexp]
type t =
  | TyVar(idx, TyId.t) /* bound type variable */
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

let is_Prod =
  fun
  | Prod(_) => true
  | _ => false;

let precedence_Prod = 1;
let precedence_Arrow = 2;
let precedence_Sum = 3;
let precedence_const = 4;
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

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

let rec print = (t: t): string =>
  switch (t) {
  | TyVar(idx, id) => "TyVar " ++ string_of_int(idx) ++ " " ++ id
  | TyVarHole(idx, id) => "TyVarHole " ++ string_of_int(idx) ++ " " ++ id
  | Hole => "hole"
  | Int => "int"
  | Float => "float"
  | Bool => "bool"
  | Arrow(t1, t2) => "Arrow(" ++ print(t1) ++ ", " ++ print(t2) ++ ")"
  | Sum(t1, t2) => "Sum(" ++ print(t1) ++ ", " ++ print(t2) ++ ")"
  | Prod(_) => "prod"
  | List(t) => "List(" ++ print(t) ++ ")"
  };
