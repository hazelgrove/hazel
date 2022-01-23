/* types with holes */
[@deriving sexp]
type t =
  | TyVar(TyVar.t) /* bound type variable */
  | TyVarHole(MetaVar.t, TyVar.t) /* free type variables */
  | Hole(MetaVar.t)
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence: t => int;

let get_prod_elements: t => list(t);
let get_prod_arity: t => int;

let matched_arrow: t => option((t, t));
let matched_sum: t => option((t, t));
let matched_list: t => option(t);

let complete: t => bool;

let increment_indices: t => t;

// let t_of_builtintype: TyId.BuiltInType.t => t;
