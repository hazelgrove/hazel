[@deriving sexp]
type idx = int; /* we use de Bruijn indices */

/* types with holes */
[@deriving sexp]
type t =
  | TyVar(idx, Var.t) /* bound type variable */
  | TyVarHole(MetaVar.t, Var.t) /* free type variables */
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

type join =
  | GLB
  | LUB;

let is_Prod: t => bool;

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence_const: int;
let precedence: t => int;

let get_prod_elements: t => list(t);

let get_prod_arity: t => int;

let print: t => string;
