module Index: {
  [@deriving sexp]
  type t; /* we use de Bruijn */

  let of_int: int => t;

  let lookup: (list('a), t) => 'a;
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

type join =
  | GLB
  | LUB;

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence: t => int;

/* type equality */
let eq: (t, t) => bool;

/* type consistency */
let consistent: (t, t) => bool;

let get_prod_elements: t => list(t);
let get_prod_arity: t => int;

let matched_arrow: t => option((t, t));
let matched_sum: t => option((t, t));
let matched_list: t => option(t);

let complete: t => bool;

let join: (join, t, t) => option(t);
let join_all: (join, list(t)) => option(t);
