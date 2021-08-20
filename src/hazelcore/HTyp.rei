/* types with holes */
[@deriving sexp]
type t =
  | Hole(InfVar.t)
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

type inf_constraint = (t, t);

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

let matched_arrow: t => (option((t, t)), list(inf_constraint));
let matched_sum: t => (option((t, t)), list(inf_constraint));
let matched_list: t => (option(t), list(inf_constraint));

let load_type_variable: t => unit;

let complete: t => bool;

let join: (join, t, t) => option(t);
let join_all: (join, list(t)) => option(t);
