/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Int
  | Float
  | Bool
  | String
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

type join =
  | GLB
  | LUB;

let is_Prod: t => bool;

let is_Arrow: t => bool;

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence_const: int;
let precedence: t => int;

/* type equality */
let eq: (t, t) => bool;

/* type consistency */
let consistent: (t, t) => bool;

let inconsistent: (t, t) => bool;

let consistent_all: list(t) => bool;

let matched_arrow: t => option((t, t));

/* matched arrow types */
let has_matched_arrow: t => bool;

let get_prod_elements: t => list(t);

let get_prod_arity: t => int;

/* matched sum types */
let matched_sum: t => option((t, t));

let has_matched_sum: t => bool;

/* matched list types */
let matched_list: t => option(t);

let has_matched_list: t => bool;

/* complete (i.e. does not have any holes) */
let complete: t => bool;

let join: (join, t, t) => option(t);

let join_all: (join, list(t)) => option(t);
