/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Label(Label.t)
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list((option(Label.t), t)))
  | List(t);

type join =
  | GLB
  | LUB;

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence_Space: int;
let precedence: t => int;

/* type equality */
let eq: (t, t) => bool;

/* type consistency */
let consistent: (t, t) => bool;

let get_prod_elements: t => list(t);
let get_prod_arity: t => int;
let get_prj_type: (t, Label.t) => option(t);

let matched_arrow: t => option((t, t));
let matched_sum: t => option((t, t));
let matched_list: t => option(t);

let complete: t => bool;

let join_all: (join, list(t)) => option(t);
