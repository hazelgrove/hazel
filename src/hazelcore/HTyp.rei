/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(sum_body)
  | Prod(list(t))
  | List(t)
and sum_body =
  | Finite(TagMap.t(option(t)))
  | Elided(UHTag.t, option(t));

type join =
  | GLB
  | LUB;

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence: t => int;

/* type equality */
let eq: (t, t) => bool;

/* type consistency */
let consistent: (t, t) => bool;

let get_prod_elements: t => list(t);
let get_prod_arity: t => int;

let matched_arrow: t => option((t, t));
let matched_list: t => option(t);

let complete: t => bool;

let join: (join, t, t) => option(t);
let join_all: (join, list(t)) => option(t);
