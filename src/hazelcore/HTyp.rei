include (module type of {
  include HTypCore;
});

type join =
  | GLB
  | LUB;

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence: t => int;

/* normalized types */
let normalize: (TyVarCtx.t, t) => t;
let head_normalize: (TyVarCtx.t, t) => t;
let normalized_consistent: (t, t) => bool;
let normalized_equivalent: (t, t) => bool;

/* type equality */
let eq: (t, t) => bool;

/* type consistency */
let consistent: (TyVarCtx.t, t, t) => bool;
let equivalent: (TyVarCtx.t, t, t) => bool;

let get_prod_elements: t => list(t);
let get_prod_arity: t => int;

let matched_arrow: (TyVarCtx.t, t) => option((t, t));
let matched_sum: (TyVarCtx.t, t) => option((t, t));
let matched_list: t => option(t);

let complete: t => bool;

let join: (TyVarCtx.t, join, t, t) => option(t);
let join_all: (TyVarCtx.t, join, list(t)) => option(t);

let increment_indices: t => t;
