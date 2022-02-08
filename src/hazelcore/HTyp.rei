/** Operations on types with holes */

include (module type of {
  include HTypCore;
});

type join =
  | GLB
  | LUB;

let head_normalize: (TyVarCtx.t, t) => t;

let normalized_consistent: (t, t) => bool;
let normalized_equivalent: (t, t) => bool;

let consistent: (TyVarCtx.t, t, t) => bool;
let equivalent: (TyVarCtx.t, t, t) => bool;

let join: (TyVarCtx.t, join, t, t) => option(t);
let join_all: (TyVarCtx.t, join, list(t)) => option(t);
