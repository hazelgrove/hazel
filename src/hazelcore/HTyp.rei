/** Operations on types with holes */

include (module type of {
  include HTypCore;
});

type join =
  | GLB
  | LUB;

let normalized_consistent: (t, t) => bool;
let normalized_equivalent: (t, t) => bool;

let consistent: (TyCtx.t, t, t) => bool;
let equivalent: (TyCtx.t, t, t) => bool;

let join: (TyCtx.t, join, t, t) => option(t);
let join_all: (TyCtx.t, join, list(t)) => option(t);

let new_Hole: MetaVarGen.t => (t, MetaVarGen.t);
