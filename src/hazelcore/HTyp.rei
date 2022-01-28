/** Operations on types with holes */

include (module type of {
  include HTypCore;
});

type join =
  | GLB
  | LUB;

let equivalent_kind: (Kind.t, Kind.t, TyCtx.t) => bool;
let equivalent: (t, t, TyCtx.t) => bool;
let consistent: (t, t, TyCtx.t) => bool;
let join: (join, t, t, TyCtx.t) => option(t);
let join_all: (join, list(t), TyCtx.t) => option(t);
let new_Hole: MetaVarGen.t => (t, MetaVarGen.t);
