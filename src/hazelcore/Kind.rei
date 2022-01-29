/** Kinds */

include (module type of {
  include KindCore;
});

let equivalent: (t, t, TyCtx.t) => bool;
let consistent_subkind: (t, t, TyCtx.t) => bool;
