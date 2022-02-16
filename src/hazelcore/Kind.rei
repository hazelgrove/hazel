include (module type of {
  include KindCore;
});

let consistent_subkind: (TyVarCtx.t, t, t) => bool;
