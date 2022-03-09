include (module type of {
  include KindCore;
});

let singleton: HTyp.t => t;

let consistent_subkind: (TyVarCtx.t, t, t) => bool;
