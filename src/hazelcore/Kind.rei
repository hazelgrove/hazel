include (module type of {
  include KindCore;
});

type t = KindCore.t(Index.abs);

let singleton: HTyp.t => t;

let consistent_subkind: (TyCtx.t, t, t) => bool;

let canonical_type: t => HTyp.t;
