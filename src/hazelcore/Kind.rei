include (module type of {
  include KindCore;
});

type t = KindCore.t(Index.absolute);

let singleton: HTyp.t => t;

let consistent_subkind: (Contexts.t, t, t) => bool;

let canonical_type: t => HTyp.t;

let subst_tyvars: (t, list((Index.Abs.t, HTyp.t))) => t;
