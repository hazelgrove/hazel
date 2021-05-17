// Construction.re encompasses Kinds and Types and Type constructors

/// k1 is a consistent_subkind of k2
let consistent_subkind: (Contexts.t, Kind.t, Kind.t) => bool;

/// k1 is equivalent to k2
let kequiv: (Contexts.t, Kind.t, Kind.t) => bool;

module HTyp: {
  type t = HTyp.t;

  type join = HTyp.join;

  /// ty1 is equivalent to ty2
  let equiv: (Contexts.t, t, t) => bool;

  /// ty1 is consistent with ty2 under a ctx
  let consistent: (Contexts.t, t, t) => bool;

  /// ty1 joined with ty2 via LUB or GLB
  let join: (Contexts.t, join, t, t) => option(t);
  let join_all: (Contexts.t, join, list(t)) => option(t);
};
