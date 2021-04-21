// Construction.re encompasses Kinds and Types and Type constructors

/// k1 is a consistent_subkind of k2
let consistent_subkind: (Contexts.t, Kind.t, Kind.t) => bool;

/// k1 is equivalent to k2
let kequiv: (Contexts.t, Kind.t, Kind.t) => bool;

/// ty1 is equivalent to ty2 and its kind is k
let kcequiv: (Contexts.t, HTyp.t, HTyp.t) => option(Kind.t);
