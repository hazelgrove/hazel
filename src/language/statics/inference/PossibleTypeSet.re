// TODO: this needs to be a proper set to get rid of duplicate types
// Temp fix just prevent duplicaste insertion

type t = list(Typ.t);

let set_contains = (x: Typ.t, ts: t) => List.exists(Typ.equal(x), ts);

let add = (x: Typ.t, ts: t) => !set_contains(x, ts) ? [x, ...ts] : ts;

// Fold for dedup
let union = (a, b) => List.fold_left((acc, t) => add(t, acc), a, b);
let empty = [];
let singleton = (t: Typ.t): t => [t];
let to_list = (t: t) => t;
