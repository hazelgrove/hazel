open Util;
// TODO: this needs to be a proper set to get rid of duplicate types
// Temp fix just prevent duplicaste insertion
module type Equivable = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  let equal: (t, t) => bool;
};

module type Type = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type elt_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(elt_t);

  let set_contains: (elt_t, t) => bool;
  let add: (elt_t, t) => t;

  // Fold for dedup
  let union: (t, t) => t;
  let empty: t;
  let singleton: elt_t => t;
  let to_list: t => t;
};

module Make = (T: Equivable) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type elt_t = T.t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(elt_t);

  let set_contains = (x: elt_t, ts: t) => List.exists(T.equal(x), ts);

  let add = (x: elt_t, ts: t) => !set_contains(x, ts) ? [x, ...ts] : ts;

  // Fold for dedup
  let union = (a, b) => List.fold_left((acc, t) => add(t, acc), a, b);
  let empty = [];
  let singleton = (t: elt_t): t => [t];
  let to_list = (t: t) => t;
};
