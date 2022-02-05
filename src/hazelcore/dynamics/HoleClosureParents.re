open Sexplib.Std;

[@deriving sexp]
type t = list(HoleClosure.t);

let add_parent = (hcp: t, new_parent: HoleClosure.t) => [
  new_parent,
  ...List.filter(p => p != new_parent, hcp),
];

let to_list = (hcp: t): list(HoleClosure.t) => hcp;

let singleton = (hc: HoleClosure.t) => [hc];
