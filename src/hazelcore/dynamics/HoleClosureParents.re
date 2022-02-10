open Sexplib.Std;

[@deriving sexp]
type t_ = (Var.t, HoleClosure.t)
and t = list(t_);

let add_parent = (hcp: t, new_parent: t_) => [
  new_parent,
  ...List.filter(p => p != new_parent, hcp),
];

let to_list = (hcp: t): list(t_) => hcp;

let singleton = (parent: t_) => [parent];
