open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t_ = (Var.t, HoleInstance.t)
and t = list(t_);

let to_list = (hcp: t): list(t_) => hcp;
let singleton = (parent: t_) => [parent];

let add_parent = (hcp: t, new_parent: t_) => [
  new_parent,
  ...List.filter(p => p != new_parent, hcp),
];
