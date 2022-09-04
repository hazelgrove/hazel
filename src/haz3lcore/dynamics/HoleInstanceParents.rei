/**
  List of hole instance parents. A single hole instance (set of closures with
  the same environment) may have multiple parents.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t_ = (Var.t, HoleInstance.t)
and t = list(t_);

let to_list: t => list(t_);
let singleton: t_ => t;

let add_parent: (t, t_) => t;
