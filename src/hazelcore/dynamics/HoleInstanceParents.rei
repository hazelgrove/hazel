/* List of hole closure parents. Analogous to InstancePath, but a single
   hole closure (set of closures with the same environment) may have
   multiple parents.
   */

[@deriving sexp]
type t_ = (Var.t, HoleInstance.t)
and t = list(t_);

let add_parent: (t, t_) => t;
let to_list: t => list(t_);
let singleton: t_ => t;
