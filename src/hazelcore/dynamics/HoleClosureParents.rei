/* List of hole closure parents. Analogous to InstancePath, but a single
   hole closure (set of closures with the same environment) may have
   multiple parents.
   */

[@deriving sexp]
type t = list(HoleClosure.t);

let add_parent: (t, HoleClosure.t) => t;
let to_list: t => list(HoleClosure.t);
let singleton: HoleClosure.t => t;
