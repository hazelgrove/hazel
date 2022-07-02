/* Representation of a unique hole closure/instantiation (the set of hole
   instances with the same hole number and environment)

   Replacement for HoleInstance.t (due to performance issue,
   should group instances with the same environment together)
   */
[@deriving sexp]
type t = (MetaVar.t, HoleClosureId.t);

let u_of_hc: t => MetaVar.t;
let i_of_hc: t => HoleClosureId.t;

/* Special HoleClosure.t used to represent the parent
   "hole instance" of the result. That is to say, if a hole
   instance has this value as its parent, then it is
   directly in the result.
   */
let result_hc: t;
