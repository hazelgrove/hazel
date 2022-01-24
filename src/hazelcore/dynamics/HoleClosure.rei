/* Representation of a hole closure (the set of hole
   instances with the same hole number and environment)

   Replacement for HoleInstance.t (due to performance issue,
   should group instances with the same environment together)

   TODO: should this include hole closure parents?
   */
[@deriving sexp]
type t = (MetaVar.t, HoleClosureId.t);

let u_of_hc: t => MetaVar.t;
let i_of_hc: t => HoleClosureId.t;
