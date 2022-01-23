/* Representation of a hole closure (the set of hole
   instances with the same hole number and environment)

   Replacement for HoleInstance.t (due to performance issue,
   should group instances with the same environment together)

   TODO: should this include the EvalEnv.t?
   TODO: should this include Environment.t rather than EvalEnv.t?
   TODO: should this include hole closure parents?
   */
[@deriving sexp]
type t = (MetaVar.t, HoleClosureId.t, EvalEnv.t);

let u_of_hc: t => MetaVar.t;
let i_of_hc: t => HoleClosureId.t;
let sigma_of_hc: t => EvalEnv.t;
