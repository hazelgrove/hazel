/* Generator for EvalEnvId.t

   Usually abbreviated to "ec" because this was originally
   EvalEnvCtx. Not sure what a better abbreviation would be.
   */
[@deriving sexp]
type t;

let empty: t;
let next: t => (t, EvalEnvId.t);
