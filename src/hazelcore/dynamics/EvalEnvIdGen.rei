/* Generator for EvalEnvId.t

   Usually abbreviated to "ec" because this was originally
   EvalEnvCtx. Not sure what a better abbreviation would be.
   */
[@deriving sexp]
type t;

/* Constructor used when beginning evaluation */
let initial: t;

/* Returns new `EvalEnvIdGen.t` and newly emitted `EvalEnvId.t` */
let next: t => (t, EvalEnvId.t);
