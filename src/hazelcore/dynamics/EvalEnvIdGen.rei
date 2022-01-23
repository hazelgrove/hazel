/* Generator for EvalEnvId.t */
[@deriving sexp]
type t;

let empty: t;
let next: t => (t, EvalEnvId.t);
