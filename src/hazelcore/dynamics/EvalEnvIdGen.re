open Sexplib.Std;

[@deriving sexp]
type t = int;

/* `EvalEnvId.empty` is a special value used for the empty
   environment at the beginning of evaluation or when resuming
   evaluation (fill and resume). */
let initial = EvalEnvId.empty + 1;

let next = (n: t): (t, EvalEnvId.t) => (n + 1, n);
