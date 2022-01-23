open Sexplib.Std;

[@deriving sexp]
type t = int;

let empty = 0;

let next = (n: t): (t, EvalEnvId.t) => (n + 1, n);
