open Sexplib.Std;

[@deriving sexp]
type t = int;

let initial: t = 0;

let inc_eval_steps = (stats: t): t => stats + 1;

let eval_steps = (stats: t): int => stats;
