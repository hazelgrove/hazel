/* make this unit a `kind` when ready */
open Sexplib.Std;

[@deriving sexp]
type t = list(Var.t);

let extend = (lst, item) => [item, ...lst];
let empty = [];

let includes = (lst, item) => List.mem(item, lst);
