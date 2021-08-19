open Sexplib.Std;

[@deriving sexp]
type t = int

let recent = (var_1: t) (var_2: t) => max var_1 var_2;;