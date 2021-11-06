open Sexplib.Std;

[@deriving sexp]
type t = list(TypVar.t);
