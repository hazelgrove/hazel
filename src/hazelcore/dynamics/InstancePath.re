open Sexplib.Std;

[@deriving sexp]
type t = list((NodeInstance.t, Var.t));
