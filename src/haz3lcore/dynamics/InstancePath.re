open Sexplib.Std;

[@deriving sexp]
type t = list((HoleInstance.t, Var.t));
