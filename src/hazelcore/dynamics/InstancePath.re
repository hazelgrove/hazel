open Sexplib.Std;

[@deriving (sexp, show)]
type t = list((HoleInstance.t, Var.t));
