open Util;

[@deriving sexp]
type t = list((HoleInstance.t, Var.t));
