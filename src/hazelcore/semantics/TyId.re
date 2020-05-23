open Sexplib.Std;

[@deriving sexp]
type t = string;

let eq = String.equal;
