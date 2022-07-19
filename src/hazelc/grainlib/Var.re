open Sexplib.Std;

[@deriving sexp]
type t = string;

let equal = String.equal;

let length = String.length;
