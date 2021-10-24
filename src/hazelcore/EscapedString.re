open Sexplib.Std;

[@deriving sexp]
type t = string;

let to_string = s => s;

let equal = String.equal;
