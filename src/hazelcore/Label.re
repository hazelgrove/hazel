open Sexplib.Std;
[@deriving sexp]
type t = string;

let length: t => int = label => String.length(label);

let sub: (t, int, int) => t = (label, s, e) => String.sub(label, s, e);

let insert: t => char => int =  // ECD: you are here, need to figure out a way to insert a character to a label