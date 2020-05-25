open Sexplib.Std;

[@deriving sexp]
type t = string;

let eq = String.equal;

let length = String.length;

let valid_regex = Re.Str.regexp("^[_a-z][_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);
